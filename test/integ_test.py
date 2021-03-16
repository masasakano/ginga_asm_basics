# -*- coding: utf-8 -*-

# Integration tests
# Run as: /usr/bin/env python3 -m unittest integ_test.py

import unittest
import re
import os
import subprocess

# FTGKY from fdump (get key from sdump), converting the type
def ftgky_frdump(sdump, key):
    mat = re.search(r'\n'+re.escape(key)+r'\s*= ([^/]+)', sdump, flags=re.IGNORECASE)
    if (not mat): return None

    if (re.search(r'^\s*\d+\s*$', mat[1])):
        return int(mat[1])

    m2 = re.search(r"^'(.*)'\s*$", mat[1])
    if (m2):
        return m2[1].strip()

    return float(mat[1])

# Get a double array of data for a specific row
def flist_data(fname, row):
    ret = subprocess.run(['flist', fname, 'STDOUT', '-', str(row), 'more=yes', 'prhead=no'], timeout=None,
                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                         encoding='utf-8', shell=False)
    lines = ret.stdout.split("\n")
    lines = lines[2:]

    return [i.split() for i in lines] # => [['Telemetry[17], '=', '250'], [...], ...]

class GingaAsmBasicsTestCase(unittest.TestCase):
    
    def setUp(self):
        self.outdir = 'test_output'
        self.smpldir = '../samples'
        self.f_tel = self.smpldir + '/' + 'ginga_sirius_P198804280220.fits.gz'
        self.f_frf = self.smpldir + '/' + 'FR880428.S0220.fits.gz'
        self.com_main = '../src/asmmkevt'
        self.com_sub  = '../src/asmtelemetryout'
        os.environ['GINGA_CHATTER'] = '4'
        if (not os.path.exists(self.outdir)):
            os.mkdir('test_output')

    def tearDown(self):
        # Clean up the temporary output directory
        if (os.path.exists(self.outdir)):
            for fname in os.listdir(self.outdir):
                os.unlink(self.outdir+'/'+fname)
                os.rmdir(self.outdir)

    def test_main(self):
        # Creates a FITS file
        fout = self.outdir + '/' + 'outmain1.fits'
        ret = subprocess.run([self.com_main, self.f_tel, self.f_frf, fout], timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0, 'Main run: stderr='+ret.stderr)

        # fverify
        ret = subprocess.run(['fverify', fout], timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0)

        # fstruct (check if the columns are created)
        ret = subprocess.run(['fstruct', fout], timeout=None,
                             stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
            # => 1  BINTABLE ASM table       8     290(124) 5056               0    1
        mat = re.search(r'\n +1 +BINTABLE [^\d]+[\d]+ +([\d]+)\(([\d]+)\) +([\d]+)', ret.stdout, flags=re.IGNORECASE)
        self.assertGreater(int(mat[2]), 100)  # more than 100 columns
        self.assertGreater(int(mat[3]), 1000) # more than 1000 rows

        # fdump (check header kwds)
        ret = subprocess.run(['fdump', fout, 'STDOUT', '-', '-', 'more=yes', 'prdata=no'], timeout=None,
                             stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        naxis2 = ftgky_frdump(ret.stdout, 'NAXIS2')     # Number of Frames in this file
        nrowstel = ftgky_frdump(ret.stdout, 'NROWSTEL') # Number of Frames in the original Telemetry
        tstartma = ftgky_frdump(ret.stdout, 'TSTARTMA')
        tendma = ftgky_frdump(ret.stdout, 'TENDMA')
        row4stat = ftgky_frdump(ret.stdout, 'ROW4STAT') # i-th Frame in the original Telemetry => 34689
        frfsfn_s = ftgky_frdump(ret.stdout, 'FRFSFN_S')
        frfsfn_e = ftgky_frdump(ret.stdout, 'FRFSFN_e')
        frfmjd_s = ftgky_frdump(ret.stdout, 'FRFMJD_S')
        frfmjd_e = ftgky_frdump(ret.stdout, 'FRFMJD_e')
        stat_asm = ftgky_frdump(ret.stdout, 'STAT-ASM')
        stat_hv2 = ftgky_frdump(ret.stdout, 'STAT-HV2')

        self.assertGreater(nrowstel, row4stat)
        self.assertLess(frfsfn_s, frfsfn_e)
        self.assertLess(frfmjd_s, frfmjd_e)
        self.assertLessEqual(frfmjd_s, tstartma)
        self.assertLessEqual(tendma, frfmjd_e)
        self.assertEqual(stat_asm, 'ON')
        self.assertEqual(stat_hv2, 'ENA')

        # flist (check ASM data)
        iframe = 16
        lines = flist_data(fout, iframe)
        self.assertEqual(lines[0][0], 'Y11CH00_Y11L00')
        iw = 1
        #self.assertEqual(int(lines[0][2]), iw)  

        orgrow = row4stat - 1 + iframe 
        self.assertEqual(orgrow, 34704)
        tellines = flist_data(self.f_tel, orgrow)
        pos_word = 0+16+4  # Header-16-bytes + Initial-common-4-bytes

        self.assertEqual(tellines[pos_word][0], 'Telemetry[21]')
        self.assertEqual(int(lines[0][2]), int(tellines[pos_word][2]))

        # Position: Base-16 + Offset-11 (12th word in Line starting from 16; See Table 5.5.3, pp.233)
        pos_word = 0+16+16+4+8-1  # Header-16-bytes + Initial-common-4-bytes
        outpos = 58 - 1  # Python index starting from 0
        self.assertEqual(tellines[pos_word][0], 'Telemetry[44]')
        self.assertEqual(2, int(tellines[pos_word][2]))
        self.assertEqual(    lines[outpos][0], 'Y22CH09_Y22H01')
        self.assertEqual(int(lines[outpos][2]), int(tellines[pos_word][2]))
        
    def test_sub(self):
        # Creates a FITS file
        fout = self.outdir + '/' + 'outsub1.fits'
        ret = subprocess.run([self.com_sub, self.f_tel, self.f_frf, fout, 'Tstart', 'Euler', 'SFNTelem'], timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0, 'Sub run: stderr='+ret.stderr)

        # fverify
        ret = subprocess.run(['fverify', fout], timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0)

        # fstruct (check if the columns are created)
        ret = subprocess.run(['fstruct', fout], timeout=None,
                             stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
            # => 1  BINTABLE ASM table       8     290(124) 5056               0    1
        mat = re.search(r'\n +1 +BINTABLE [^\d]+[\d]+ +([\d]+)\(([\d]+)\) +([\d]+)', ret.stdout, flags=re.IGNORECASE)
        self.assertEqual(int(mat[2]), 5)  # 5 columns only
        self.assertGreater(int(mat[3]), 10000) # more than 10000 rows

