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

# Get a double array of data for the specified row
def flist_data(fname, row):
    ret = subprocess.run(['flist', fname, 'STDOUT', '-', str(row), 'more=yes', 'prhead=no'], timeout=None,
                         stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                         encoding='utf-8', shell=False)
    lines = ret.stdout.split("\n")
    lines = lines[2:]

    return [i.split() for i in lines] # => [['Telemetry[17], '=', '250'], [...], ...]

# Get an array of data for the specified row from the QDP output
# assuming the first 2 rows are comment lines
def qdp_data(fname, row):
    with open(fname, 'r') as f:  # b for Binary mode
        iline = 0
        for line in f:
            if (re.search(r'^ *(@|!)', line)): continue
            iline += 1
            if (iline == row):
                if os.environ.get('GINGA_DEBUG') is not None: print('DEBUG:qdp_data:sub',line.rstrip()) # DEBUG
                return line.split()


class GingaAsmBasicsTestCase(unittest.TestCase):
    
    def setUp(self):
        self.outdir = 'test_output'
        self.smpldir = '../samples'
        self.f_tel = self.smpldir + '/' + 'ginga_sirius_P198804280220.fits.gz'
        self.f_frf = self.smpldir + '/' + 'FR880428.S0220.fits.gz'
        self.com_main = '../src/asmmkevt'
        self.com_sub  = '../src/asmtelemetryout'
        self.com_qdp  = '../src/asm2qdp'
        self.com_qdpb = '../src/asm2qdpindy'
        os.environ['GINGA_CHATTER'] = '4'
        if (not os.path.exists(self.outdir)):
            os.mkdir(self.outdir)

    def tearDown(self):
        # Clean up the temporary output directory
        if (os.path.exists(self.outdir)):
            if os.environ.get('GINGA_DEBUG') is not None:
                print('NOTE: Temporary directory remains b/c GINGA_DEBUG is set: '+self.outdir)
                return

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

    def test_qdp(self):
        # Creates a QDP file (and PCO file)
        outdir = self.outdir

        # Creates a FITS file
        fout = self.outdir + '/' + 'outmain3.fits'
        ret = subprocess.run([self.com_main, self.f_tel, self.f_frf, fout], timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0, 'Main run: stderr='+ret.stderr)

        # Specifies 2 channels
        foutroot2 = outdir + '/' + 'outqdp_4_13'
        arcom_4_13 = [self.com_qdp, fout, foutroot2, "4", "13"]
        ret = subprocess.run(arcom_4_13, timeout=None,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             #stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        if os.environ.get('GINGA_DEBUG') is not None: print(ret.stdout)  # DEBUG
        if os.environ.get('GINGA_DEBUG') is not None: print(ret.stderr)  # DEBUG
        self.assertEqual(ret.returncode, 0, 'QDP run: stderr='+ret.stderr)

        foutroot2b = outdir + '/' + 'outqdp_4_13b'
        arcom_4_13b = [self.com_qdpb, fout, foutroot2b, "4", "13"]
        retb = subprocess.run(arcom_4_13b, timeout=None,
                             stdout=subprocess.PIPE, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             #stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        if os.environ.get('GINGA_DEBUG') is not None: print(retb.stdout)  # DEBUG
        if os.environ.get('GINGA_DEBUG') is not None: print(retb.stderr)  # DEBUG
        self.assertEqual(retb.returncode, 0, 'QDP-b run: stderr='+retb.stderr)

        # Summed channels (Default)
        foutroot8 = outdir + '/' + 'outqdp_def'
        arcom_def = [self.com_qdp, fout, foutroot8]
        ret = subprocess.run(arcom_def, timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(ret.returncode, 0, 'QDP run: stderr='+ret.stderr)

        foutroot8b = outdir + '/' + 'outqdp_defb'  # asm2qdpindy
        arcom_defb = [self.com_qdpb, fout, foutroot8b]
        retb = subprocess.run(arcom_defb, timeout=None,
                             stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, #stderr=subprocess.DEVNULL,
                             encoding='utf-8', shell=False)
        self.assertEqual(retb.returncode, 0, f'QDP run-b: com={arcom_defb} stderr='+retb.stderr)

        # Get QDP and FITS values
        row = 110 # for CH13
        arqdprow2 = qdp_data(foutroot2+'.qdp', row)
        if os.environ.get('GINGA_DEBUG') is not None: print('DEBUG-py3:', arqdprow2 )  # DEBUG
        y21ch13_qdp     = arqdprow2[4] # 2nd detector, 2nd band (CH13 (from CH00))
        y21ch13_qdp_all = arqdprow2    # 2nd detector

        arqdprow2b = qdp_data(foutroot2b+'.qdp', row)  # asm2qdpindy
        if os.environ.get('GINGA_DEBUG') is not None: print('DEBUG-py3b:', arqdprow2b )  # DEBUG
        y21ch13_qdpb    = arqdprow2b[4] # 2nd detector, 2nd band (CH13 (from CH00))
        y21ch13_qdp_allb= arqdprow2b    # 2nd detector

        row = 87 # for CH04
        arqdprow2 = qdp_data(foutroot2+'.qdp', row)
        #if os.environ.get('GINGA_DEBUG') is not None: print('DEBUG-py4:', arqdprow2 )  # DEBUG
        y13ch04_qdp     = arqdprow2[9] # 5th detector, 1st band (CH04 (from CH00))
        y13ch04_qdp_all = arqdprow2    # 5th detector

        row = 110 # for CH08-15
        arqdprow8 = qdp_data(foutroot8+'.qdp', row)
        y21ch08_15_qdp     = arqdprow8[4] # 2nd detector, 2nd band (CH08-15)
        y21ch08_15_qdp_all = arqdprow8    # 2nd detector

        arqdprow8b= qdp_data(foutroot8b+'.qdp', row)
        y21ch08_15_qdpb    = arqdprow8b[4] # 2nd detector, 2nd band (CH08-15)
        y21ch08_15_qdp_allb= arqdprow8b    # 2nd detector

        row = 87 # for CH00-07
        arqdprow8 = qdp_data(foutroot8+'.qdp', row)
        y13ch00_07_qdp     = arqdprow8[9] # 5th detector, 1st band (CH00-07)
        y13ch00_07_qdp_all = arqdprow8    # 5th detector

        row = 110 # for CH08-15
        y21ch13_fits = "-99"
        y21ch08_15_fits = 0
        lines = flist_data(fout, row)
        for ea in lines:
            #print(ea)
            if (len(ea) < 2): continue
            mat = re.search(r'^ *Y21CH(\d\d)', ea[0])
            if (mat):
                if (mat[1] == '13'): y21ch13_fits = ea[2]
                imat = int(mat[1])
                if (8 <= imat): y21ch08_15_fits += int(ea[2])
                if (mat[1] == '15'): break

        row = 87 # for CH00-07
        y13ch04_fits = "-99"
        y13ch00_07_fits = 0
        lines = flist_data(fout, row)
        for ea in lines:
            #print(ea)
            if (len(ea) < 2): continue
            mat = re.search(r'^ *Y13CH(\d\d)', ea[0])
            if (mat):
                if (mat[1] == '04'): y13ch04_fits = ea[2]
                imat = int(mat[1])
                if (imat <= 7): y13ch00_07_fits += int(ea[2])
                if (mat[1] == '15'): break

        # if os.environ.get('GINGA_DEBUG') is not None: print(f'DEBUG: FITS-sum(08-15)={y21ch08_15_fits}\n')  # => 7 # DEBUG
        self.assertEqual(y21ch13_fits, '3')
        self.assertEqual(y21ch08_15_fits, 7)
        self.assertEqual(y13ch04_fits, '2')   # ROW=87:           CH= 0       4     7
        self.assertEqual(y13ch00_07_fits, 4)  # 4.727904017105326E+04 0 0 1 1 2 0 0 0
        self.assertEqual(y21ch13_qdpb,y21ch13_fits, 'QDPbY21CH13==3: Command: '+' '.join(arcom_4_13b)+"\n> "+' '.join(y21ch13_qdp_allb))
        self.assertEqual(int(y21ch08_15_qdpb),y21ch08_15_fits, 'QDPbY21CH08_15: Command: '+' '.join(arcom_defb))
        self.assertEqual(y21ch13_qdp, y21ch13_fits, 'QDP Y21CH13==3: Command: '+' '.join(arcom_4_13)+"\n> "+' '.join(y21ch13_qdp_all))
        self.assertEqual(int(y21ch08_15_qdp), y21ch08_15_fits, 'QDP Y21CH08_15: Command: '+' '.join(arcom_def))
        self.assertEqual(y13ch04_qdp, y13ch04_fits, 'QDP Y13CH04==2: Command: '+' '.join(arcom_4_13)+"\n> "+' '.join(y13ch04_qdp_all))
        self.assertEqual(int(y13ch00_07_qdp), y13ch00_07_fits, 'QDP Y13CH00_07: Command: '+' '.join(arcom_def))

