%readme

To make,

1. Set up the HEASOFT environment, including the required environmental variables.
2. Adjust `src/Makefile` (directories, versions of the libraries, etc)
3. `cd src; make`
4. Executable `asmmkevt` is created in the src/ directory.

To run, for example, in the src/ directory

    GINGA_CHATTER=4 ./asmmkevt ../../ginga_samples/ginga_sirius_P198804280220.fits \
      ../../ginga_samples/FR880428.S0220.fits ../../ginga_samples/mkevt_out_test.fits

Usage is

    asmmkevt Teletry.fits FRF.fits output_asm_event_file.fits
    
## Sub-program ##

### asmtelemetryout ###

`asmtelemetryout` outputs for the period of data where ASM-Mode is ON. Run `asmtelemetryout` instead to output selected columns for the entire period:

    GINGA_CHATTER=4 asmtelemetryout ../../ginga_samples/ginga_sirius_P198804280220.fits \
      ../../ginga_samples/FR880428.S0220.fits ../../ginga_samples/asmtelemetry_try06.fits \
      Tstart SFNum SFNTelem Fr6bits FrameNum Mode_ASM Mode_PHA ModeSlew Status_C Status_S DP_C DP_S bitrate

You can view the options with

    asmtelemetryout -h

### asm2qdp ###

`asm2qdp` outputs a QDP and PCO files.

    USAGE: asm2qdp ASM.fits OUT_ROOT [CH_A CH_B]

For the command-line 3rd and 4th arguments `CH_n`, the number n is 0-15 (as in the PHA mode; for the Time-mode, interpret the 'H'-band as 08-15).

You must specify either a pair or none.  If you specify a pair, the data for the specified two channels are output.
If you specify none, the sum of CH00-07 and 08-15 are output.

Then, `OUT_ROOT.qdp` and `OUT_ROOT.pco` are created (if they exist, they are overwritten).
The format of the QDP file is:

    MJD Y1FW1CH_A Y1FW1CH_B Y2FW1CH_A Y2FW1CH_B Y1FW2CH_A Y1FW2CH_B ...

At the top (2nd line), the comment describes the column.

You can view the options with

    asm2qdp -h

## Tests ##

To run tests,

1. Set up the HEASOFT environment, including the required environmental variables.
2. Adjust `test/Makefile` (directories, versions of the libraries, etc)
3. `cd test; make test` or alternatively, `cd src; make test`  
   Whichever, the standard `make` is run first before the test is executed.  
   See `test/Makefile` if you want to know what it is doing (quite simple, really).

`Makefile` has some problems, and sometimes alterations in some library files may not be reflected immediately in testing (and an error is raised). If that happens, `touch` the test source file and rerun `make test`.

## Troubleshooting ##

* If `make` does not work well due to some linking problems, try `make clean` first.
* `make test` does not test file existence. Make sure the sample files exist in the `samples` directory.

## Miscellaneous ##

You may set the environmental variable `GINGA_DEBUG` for testing.  Some diagnostic messages are printed when you run the programs.

## Source files ##

* `asm_aux.f90`: Auxiliary functions/subroutines specific in this package (NOT Fortran-generic utility).
* `asm_fits_common.f90`: Define FITS-related types and constant variables and common basic routines.
* `asm_fitsout.f90`: Anything to do with outputting FITS files and with merging processing.
* `asm_read_evt.f90`: Read a generated ASM event FITS and output it as a pair of QDP/PCO.
* `asm_read_telemetry.f90`: Anything to do with reading FITS files.
* `asmdump.f90`: Obsolete and unused.
* `asmmkevt.f90`: Main program to deal with the command-line arguments and run the process.
* `err_exit.f90`: Error handling routines.
* `fort_util.f90`: General Fortran90 utility routines.
* `test/*.{f90,sh,py}` : For unit and integration testings.

## Further read ##

* [Detailed description](doc/DESCRIPTION.ja.html) in Japanese: <doc/DESCRIPTION.ja.html>
  * Its document source file: <doc/DESCRIPTION.ja.md>
  * Run `make doc` at the top directory to generate/update it; it assumes `pandoc` exists in your command-search path.

