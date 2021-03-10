%readme

To make,

1. Set up the HEASOFT environment, including the required environmental variables.
2. Adjust `src/Makefile` (directories, versions of the libraries, etc)
3. `cd src; make`
4. Executable `asmmkevt` is created in the src/ directory.

To run tests,

1. Set up the HEASOFT environment, including the required environmental variables.
2. Adjust `test/Makefile` (directories, versions of the libraries, etc)
3. `cd test; make test` or alternatively, `cd src; make test`  
   Whichever, the standard `make` is run first before the test is executed.  
   See `test/Makefile` if you want to know what it is doing (quite simple, really).

To run, for example, in the src/ directory

    GINGA_CHATTER=4 ./asmmkevt ../../../ginga_samples/ginga_sirius_P198804280220.fits ../../../ginga_samples/FR880428.S0220.fits ../../../ginga_samples/mkevt_out_test.fits

Usage is

    asmmkevt Teletry.fits FRF.fits output_asm_event_file.fits
