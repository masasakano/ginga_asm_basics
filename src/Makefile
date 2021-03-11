HEADIR = ${HEADAS}/lib

# Linux
XORGDIR = /usr/lib64/xorg/modules
#GCCDIR = /usr/lib/gcc/x86_64-redhat-linux/4.8.5
GCC = /usr/bin/gcc
LIB64 = /usr/lib64
#GFORTRAN = /usr/bin/gfortran

VER_FTOOLS = 6.28
VER_PGPLOT = 5.2.2 # specify, after modifying the relevant words (4 places)
VER_APE    = 2.9   # specify, after modifying the relevant words (2 places)
#GINGA_TOOLS_DIR = ../../ginga_tools/v1.1/src # specify
GINGA_TOOLS_DIR = /home/parallels/install/ginga_tools/v1.1/src/ # specify
GINGA_TDIR = /home/parallels/install/ginga_tools/v1.1/src/ # specify

LNXLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -lpng #-Wno-comment -Wno-long-long ## (only for C/C++ but not for f90)
#LNXLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_6.25 -lcfitsio -lxanlib_6.25 -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -Wno-comment -Wno-long-long -lpng
#LNXOPT = LD="gfortran -g" CC="gcc -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"
LNXOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"

# Mac
XORGDIR =/opt/X11/lib
MACLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -Wno-comment -Wno-long-long 
MACOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${MACLIB}" ALL="${ALL}"

INSTALL_DIR = ../bin

ALL = asmdump asmmkevt asmtelemetryout

# ALLS = make_sf_list.pl

start:
	$(MAKE) `uname`

err_exit.o: err_exit.f90
	${FC} ${FFLAGS} -c -o $@ $<

fort_util.o: fort_util.f90 err_exit.o
	${FC} ${FFLAGS} -c -o $@  fort_util.f90

asm_fits_common.o: asm_fits_common.f90 fort_util.o err_exit.o
	${FC} ${FFLAGS} -c -o $@ asm_fits_common.f90

asm_aux.o asm_aux.mod: asm_aux.f90 fort_util.o err_exit.o asm_fits_common.o
	${FC} ${FFLAGS} -c -o asm_aux.o asm_aux.f90

asm_read_telemetry.o asm_read_telemetry.mod: asm_read_telemetry.f90 fort_util.o err_exit.o asm_fits_common.o asm_aux.o
	${FC} ${FFLAGS} -c -o asm_read_telemetry.o asm_read_telemetry.f90

asm_fitsout.o asm_fitsout.mod: asm_fitsout.f90 fort_util.o err_exit.o asm_fits_common.o asm_aux.o
	${FC} ${FFLAGS} -c -o asm_fitsout.o asm_fitsout.f90

#GINGA_FRFREAD_OBJ = ../../ginga_tools/v1.1/src/frfread.o ${GINGA_TOOLS_DIR}/getoa2.o \

GINGA_FRFREAD_OBJ = ../../ginga_tools/v1.1/src/frfread.o ../../ginga_tools/v1.1/src/getoa2.o \
        ../../ginga_tools/v1.1/src/cnven.o ../../ginga_tools/v1.1/src/mjd.o ../../ginga_tools/v1.1/src/srcarray.o \
        ../../ginga_tools/v1.1/src/satpn3.o ../../ginga_tools/v1.1/src/orbit.o ../../ginga_tools/v1.1/src/recpol_new.o

ASMDUMP_OBJ = err_exit.o fort_util.o \
        asm_fits_common.o asm_aux.o asm_read_telemetry.o asm_fitsout.o 
#        frfread.o getoa2.o cnven.o mjd.o srcarray.o\
#        satpn3.o orbit.o recpol_new.o

asmtelemetryout.o: ${ASMDUMP_OBJ} asmtelemetryout.f90
	${FC} ${FFLAGS} -c -o $@ asmtelemetryout.f90 ${LINKLIB}

asmmkevt.o: ${ASMDUMP_OBJ} asmmkevt.f90
	${FC} ${FFLAGS} -c -o $@ asmmkevt.f90 ${LINKLIB}

asmdump.o: ${ASMDUMP_OBJ} asmdump.f90
	${FC} ${FFLAGS} -c -o $@ asmdump.f90 ${LINKLIB}

asmtelemetryout: ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} asmtelemetryout.o
	${LD} -o asmtelemetryout asmtelemetryout.o ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} ${LINKLIB}

asmmkevt: ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} asmmkevt.o
	${LD} -o asmmkevt asmmkevt.o ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} ${LINKLIB}

asmdump: ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} asmdump.o
	${LD} -o asmdump asmdump.o ${GINGA_FRFREAD_OBJ} ${ASMDUMP_OBJ} ${LINKLIB}

#asmdump:                 asmdump.o err_exit.o asm_fits_common.o asm_aux.o asm_read_telemetry.o asm_fitsout.o \
#	frfread.o getoa2.o cnven.o mjd.o srcarray.o\
#       	satpn3.o orbit.o recpol_new.o
#	${LD} -o asmdump asmdump.o err_exit.o asm_fits_common.o asm_aux.o asm_fitsout.o \
#		frfread.o getoa2.o cnven.o mjd.o srcarray.o\
#	 satpn3.o orbit.o recpol_new.o\
#	 ${LINKLIB}

#frfplot: frfplot.o srcarray.o mjd.o lacplot.o lacplot2.o insarray.o\
#	 chkfnm.o plotsub.o plots1.o plots2.o frfread.o cnven.o\
#         lactrn2.o sacyz.o\
#	 henkan.o timchk.o pgplotint.o pltitlfr.o ssl2.o
#	${LD} -o frfplot frfplot.o srcarray.o mjd.o lacplot.o\
#	 lacplot2.o insarray.o\
#	 chkfnm.o plotsub.o plots1.o plots2.o frfread.o cnven.o\
#         lactrn2.o sacyz.o\
#	 henkan.o timchk.o pgplotint.o pltitlfr.o ssl2.o\
#	 ${LINKLIB}

%.o : %.F parameters.F
	${FC} ${FFLAGS} -c -o $@ $<

#.F.o:
#	${FC} ${FFLAGS} -c -o $@ $<
#
#.f.o:
#	${FC} ${FFLAGS} -c -o $@ $<

#%.o %.mod: %.f90
#%.o: %.f90
.f90.o:
	${FC} ${FFLAGS} -c -o $@ $<

.c.o:
	${CC} ${CFLAGS} -c -o $@ $<

all: ${ALL}


.PHONY: install test clean Linux Darwin

Linux:
	${MAKE} ${LNXOPT} all

Darwin:
	${MAKE} ${MACOPT} all

install:
	if [ ! -d ${INSTALL_DIR} ]; then mkdir -p ${INSTALL_DIR}; fi
	mv -f ${ALL} ${INSTALL_DIR}
#	cp -f ${ALLS} ${INSTALL_DIR}/

test::
	[ -x ../test ] && (cd ../test && GINGA_CHATTER=4 ./runtest.sh)
#[ -x test ] && (cd test; make) && (./test/f90test1 && ./test/asmtest)

clean::
	-${RM} -f *.o *.mod ${ALL}

