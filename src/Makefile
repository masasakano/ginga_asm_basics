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

LNXLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -lpng #-Wno-comment -Wno-long-long ## (only for C/C++ but not for f90)
LNXOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"

# Mac
XORGDIR =/opt/X11/lib
MACLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -Wno-comment -Wno-long-long 
MACOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${MACLIB}" ALL="${ALL}"

INSTALL_DIR = ../bin

ALL = asmmkevt asmtelemetryout asm2qdp mjd2date date2mjd

start:
	$(MAKE) `uname`

err_exit.o: err_exit.f90
	${FC} ${FFLAGS} -c -o $@ $<

fort_util.o: fort_util.f90 err_exit.o
	${FC} ${FFLAGS} -c -o $@  fort_util.f90

asm_consts.o: asm_consts.f90
	${FC} ${FFLAGS} -c -o $@ asm_consts.f90

asm_fits_common.o: asm_fits_common.f90 fort_util.o err_exit.o asm_consts.o
	${FC} ${FFLAGS} -c -o $@ asm_fits_common.f90

asm_aux.o asm_aux.mod: asm_aux.f90 fort_util.o err_exit.o asm_consts.o asm_fits_common.o
	${FC} ${FFLAGS} -c -o asm_aux.o asm_aux.f90

asm_read_telemetry.o asm_read_telemetry.mod: asm_read_telemetry.f90 fort_util.o err_exit.o asm_consts.o asm_fits_common.o asm_aux.o
	${FC} ${FFLAGS} -c -o asm_read_telemetry.o asm_read_telemetry.f90

asm_fitsout.o asm_fitsout.mod: asm_fitsout.f90 fort_util.o err_exit.o asm_consts.o asm_fits_common.o asm_aux.o
	${FC} ${FFLAGS} -c -o asm_fitsout.o asm_fitsout.f90

asm_read_evt.o asm_read_evt.mod: asm_read_evt.f90 fort_util.o err_exit.o asm_consts.o asm_fits_common.o #asm_aux.o
	${FC} ${FFLAGS} -c -o asm_read_evt.o asm_read_evt.f90

#GINGA_FRFREAD_OBJ = ../../ginga_tools/v1.1/src/frfread.o ${GINGA_TOOLS_DIR}/getoa2.o

GINGA_FRFREAD_OBJ = ../../ginga_tools/v1.1/src/frfread.o ../../ginga_tools/v1.1/src/getoa2.o \
        ../../ginga_tools/v1.1/src/cnven.o ../../ginga_tools/v1.1/src/mjd.o ../../ginga_tools/v1.1/src/srcarray.o \
        ../../ginga_tools/v1.1/src/satpn3.o ../../ginga_tools/v1.1/src/orbit.o ../../ginga_tools/v1.1/src/recpol_new.o

ASMMAIN_OBJ = err_exit.o fort_util.o \
        asm_consts.o asm_fits_common.o asm_aux.o asm_read_telemetry.o asm_fitsout.o 
#        frfread.o getoa2.o cnven.o mjd.o srcarray.o\
#        satpn3.o orbit.o recpol_new.o

ASM2QDP_OBJ = err_exit.o fort_util.o \
        asm_consts.o asm_fits_common.o asm_read_evt.o

mjd2date.o: ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o mjd2date.f90
	${FC} ${FFLAGS} -c -o $@ mjd2date.f90 ${LINKLIB}

date2mjd.o: ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o date2mjd.f90
	${FC} ${FFLAGS} -c -o $@ date2mjd.f90 ${LINKLIB}

asm2qdp.o: ${ASM2QDP_OBJ} asm2qdp.f90
	${FC} ${FFLAGS} -c -o $@ asm2qdp.f90 ${LINKLIB}

asmtelemetryout.o: ${ASMMAIN_OBJ} asmtelemetryout.f90
	${FC} ${FFLAGS} -c -o $@ asmtelemetryout.f90 ${LINKLIB}

asmmkevt.o: ${ASMMAIN_OBJ} asmmkevt.f90
	${FC} ${FFLAGS} -c -o $@ asmmkevt.f90 ${LINKLIB}

mjd2date: ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o mjd2date.o
	${LD} -o mjd2date mjd2date.o ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o ${LINKLIB}

date2mjd: ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o date2mjd.o
	${LD} -o date2mjd date2mjd.o ../../ginga_tools/v1.1/src/mjd.o fort_util.o err_exit.o asm_consts.o asm_fits_common.o ${LINKLIB}

asm2qdp: ${ASM2QDP_OBJ} asm2qdp.o
	${LD} -o asm2qdp asm2qdp.o ${ASM2QDP_OBJ} ${LINKLIB}

asmtelemetryout: ${GINGA_FRFREAD_OBJ} ${ASMMAIN_OBJ} asmtelemetryout.o
	${LD} -o asmtelemetryout asmtelemetryout.o ${GINGA_FRFREAD_OBJ} ${ASMMAIN_OBJ} ${LINKLIB}

asmmkevt: ${GINGA_FRFREAD_OBJ} ${ASMMAIN_OBJ} asmmkevt.o
	${LD} -o asmmkevt asmmkevt.o ${GINGA_FRFREAD_OBJ} ${ASMMAIN_OBJ} ${LINKLIB}

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

test::
	[ -x ../test ] && (cd ../test && GINGA_CHATTER=4 ./runtest.sh)
#[ -x test ] && (cd test; make) && (./test/f90test1 && ./test/asmtest)

clean::
	-${RM} -f *.o *.mod ${ALL}

