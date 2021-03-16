HEADIR = ${HEADAS}/lib

# Linux
XORGDIR = /usr/lib64/xorg/modules
#GCCDIR = /usr/lib/gcc/x86_64-redhat-linux/4.8.5
GCC = /usr/bin/gcc
LIB64 = /usr/lib64
#GFORTRAN = /usr/bin/gfortran

VER_FTOOLS = 6.28
VER_PGPLOT = 5.2.2 # specify, after modifying the relevant words (4 places)
VER_APE    = 2.9 # specify, after modifying the relevant words (2 places)
GINGA_TOOLS_DIR = /home/parallels/install/ginga_tools/v1.1/src/ # specify
SRCDIR = ../src

LNXLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -lpng # -Wno-comment -Wno-long-long
#LNXLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_6.25 -lcfitsio -lxanlib_6.25 -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -Wno-comment -Wno-long-long -lpng
#LNXOPT = LD="gfortran -g" CC="gcc -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"
LNXOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"

# Mac
XORGDIR =/opt/X11/lib
MACLIB = -I${HEADAS}/include -I${HEADAS}/include/ape -L${XORGDIR} -L${HEADIR} -lftools_${VER_FTOOLS} -lcfitsio -lxanlib_${VER_FTOOLS} -lcpgplot5.2.2 -lpgplot5.2.2 -lape_2.9 -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -Wno-comment -Wno-long-long 
MACOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${MACLIB}" ALL="${ALL}"

#INSTALL_DIR = ../bin

#ALL = asmtest f90test1
ALL = unittest.o asmtest f90test1

#ALLS = make_sf_list.pl

start:
	$(MAKE) `uname`

# The following seems to be required due to -I../src in Default
unittest.o: unittest.f90 ${SRCDIR}/fort_util.o
	${FC} ${FFLAGS} -I../src -c -o $@ $<

f90test1: f90test1.o
	${LD} -o f90test1 f90test1.o\
	 ${LINKLIB}

GINGA_FRFREAD_OBJ = ../../ginga_tools/v1.1/src/frfread.o ../../ginga_tools/v1.1/src/getoa2.o \
        ../../ginga_tools/v1.1/src/cnven.o ../../ginga_tools/v1.1/src/mjd.o ../../ginga_tools/v1.1/src/srcarray.o \
        ../../ginga_tools/v1.1/src/satpn3.o ../../ginga_tools/v1.1/src/orbit.o ../../ginga_tools/v1.1/src/recpol_new.o
#GINGA_FRFREAD_OBJ = ${GINGA_TOOLS_DIR}/frfread.o ${GINGA_TOOLS_DIR}/getoa2.o \
#        ${GINGA_TOOLS_DIR}/cnven.o ${GINGA_TOOLS_DIR}/mjd.o ${GINGA_TOOLS_DIR}/srcarray.o \
#        ${GINGA_TOOLS_DIR}/satpn3.o ${GINGA_TOOLS_DIR}/orbit.o ${GINGA_TOOLS_DIR}/recpol_new.o

ASMTEST_OBJ = unittest.o ${SRCDIR}/fort_util.o ${SRCDIR}/err_exit.o \
        ${SRCDIR}/asm_fits_common.o ${SRCDIR}/asm_aux.o ${SRCDIR}/asm_read_telemetry.o ${SRCDIR}/asm_fitsout.o 

## Required to remake ./*.mod (I think)
asmtest.o: asmtest.f90 ${ASMTEST_OBJ}
	${FC} ${FFLAGS} -I../src -c -o $@ asmtest.f90

asmtest: asmtest.o ${ASMTEST_OBJ} ${GINGA_FRFREAD_OBJ}
	${LD} -I../src -o asmtest asmtest.o ${ASMTEST_OBJ} ${GINGA_FRFREAD_OBJ} ${LINKLIB}

%.o : %.F parameters.F
	${FC} ${FFLAGS} -c -o $@ $<

#.F.o:
#	${FC} ${FFLAGS} -c -o $@ $<
#
#.f.o:
#	${FC} ${FFLAGS} -c -o $@ $<

%.o: %.f90
	${FC} ${FFLAGS} -I../src -c -o $@ $<
# -I.. is required to tell the compiler *.mod file exist in ../ 

.c.o:
	${CC} ${CFLAGS} -c -o $@ $<

all: ${ALL}

.PHONY: test clean Linux Darwin

Linux:
	${MAKE} ${LNXOPT} all

Darwin:
	${MAKE} ${MACOPT} all

#install:
#	if [ ! -d ${INSTALL_DIR} ]; then mkdir -p ${INSTALL_DIR}; fi
#	mv -f ${ALL} ${INSTALL_DIR}/
#	cp -f ${ALLS} ${INSTALL_DIR}/

test::
	GINGA_CHATTER=4 ./runtest.sh

clean::
	-${RM} -f *.o ${ALL}

