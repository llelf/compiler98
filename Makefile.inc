### Configurable variables:

OPT = -O3

# CC = actually sourced from LIBDIR/MACHINE/config
INSTALL = cp
AR = ar
LD = ld


VERSION = 1.12
# When incrementing the version number, don't forget to change the
# corresponding version in the configure script!
#   (odd minor number = CVS version;  even minor number = release version)

HVERSION = 3.02
# HVERSION is the separate version number for hmake.
#   (odd/even minor number is irrelevant - all are release versions)


### End of configurable variables

ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
#MKDIR:=$(PWD)
endif

TOPDIR= $(MKDIR)
LOCAL = $(TOPDIR)/script/
export LOCAL

MACHINE := $(shell $(LOCAL)harch)
export MACHINE

OBJ=obj${CFG}
SCRIPTDIR=${LOCAL}
INCDIR=${TOPDIR}/include
SRCDIR=${TOPDIR}/src
LIBDIR=${TOPDIR}/lib
DST=${LIBDIR}/${MACHINE}

### This is a very important step: get the local configuration
include ${DST}/config
###

ifeq "" "${BUILDBASEDIR}"
BUILDBASEDIR:=/tmp
endif
BUILDDIR=${BUILDBASEDIR}/${MACHINE}

HC = $(LOCAL)nhc98
export HC
HMAKE = $(LOCAL)hmake
export HMAKE

BUILDCOMP := $(shell echo $(BUILDWITH) | cut -c1-3 )
CPP = $(CC) -E -x c

DCFG:=
TRACING=$(findstring T,${CFG})
PROFILING=$(findstring p,${CFG})
TPROF=$(findstring z,${CFG})

ifeq "T" "${TRACING}"
DCFG:=${DCFG} -DTRACE=1 -DDBGTRANS
endif

ifeq "p" "${PROFILING}"
DCFG:=${DCFG} -DPROFILE=2
endif

ifeq "z" "${TPROF}"
DCFG:=${DCFG} -DTPROF
endif

ifeq "1" "${USINGRTS}"
NHEAP = +RTS -H8M -RTS
endif


ifeq "T" "${TRACING}"
  HISUFFIX=T.hi
  O=T.o
  C=T.c
else
ifeq "p" "${PROFILING}"
  HISUFFIX=hi
  O=p.o
  C=p.c
else
ifeq "z" "${TPROF}"
  HISUFFIX=hi
  O=z.o
  C=z.c
else
  HISUFFIX=hi
  O=o
  C=hc
endif
endif
endif

ifeq "T" "${TRACING}"
ifeq "z" "${TPROF}"
  HISUFFIX=T.hi
  O=Tz.o
  C=Tz.c
endif
endif
