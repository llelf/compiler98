### Configurable variables:

OPT = -O3

CC = gcc
INSTALL = cp
AR = ar
LD = ld

### End of configurable variables

ifeq "" "${MKDIR}"
#MKDIR:=$(PWD)
MKDIR:=$(shell pwd)
endif

TOPDIR= $(MKDIR)
LOCAL = $(TOPDIR)/script/
export LOCAL

ifeq "" "${BUILDBASEDIR}"
BUILDBASEDIR:=/tmp
endif

MACHINE := $(shell $(LOCAL)harch)
export MACHINE
HC = $(LOCAL)nhc98
export HC

OBJ=obj${CFG}
SCRIPTDIR=${LOCAL}
BUILDDIR=${BUILDBASEDIR}/${MACHINE}
INCDIR=${TOPDIR}/include
SRCDIR=${TOPDIR}/src
#TRACEDIR=${SRCDIR}/tracer
#RUNTIMEDIR=${SRCDIR}/runtime
#PRELUDEDIR=${SRCDIR}/prelude
LIBDIR=${TOPDIR}/lib
DST=${LIBDIR}/${MACHINE}

### This is a very important step: get the local configuration

include ${DST}/config

###

BUILDCOMP := $(shell echo $(BUILDWITH) | cut -c1-3 )

DCFG=
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

