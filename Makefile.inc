### Configurable variables:

OPT = -O2 -fomit-frame-pointer

CC = gcc
INSTALL = cp
AR = ar
LD = ld

### End of configurable variables

ifeq "" "${MKDIR}"
#MKDIR:=$(PWD)
MKDIR:=$(shell pwd)
endif

LOCAL = $(MKDIR)/script/
export LOCAL

ifeq "" "${BUILDBASEDIR}"
BUILDBASEDIR:=/tmp
endif

MACHINE := $(shell $(LOCAL)harch)
export MACHINE
HC = $(LOCAL)nhc98

OBJ=obj${CFG}
NHC98BASEDIR=${MKDIR}
SCRIPTDIR=${NHC98BASEDIR}/script
BUILDDIR=${BUILDBASEDIR}/${MACHINE}
INCDIR=${NHC98BASEDIR}/include
SRCDIR=${NHC98BASEDIR}/src
TRACEDIR=${SRCDIR}/tracer
RUNTIMEDIR=${SRCDIR}/runtime
PRELUDEDIR=${SRCDIR}/prelude
LIBDIR=${NHC98BASEDIR}/lib
DST=${LIBDIR}/${MACHINE}

### This is a very important step: get the local configuration

include ${DST}/config

###

BUILDCOMP := $(shell echo $(BUILDWITH) | cut -c1-3 )

DCFG=
TRACING=$(findstring T,${CFG})
PROFILING=$(findstring p,${CFG})
TPROF=$(findstring t,${CFG})

ifeq "T" "${TRACING}"
DCFG:=${DCFG} -DTRACE -DDBGTRANS
endif

ifeq "p" "${PROFILING}"
DCFG:=${DCFG} -DPROFILE=2
endif

ifeq "t" "${TPROF}"
DCFG:=${DCFG} -DTPROF
endif

ifeq "1" "${USINGRTS}"
NHEAP = +RTS -H8M -RTS
endif
