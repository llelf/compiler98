### Configurable variables:

OPT = -O3

# CC = actually sourced from LIBDIR/MACHINE/config
INSTALL = cp
AR = ar
LD = ld


VERSION = 1.15
# When incrementing the version number, don't forget to change the
# corresponding version in the configure script!
#   (odd minor number = CVS version;  even minor number = release version)

HMAKEVERSION = 3.05
# HMAKEVERSION is the separate version number for hmake.
#   (odd/even minor number is irrelevant - all are release versions)

HATVERSION = 2.01
# HATVERSION is the separate version number for hat.
#   (odd minor number = CVS version;  even minor number = release version)


### End of configurable variables

ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
#MKDIR:=$(PWD)
endif

TOPDIR := $(MKDIR)
MACHINE = $(shell $(TOPDIR)/script/harch)
export MACHINE

# CYG = $(findstring CYGWIN, $(MACHINE))
# ifeq "CYGWIN" "${CYG}"
#   TOPDIR := $(shell echo `cygpath -w $(TOPDIR)` | tr '\\' '/' )
# endif


OBJ=obj$(CFG)
INCDIR=$(TOPDIR)/include
SRCDIR=$(TOPDIR)/src
LIBDIR=$(TOPDIR)/lib
DST=$(LIBDIR)/$(MACHINE)

### This is a very important step: get the local configuration
include $(DST)/config
###

ifeq "" "${BUILDBASEDIR}"
BUILDBASEDIR:=/tmp
endif
# ifeq "CYGWIN" "${CYG}"
#   BUILDDIR = $(shell echo `cygpath -w $(BUILDBASEDIR)/$(MACHINE)` | tr '\\' '/')
# else
BUILDDIR = $(BUILDBASEDIR)/$(MACHINE)
# endif

LOCAL = $(TOPDIR)/script/
export LOCAL
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
