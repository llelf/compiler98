ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc

ifeq "sparc-solaris2" "${MACHINE}"
BUGFIX = -D__EXTENSIONS__
else
BUGFIX = 
endif

TRACEOBJDIR=${RUNTIMEDIR}/obj${CFG}
