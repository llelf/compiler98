ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc
