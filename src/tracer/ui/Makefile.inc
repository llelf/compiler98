ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
ifeq (${MKDIR}/Makefile.inc,$(wildcard ${MKDIR}/Makefile.inc))
include ${MKDIR}/Makefile.inc
endif
