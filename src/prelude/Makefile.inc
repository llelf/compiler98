ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc

LOW = ../LowB/

ifeq "p" "${PROFILING}"
HC := $(LOCAL)nhc98 -p $(NHEAP)
else
HC := $(LOCAL)nhc98
endif

ifeq "t" "${TPROF}"
HC += -t
endif
