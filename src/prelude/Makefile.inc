ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc


ifeq "p" "${PROFILING}"
HC := $(LOCAL)nhc98 -cpp -p
else
HC := $(LOCAL)nhc98 -cpp
endif

ifeq "t" "${TPROF}"
HC += -t
endif

ifneq "${TRACING}" "T"

LOW = ../LowB/

else	#### tracing

LOW = ../LowT/
COMPILE      = ${HC} -c -part -redefine \
		-dbgprelude -trusted -P${INCDIR}/ \
		-DTRACING
DBG_COMPILE  = ${HC} -T -c -part -redefine \
		-dbgtrans -trusted -P${INCDIR}/ \
		-DTRACING
PLAIN_COMPILE= ${HC} -c -part -redefine -dbgprelude

X_COMPILE    = ${HC} -dbgprelude -trusted -P${INCDIR} -DTRACING \
		-hi-suffix=T_hi
TR_COMPILE   = ${HC} -T -dbgtrans -trusted -P${INCDIR} -DTRACING
PL_COMPILE   = ${HC} -dbgprelude

endif

