ifeq "" "${MKDIR}"
MKDIR:=$(shell pwd)
else
MKDIR:=$(patsubst %/$(notdir ${MKDIR}),%, ${MKDIR})
endif
include ${MKDIR}/Makefile.inc

NHC98=${NHC98BASEDIR}/script/nhc98

COMPILE=${NHC98} $(PROF) -part -prelude -redefine -dbgprelude -trusted -c -P${NHC98BASEDIR}/include/tracer/
DBG_COMPILE=${NHC98} $(PROF) -T -part -prelude -redefine -c -dbgtrans -trusted -P${NHC98BASEDIR}/include/tracer/
PLAIN_COMPILE=${NHC98} -c $(PROF) -part -prelude -redefine -dbgprelude
PRELUDELIB=${OBJ}/libprelude${CFG}.a

TRACEPRELUDEDIR=${TRACEDIR}/prelude
TRACEPRELUDEOBJ=${TRACEPRELUDEDIR}/${OBJ}


