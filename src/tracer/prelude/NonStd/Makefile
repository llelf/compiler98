include Makefile.inc

VPATH = .

.SUFFIXES: .hi .hs .o .po

NHC98=${NHC98SRC}/script/nhc98

COMPILE=${NHC98} $(PROF) -part -prelude -redefine -dbgprelude -trusted -c -P${NHC98DIR}/lib_b/debug/
DBG_COMPILE=${NHC98} $(PROF) -part -prelude -redefine -c -dbgtrans -trusted -P${NHC98DIR}/lib_b/debug/
PLAIN_COMPILE=${NHC98} -c $(PROF) -part -prelude -redefine -dbgprelude

MAINSRC= 
MAINOBJ=$(patsubst %.hs,obj/%.${OBJSUFFIX},${MAINSRC})

PRELSRCS= 

NOTDONE= NonStdEval.hs NonStdProfile.hs NonStdTrace.hs NonStdUnsafeCoerce.hs

PRELOBJS=$(patsubst %.hs,obj/%.${OBJSUFFIX},${PRELSRCS})

$(PRELOBJS): obj/%.${OBJSUFFIX}: %.hs
	$(DBG_COMPILE) -o $@ $<

all: ${PRELOBJS}

