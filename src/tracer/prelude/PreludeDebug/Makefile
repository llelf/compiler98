include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/PreludeDebug

VPATH = .

.SUFFIXES: .hi .hs .o .po

SRCS= 

NOTDONE= ???

MAINSRC= PreludeDebug.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${MAINSRC}
	${NHC98} ${PROF} -part -prelude -redefine -c -o $@ ${MAINSRC}
	#${DBG_COMPILE} -o $@ ${MAINSRC}

install:

clean:
	rm -f ${OBJS} ${MAINOBJ}
