include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/System

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= DExitCode.hs ExitWith.hs
NTPRELSRCS= 
#PrimExitWith.hs

MAINSRC= System.hs


OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
NTPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${NTPRELSRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC= $(patsubst %.hs,%.hi,${MAINSRC})



NOTDONE= ???


$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../LowB/ -P../PreludeIO/ -o $@ $<

$(NTPRELOBJS): ${OBJDIR}/%.o: %.hs
	$(COMPILE) -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${NTPRELOBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

install:

install_ifc:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f {MAINOBJ} ${OBJS} ${NTPRELOBJS}
