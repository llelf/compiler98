include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/IO

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= HGetChar.hs HGetContents.hs HPutChar.hs HPutStr.hs HClose.hs
NOTDONE= ???

MAINSRC=IO.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})


$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../PreludeIO/ -P../LowB/ -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

install:

install_ifc:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f ${MAINOBJ} ${OBJS}
