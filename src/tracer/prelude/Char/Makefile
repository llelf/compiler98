include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/Char

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= IsLower.hs IsUpper.hs IsAlpha.hs IsDigit.hs IsAlphanum.hs IsControl.hs IsPrint.hs IsSpace.hs ToLower.hs ToUpper.hs IsAscii.hs IsHexDigit.hs IsOctDigit.hs

NOTDONE= ???

MAINSRC= Char.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC= $(patsubst %.hs,%.hi,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

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
	rm -f ${OBJS} ${MAINOBJ}
