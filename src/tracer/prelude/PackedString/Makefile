include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/PackedString

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= DPackedString.hs PackString.hs
NOTDONE= ???

MAINSRC=PackedString.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../LowB/ -o $@ $<

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
