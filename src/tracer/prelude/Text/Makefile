include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/Text

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= ShowLitChar.hs ShowSigned.hs ShowFloat.hs ShowInt.hs 

NOTDONE= ReadDec.hs RaadFloat.hs ReadLitChar.hs ReadSigned.hs 
	 
MAINSRC=Text.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})

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
	rm -f ${MAINOBJ} ${OBJS}
