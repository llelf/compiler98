include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/Ratio

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= Reduce.hs RatioCon.hs Denominator.hs Numerator.hs \
	  Prec.hs Eq_Ratio.hs Num_Ratio.hs Show_Ratio.hs \
	DRatio.hs TRational.hs PrelRatio.hs

NOTDONE= ???

MAINSRC=Ratio.hs

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../Prelude/ -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE)  -P../Prelude/ -o $@ ${MAINSRC}

install:

install_ifc:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f ${OBJS} ${MAINOBJ}

