include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/LowB

VPATH = .

.SUFFIXES: .hi .hs .o .po

MAINSRCS= LowIO.hs LowSystem.hs LowPS.hs
MAINIFC= $(patsubst %.hs,%.hi,${MAINSRCS})
MAINOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRCS})
TRACEIFC=NonStdTrace.hi

IOPRELSRCS= PrimHGetChar.hs PrimHGetContents.hs PrimHPutChar.hs PrimHClose.hs

SYSPRELSRCS= PrimExitWith.hs
PSPRELSRCS= PrimPackString.hs
NTPRELSRCS = _Driver.hs PrimIntFromInteger.hs 
TRACEPRELSRCS = NonStdTrace.hs

NOTDONE= PrimExitWith.hs

IOPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${IOPRELSRCS})
SYSPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SYSPRELSRCS})
PSPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${PSPRELSRCS})
NTPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${NTPRELSRCS})
TRACEPRELOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${TRACEPRELSRCS})

$(IOPRELOBJS) ${SYSPRELOBJS} ${PSPRELOBJS} ${TRACEPRELOBJS}: ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -P../PreludeIO/ -P../System/ -o $@ $<

$(NTPRELOBJS): ${OBJDIR}/%.o: %.hs
	$(COMPILE) -P../PreludeIO/ -P../System/ -P../ -o $@ $<

all: ${NTPRELOBJS} $(IOPRELOBJS) ${SYSPRELOBJS} ${PSPRELOBJS} ${TRACEPRELOBJS} ${MAINOBJS} 

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

install:

install_ifc:
	cp ${TRACEIFC} ${IFCDEST}

${OBJDIR}/LowPS.o: LowPS.hs ${IOPRELOBJS}
	$(DBG_COMPILE) -o $@ LowPS.hs

${OBJDIR}/LowIO.o: LowIO.hs ${IOPRELOBJS}
	$(DBG_COMPILE) -o $@ LowIO.hs

${OBJDIR}/LowSystem.o: LowSystem.hs ${SYSPRELOBJS}
	$(DBG_COMPILE) -o $@ LowSystem.hs

clean:
	rm -f ${NTPRELOBJS} $(IOPRELOBJS) ${SYSPRELOBJS} ${PSPRELOBJS} ${TRACEPRELOBJS} ${MAINOBJS} 
