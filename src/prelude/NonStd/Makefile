include Makefile.inc

THISLIB = NonStd

SRCS = \
	NonStdUnsafeCoerce.hs \
	NonStdProfile.hs \
	NonStdEval.hs  \
	NonStdTrace.hs \
	Warning.hs
GCSRCS = NonStdGetPID.gc


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
GCOBJS = $(patsubst %.gc,${OBJDIR}/%.o,${GCSRCS})
CFILES = $(patsubst %.hs,%.c,${SRCS})
GCCFILES = $(patsubst %.gc,%.c,${GCSRCS})

HFLAGS = -P./ -P../LowB/


all:  ${OBJS} ${GCOBJS}
boot: ${OBJS}
cfiles: ${CFILES} ${GCCFILES}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${GCCFILES}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s *_.hs


 
#.SUFFIXES: .hs .c .o
#.hs.o:
#	$(HC) -c +CTS -lib -CTS $(HFLAGS) $<
#.hs.c:
#	$(HC) -C +CTS -lib -CTS $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c +CTS -lib -CTS $(HFLAGS) -o $@ $<
${GCOBJS}: ${OBJDIR}/%.o : %.gc
	$(HC) -c +CTS -lib -CTS $(HFLAGS) -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C +CTS -lib -CTS $(HFLAGS) $<
${GCCFILES}: %.c : %.gc
	$(HC) -C +CTS -lib -CTS $(HFLAGS) $<


# I know that they depend on .hi, but make is stupid :-(
