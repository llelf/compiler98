include Makefile.inc

THISLIB = NonStd

SRCS = \
	NonStdUnsafeCoerce.hs \
	NonStdProfile.hs \
	NonStdEval.hs  \
	NonStdTrace.hs \
	Warning.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
CFILES = $(patsubst %.hs,%.c,${SRCS})

HFLAGS = -P./ -P../LowB/


all:  ${OBJS}
boot: ${OBJS}
cfiles: ${CFILES}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s


 
.SUFFIXES: .hs .c .o
.hs.o:
	$(HC) -c +CTS -lib -CTS $(HFLAGS) $<
.hs.c:
	$(HC) -C +CTS -lib -CTS $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c +CTS -lib -CTS $(HFLAGS) -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C +CTS -lib -CTS $(HFLAGS) $<


# I know that they depend on .hi, but make is stupid :-(
