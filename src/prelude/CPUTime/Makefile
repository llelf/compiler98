include Makefile.inc

THISLIB = CPUTime

SRCS	= CpuTimePrecision.hs
GCSRCS	= GetCPUTime.gc


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
GCOBJS = $(patsubst %.gc,${OBJDIR}/%.o,${GCSRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
GCCFILES = $(patsubst %.gc,%.c,${GCSRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part
#HEAP = +RTS -H2M -RTS


all:  ${OBJS} ${GCOBJS} ${MAINOBJ}
boot: ${OBJS} ${GCOBJS} ${MAINOBJ}
cfiles: ${CFILES} ${GCCFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${GCCFILES} ${MAINCFILE}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR} ${GCOBJS}
${MAINOBJ}: ${OBJS} ${GCOBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s
 


.SUFFIXES: .hs .o .c
.hs.o:
	$(HC) -c $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<
${GCOBJS}: ${OBJDIR}/%.o : %.gc
	$(HC) -c $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${GCCFILES}: %.c : %.gc
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(

