include Makefile.inc

THISLIB = Ix

SRCS	= Ix_Bool.hs Ix_Char.hs Ix_Int.hs Ix_Integer.hs Ix_Ordering.hs \
          Ix_2.hs Ix_3.hs Ix_4.hs RangeSize.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part
#HEAP = +RTS -H2M -RTS


all:  ${OBJS} ${MAINOBJ}
boot: ${OBJS} ${MAINOBJ}
cfiles: ${CFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${MAINCFILE}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s

 

.SUFFIXES: .hs .o .c
.hs.o:
	$(HC) -c $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(
${OBJDIR}/Ix_2.o: Ix_2.hs ${OBJDIR}/RangeSize.o
${OBJDIR}/Ix_3.o: Ix_3.hs ${OBJDIR}/RangeSize.o
${OBJDIR}/Ix_4.o: Ix_4.hs ${OBJDIR}/RangeSize.o


# C-files dependencies.
Ix_2.c: RangeSize.c
Ix_3.c: RangeSize.c
Ix_4.c: RangeSize.c
