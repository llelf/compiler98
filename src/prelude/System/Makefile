include Makefile.inc

THISLIB = System

SRCS	= \
	Eq_ExitCode.hs    GetArgs.hs        GetProgName.hs    Read_ExitCode.hs  SystemFun.hs         \
	DExitCode.hs      ExitWith.hs       GetEnv.hs         Ord_ExitCode.hs   Show_ExitCode.hs  



OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

LOW = ../LowB/
HFLAGS = -part -P$(LOW)
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
	$(HC) -c $(NHEAP) +CTS -lib -CTS -P$(LOW) -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS -P$(LOW) $<


# I know that they depend on .hi, but make is stupid :-(

${OBJDIR}/Eq_ExitCode.o: Eq_ExitCode.hs ${OBJDIR}/DExitCode.o
${OBJDIR}/Ord_ExitCode.o: Ord_ExitCode.hs ${OBJDIR}/DExitCode.o ${OBJDIR}/Eq_ExitCode.o
${OBJDIR}/Read_ExitCode.o: Read_ExitCode.hs ${OBJDIR}/DExitCode.o
${OBJDIR}/Show_ExitCode.o: Show_ExitCode.hs ${OBJDIR}/DExitCode.o


# C-files dependencies.
Eq_ExitCode.c:   DExitCode.c
Ord_ExitCode.c:  DExitCode.c Eq_ExitCode.c
Read_ExitCode.c: DExitCode.c
Show_ExitCode.c: DExitCode.c
