include Makefile.inc

THISLIB = Directory

HSSRCS	= DoesFileExist.hs DPermissions.hs
GCSRCS	= GetModificationTime.gc DoesDirectoryExist.gc


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs


HSOBJS = $(patsubst %.hs,${OBJDIR}/%.o,${HSSRCS})
GCOBJS = $(patsubst %.gc,${OBJDIR}/%.o,${GCSRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
OBJS = ${HSOBJS} ${GCOBJS}

HSCFILES = $(patsubst %.hs,%.c,${HSSRCS})
GCCFILES = $(patsubst %.gc,%.c,${GCSRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})
CFILES = ${HSCFILES} ${GCCFILES}

HFLAGS = -P../NonStd/ -part
#HEAP = +RTS -H2M -RTS


all:  ${OBJS} ${MAINOBJ}
boot: ${OBJS} ${MAINOBJ}
cfiles: ${CFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) *.c
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s


 
.SUFFIXES: .hs .gc .o .c
#.hs.o:
#	$(HC) -c $(HFLAGS) $<

${HSOBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c  -P$(LOW) -P../PreludeIO/ -part -o $@ $<
${GCOBJS}: ${OBJDIR}/%.o : %.gc
	$(HC) -c  -P../Time/ -P../PreludeIO/ -part -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c -P../NonStd/ +CTS -lib -CTS -o $@ $<

${HSCFILES}: %.c : %.hs
	$(HC) -C -P$(LOW) -P../PreludeIO/ -part $<
${GCCFILES}: %.c : %.gc
	$(HC) -C -P../Time/ -P../PreludeIO/ -part $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C -P../NonStd/ +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(

