include Makefile.inc

THISLIB = IOExtras

HSSRCS	= FixIO.hs PerformGC.hs UnsafePerformIO.hs UnsafePtrEq.hs \
	  DIORef.hs NewIORef.hs ReadIORef.hs WriteIORef.hs \
	  DIOArray.hs BoundsIOArray.hs FreezeIOArray.hs NewIOArray.hs \
	  ReadIOArray.hs WriteIOArray.hs
GCSRCS	=


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

PARTFLAGS = -P../PreludeIO/ -part
LIBFLAGS  = -lib


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
	rm -f ${OBJDIR}/*.o *.hi *.o *_.hs *.c *.s


${HSOBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c  -P$(LOW) -P../PreludeIO/ -P../Array -part -o $@ $<
${GCOBJS}: ${OBJDIR}/%.o : %.gc
	$(HC) -c  -P../Time/ -P../PreludeIO/ -part -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c +CTS -lib -CTS -o $@ $<

${HSCFILES}: %.c : %.hs
	$(HC) -C -P$(LOW) -P../PreludeIO/ -P../Array -part $<
${GCCFILES}: %.c : %.gc
	$(HC) -C -P../Time/ -P../PreludeIO/ -part $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C -P../NonStd/ +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(

## Yucky .hi stuff to deal with mutual dependency from FFI lib.
HIFILES = $(patsubst %.hs,../${THISLIB}/%.hi,${HSSRCS})
${HIFILES}: ../${THISLIB}/%.hi : %.hs
	$(HC) -c $(HFLAGS) $(PARTFLAGS) -o /dev/null $<

