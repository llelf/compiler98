include Makefile.inc

THISLIB = Numeric

SRCS	= \
        Nonnull.hs     ReadDec.hs     LexDigits.hs \
        ReadFloat.hs   ReadHex.hs     ReadInt.hs \
        ReadOct.hs     ReadSigned.hs  ShowFloat.hs \
        ShowInt.hs     ShowSigned.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part -P../Prelude/ -P../Char/ -P../PreludeText/ -P../PreludeList/ -P../Ratio/
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

../PreludeList/%.hi : ../PreludeList/%.hs
	cd ../PreludeList; ${MAKE} $@
../Prelude/%.hi : ../Prelude/%.hs
	cd ../Prelude; ${MAKE} $@


# I know that they depend on .hi, but make is stupid :-(

${OBJDIR}/ReadDec.o: ReadDec.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadHex.o: ReadHex.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadOct.o: ReadOct.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadFloat.o : ReadFloat.hs ${OBJDIR}/ReadDec.o
${OBJDIR}/ReadLitChar.o : ReadLitChar.hs ${OBJDIR}/AsciiTab.o ${OBJDIR}/ReadDec.o ${OBJDIR}/ReadOct.o ${OBJDIR}/ReadHex.o
${OBJDIR}/ShowLitChar.o : ShowLitChar.hs ${OBJDIR}/AsciiTab.o

## Yucky .hi stuff to deal with mutual dependencies from Char lib.

HIFILES = $(patsubst %.hs,../${THISLIB}/%.hi,${SRCS})
${HIFILES}: ../${THISLIB}/%.hi : %.hs
	$(HC) -c $(HFLAGS) -o /dev/null $<
