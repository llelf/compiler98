include Makefile.inc

THISLIB = Char

SRCS	= \
        AsciiTab.hs Chr.hs Ord.hs IsDigit.hs DigitToInt.hs IntToDigit.hs \
	IsAlpha.hs IsAlphaNum.hs IsAscii.hs IsControl.hs \
        IsHexDigit.hs IsLower.hs IsOctDigit.hs \
        IsLatin1.hs IsPrint.hs IsSpace.hs IsUpper.hs \
        LexLitChar.hs ReadLitChar.hs ShowLitChar.hs \
        ToLower.hs ToUpper.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -P../Numeric/ -P../PreludeList/ -part
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

${OBJDIR}/IsAlpha.o : IsAlpha.hs ${OBJDIR}/IsUpper.o ${OBJDIR}/IsLower.o
${OBJDIR}/IsAlphaNum.o : IsAlphaNum.hs ${OBJDIR}/IsAlpha.o ${OBJDIR}/IsDigit.o
${OBJDIR}/IsPrint.o: IsPrint.hs ${OBJDIR}/IsControl.o
${OBJDIR}/ToLower.o: ToLower.hs ${OBJDIR}/IsUpper.o
${OBJDIR}/ToUpper.o: ToUpper.hs ${OBJDIR}/IsLower.o
${OBJDIR}/LexLitChar.o: LexLitChar.hs ../Numeric/LexDigits.hi
${OBJDIR}/ReadLitChar.o: ReadLitChar.hs ../Numeric/ReadDec.hi ../Numeric/ReadOct.hi ../Numeric/ReadHex.hi
${OBJDIR}/ShowLitChar.o: ShowLitChar.hs

IsAlpha.c : IsAlpha.hs IsUpper.c IsLower.c
IsAlphaNum.c : IsAlphaNum.hs IsAlpha.c IsDigit.c
IsPrint.c: IsPrint.hs IsControl.c
ToLower.c: ToLower.hs IsUpper.c
ToUpper.c: ToUpper.hs IsLower.c
LexLitChar.c: LexLitChar.hs ../Numeric/LexDigits.hi
ReadLitChar.c: ReadLitChar.hs ../Numeric/ReadDec.hi ../Numeric/ReadOct.hi ../Numeric/ReadHex.hi

## Yucky .hi stuff to deal with mutual dependencies.

NUMHIFILES =  LexDigits Nonnull ReadDec ReadOct ReadHex ReadInt
NUMHIS     = $(patsubst %,../Numeric/%.hi,${NUMHIFILES})

${NUMHIS}: ../Numeric/%.hi : ../Numeric/%.hs
	cd ../Numeric; ${MAKE} $@

../Numeric/LexDigits.hi: ../Numeric/Nonnull.hi
../Numeric/ReadDec.hi: ../Numeric/ReadInt.hi
../Numeric/ReadOct.hi: ../Numeric/ReadInt.hi
../Numeric/ReadHex.hi: ../Numeric/ReadInt.hi
../Numeric/ReadInt.hi: ../Numeric/Nonnull.hi

