include Makefile.inc

THISLIB	= Char
SEARCH	= -P../Numeric -P../PreludeList
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	=

SRCS	= \
        AsciiTab.hs Chr.hs Ord.hs IsDigit.hs DigitToInt.hs IntToDigit.hs \
	IsAlpha.hs IsAlphaNum.hs IsAscii.hs IsControl.hs \
        IsHexDigit.hs IsLower.hs IsOctDigit.hs \
        IsLatin1.hs IsPrint.hs IsSpace.hs IsUpper.hs \
        LexLitChar.hs ReadLitChar.hs ShowLitChar.hs \
        ToLower.hs ToUpper.hs

SRCS_T	= \
	AsciiTab.hs Chr.hs Ord.hs IsDigit.hs DigitToInt.hs IntToDigit.hs \
	IsLatin1.hs IsSpace.hs IsUpper.hs \
	IsAscii.hs IsControl.hs \
	IsHexDigit.hs IsLower.hs IsOctDigit.hs IsPrint.hs \
	IsAlpha.hs IsAlphaNum.hs \
	ShowLitChar.hs \
	ToLower.hs ToUpper.hs \
	LexLitChar.hs ReadLitChar.hs

# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

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

