include Makefile.inc

THISLIB	= Numeric
SEARCH	= -P../Char -P../PreludeText -P../PreludeList -P../Ratio

SRCS	= \
        Nonnull.hs     ReadDec.hs     LexDigits.hs \
        ReadFloat.hs   ReadHex.hs     ReadInt.hs \
        ReadOct.hs     ReadSigned.hs  ShowFloat.hs \
        ShowInt.hs     ShowSigned.hs
SRCS_T = ${SRCS}
	 

# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
../PreludeList/%.hi : ../PreludeList/%.hs
	cd ../PreludeList; ${MAKE} $@
../Prelude/%.hi : ../Prelude/%.hs
	cd ../Prelude; ${MAKE} $@


# Here are the dependencies.

${OBJDIR}/ReadDec.o: ReadDec.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadHex.o: ReadHex.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadOct.o: ReadOct.hs ${OBJDIR}/ReadInt.o
${OBJDIR}/ReadFloat.o : ReadFloat.hs ${OBJDIR}/ReadDec.o
${OBJDIR}/ReadLitChar.o : ReadLitChar.hs ${OBJDIR}/AsciiTab.o ${OBJDIR}/ReadDec.o ${OBJDIR}/ReadOct.o ${OBJDIR}/ReadHex.o
${OBJDIR}/ShowLitChar.o : ShowLitChar.hs ${OBJDIR}/AsciiTab.o

