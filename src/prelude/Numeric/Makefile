include Makefile.inc

THISLIB	= Numeric
SEARCH	= -P../Char -P../PreludeText -P../PreludeList -P../Ratio

SRCS	= \
        Nonnull.hs     ReadDec.hs     LexDigits.hs \
        ReadFloat.hs   ReadHex.hs     ReadInt.hs \
        ReadOct.hs     ReadSigned.hs  ShowFloat.hs \
        ShowInt.hs     ShowSigned.hs  ShowIntBase.hs
SRCS_T = ${SRCS}
	 

# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
../PreludeList/%.${HISUFFIX} : ../PreludeList/%.hs
	cd ../PreludeList; ${MAKE} $@
../Prelude/%.${HISUFFIX} : ../Prelude/%.hs
	cd ../Prelude; ${MAKE} $@


# Here are the dependencies.

${OBJDIR}/ReadDec.$O: ReadDec.hs ${OBJDIR}/ReadInt.$O
${OBJDIR}/ReadHex.$O: ReadHex.hs ${OBJDIR}/ReadInt.$O
${OBJDIR}/ReadOct.$O: ReadOct.hs ${OBJDIR}/ReadInt.$O
${OBJDIR}/ReadFloat.$O : ReadFloat.hs ${OBJDIR}/ReadDec.$O
${OBJDIR}/ReadLitChar.$O : ReadLitChar.hs ${OBJDIR}/AsciiTab.$O ${OBJDIR}/ReadDec.$O ${OBJDIR}/ReadOct.$O ${OBJDIR}/ReadHex.$O
${OBJDIR}/ShowLitChar.$O : ShowLitChar.hs ${OBJDIR}/AsciiTab.$O

