include Makefile.inc

THISLIB	= IO
SEARCH	= -P../PreludeIO -P$(LOW) -P../$(MACHINE)

SRCS	= \
	HFlush.hs         HPrint.hs        HSetPosn.hs \
        IsEOF.hs          SeekMode.hs      BufferMode.hs \
        HGetChar.hs       HPutChar.hs      DHandlePosn.hs \
        IsEOFError.hs     Eq_HandlePosn.hs HGetContents.hs \
        HPutStr.hs        IOMode.hs        IsUserError.hs   \
	HClose.hs         HGetPosn.hs      HSeek.hs \
        IoeGetFileName.hs OpenFile.hs      HFileSize.hs \
        HIsEOF.hs         HSetBuffering.hs IoeGetHandle.hs \
        HGetBuffering.hs  HGetFileName.hs  HPutStrLn.hs \
	NotDoneYet.hs     DSocket.hs       OpenSocket.hs \
	IoeGetErrorString.hs	HGetLine.hs	\
	IsIOErrors.hs     Try.hs           Bracket.hs	Bracket_.hs

SRCS_T	= \
	HPrint.hs \
        HGetChar.hs       HPutChar.hs      \
        HGetContents.hs   \
        HPutStr.hs        \
	HClose.hs         \
	HPutStrLn.hs	\
	NotDoneYet.hs	\
	Try.hs           Bracket.hs	Bracket_.hs


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/Eq_HandlePosn.o: Eq_HandlePosn.hs ${OBJDIR}/DHandlePosn.o

# C deps.
Eq_HandlePosn.c: DHandlePosn.c

