include Makefile.inc

THISLIB	= IO
SEARCH	= -P../PreludeIO -P$(LOW) -P../$(MACHINE)

COMMON_SRCS = \
	DHandle.hs	  DHandlePosn.hs \
	HFlush.hs         HPrint.hs        HSetPosn.hs \
        IsEOF.hs          SeekMode.hs      BufferMode.hs \
        HGetChar.hs       HPutChar.hs                     \
        IsEOFError.hs     Eq_HandlePosn.hs HGetContents.hs \
        HPutStr.hs        IOMode.hs        IsUserError.hs   \
	HClose.hs         HGetPosn.hs      HSeek.hs \
        IoeGetFileName.hs OpenFile.hs      HFileSize.hs \
        HIsEOF.hs                          IoeGetHandle.hs \
                          HGetFileName.hs  HPutStrLn.hs \
	NotDoneYet.hs                                    \
	IoeGetErrorString.hs	HGetLine.hs	\
	IsIOErrors.hs     Try.hs           Bracket.hs	Bracket_.hs

SRCS	= ${COMMON_SRCS} \
	HSetBuffering.hs HGetBuffering.hs DSocket.hs OpenSocket.hs \
	StdIO.hs
SRCS_T	= ${COMMON_SRCS}
SRCS_NT = StdIO.hs
NOTDONEYET = \
	HSetBuffering.hs HGetBuffering.hs DSocket.hs OpenSocket.hs


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/NotDoneYet.o:     ${OBJDIR}/DHandle.o 
${OBJDIR}/IoeGetFileName.o: ${OBJDIR}/DHandle.o ${OBJDIR}/HGetFileName.o ${OBJDIR}/IoeGetHandle.o
${OBJDIR}/IoeGetHandle.o:   ${OBJDIR}/DHandle.o 
${OBJDIR}/Bracket_.o:       ${OBJDIR}/Try.o 
${OBJDIR}/Bracket.o:        ${OBJDIR}/Try.o 
${OBJDIR}/HPutStrLn.o:      ${OBJDIR}/DHandle.o ${OBJDIR}/HPutStr.o ${OBJDIR}/HPutChar.o 
${OBJDIR}/HPutStr.o:        ${OBJDIR}/DHandle.o ${OBJDIR}/HPutChar.o 
${OBJDIR}/HPutChar.o:       ${OBJDIR}/DHandle.o 
${OBJDIR}/HGetContents.o:   ${OBJDIR}/DHandle.o 
${OBJDIR}/HGetChar.o:       ${OBJDIR}/DHandle.o 
${OBJDIR}/HGetLine.o:       ${OBJDIR}/HGetChar.o 
${OBJDIR}/HSeek.o:          ${OBJDIR}/SeekMode.o ${OBJDIR}/DHandle.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/HSetPosn.o:       ${OBJDIR}/DHandle.o ${OBJDIR}/DHandlePosn.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/HGetPosn.o:       ${OBJDIR}/DHandle.o ${OBJDIR}/DHandlePosn.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/HFlush.o:         ${OBJDIR}/DHandle.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/HGetBuffering.o:  ${OBJDIR}/DHandle.o ${OBJDIR}/BufferMode.o 
${OBJDIR}/HSetBuffering.o:  ${OBJDIR}/DHandle.o ${OBJDIR}/BufferMode.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/IsEOF.o:          ${OBJDIR}/HIsEOF.o  ${OBJDIR}/StdIO.o
${OBJDIR}/HIsEOF.o:         ${OBJDIR}/DHandle.o 
${OBJDIR}/HGetFileName.o:   ${OBJDIR}/DHandle.o 
${OBJDIR}/HFileSize.o:      ${OBJDIR}/DHandle.o ${OBJDIR}/HGetFileName.o 
${OBJDIR}/OpenFile.o:       ${OBJDIR}/DHandle.o ${OBJDIR}/IOMode.o ${OBJDIR}/HClose.o 
${OBJDIR}/HClose.o:         ${OBJDIR}/DHandle.o 
${OBJDIR}/Eq_HandlePosn.o:  ${OBJDIR}/DHandlePosn.o 

# C deps.
NotDoneYet.c: DHandle.c 
IoeGetFileName.c: DHandle.c HGetFileName.c IoeGetHandle.c 
IoeGetHandle.c: DHandle.c 
Bracket_.c: Try.c 
Bracket.c: Try.c 
HPutStrLn.c: DHandle.c HPutStr.c HPutChar.c 
HPutStr.c: DHandle.c HPutChar.c 
HPutChar.c: DHandle.c 
HGetContents.c: DHandle.c HGetChar.c 
HGetChar.c: DHandle.c 
HGetLine.c: HGetChar.c 
HSeek.c: SeekMode.c DHandle.c HGetFileName.c 
HSetPosn.c: DHandle.c DHandlePosn.c HGetFileName.c 
HGetPosn.c: DHandle.c DHandlePosn.c HGetFileName.c 
HFlush.c: DHandle.c HGetFileName.c 
HGetBuffering.c: DHandle.c BufferMode.c 
HSetBuffering.c: DHandle.c BufferMode.c HGetFileName.c 
IsEOF.c: DHandle.c HIsEOF.c 
HIsEOF.c: DHandle.c 
HGetFileName.c: DHandle.c 
HFileSize.c: DHandle.c HGetFileName.c 
OpenFile.c: DHandle.c IOMode.c HClose.c 
HClose.c: DHandle.c 
Eq_HandlePosn.c: DHandlePosn.c 

