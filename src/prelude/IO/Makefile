include Makefile.inc

THISLIB	= IO
SEARCH	= -P../PreludeIO -P$(LOW) -P../$(MACHINE)

COMMON_SRCS = \
	DHandle.hs	  DHandlePosn.hs \
	HFlush.hs         HPrint.hs        HSetPosn.hs \
        IsEOF.hs          SeekMode.hs      BufferMode.hs \
        HGetChar.hs       HPutChar.hs                     \
        IsEOFError.hs     HGetContents.hs \
        HPutStr.hs        IOMode.hs        IsUserError.hs   \
	HClose.hs         HGetPosn.hs      HSeek.hs \
        IoeGetFileName.hs OpenFile.hs      HFileSize.hs \
        HIsEOF.hs                          IoeGetHandle.hs \
                          HGetFileName.hs  HPutStrLn.hs \
	NotDoneYet.hs                                    \
	IoeGetErrorString.hs	HGetLine.hs	\
	Eq_Handle.hs      Eq_HandlePosn.hs \
	Show_Handle.hs    Show_HandlePosn.hs \
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
${OBJDIR}/NotDoneYet.$O:     ${OBJDIR}/DHandle.$O 
${OBJDIR}/IoeGetFileName.$O: ${OBJDIR}/DHandle.$O ${OBJDIR}/HGetFileName.$O ${OBJDIR}/IoeGetHandle.$O
${OBJDIR}/IoeGetHandle.$O:   ${OBJDIR}/DHandle.$O 
${OBJDIR}/Bracket_.$O:       ${OBJDIR}/Try.$O 
${OBJDIR}/Bracket.$O:        ${OBJDIR}/Try.$O 
${OBJDIR}/HPutStrLn.$O:      ${OBJDIR}/DHandle.$O ${OBJDIR}/HPutStr.$O ${OBJDIR}/HPutChar.$O 
${OBJDIR}/HPutStr.$O:        ${OBJDIR}/DHandle.$O ${OBJDIR}/HPutChar.$O 
${OBJDIR}/HPutChar.$O:       ${OBJDIR}/DHandle.$O 
${OBJDIR}/HGetContents.$O:   ${OBJDIR}/DHandle.$O 
${OBJDIR}/HGetChar.$O:       ${OBJDIR}/DHandle.$O 
${OBJDIR}/HGetLine.$O:       ${OBJDIR}/HGetChar.$O 
${OBJDIR}/HSeek.$O:          ${OBJDIR}/SeekMode.$O ${OBJDIR}/DHandle.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/HSetPosn.$O:       ${OBJDIR}/DHandle.$O ${OBJDIR}/DHandlePosn.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/HGetPosn.$O:       ${OBJDIR}/DHandle.$O ${OBJDIR}/DHandlePosn.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/HFlush.$O:         ${OBJDIR}/DHandle.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/HGetBuffering.$O:  ${OBJDIR}/DHandle.$O ${OBJDIR}/BufferMode.$O 
${OBJDIR}/HSetBuffering.$O:  ${OBJDIR}/DHandle.$O ${OBJDIR}/BufferMode.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/IsEOF.$O:          ${OBJDIR}/HIsEOF.$O  ${OBJDIR}/StdIO.$O
${OBJDIR}/HIsEOF.$O:         ${OBJDIR}/DHandle.$O 
${OBJDIR}/HGetFileName.$O:   ${OBJDIR}/DHandle.$O 
${OBJDIR}/HFileSize.$O:      ${OBJDIR}/DHandle.$O ${OBJDIR}/HGetFileName.$O 
${OBJDIR}/OpenFile.$O:       ${OBJDIR}/DHandle.$O ${OBJDIR}/IOMode.$O ${OBJDIR}/HClose.$O 
${OBJDIR}/HClose.$O:         ${OBJDIR}/DHandle.$O 
${OBJDIR}/Eq_Handle.$O:      ${OBJDIR}/DHandle.$O 
${OBJDIR}/Eq_HandlePosn.$O:  ${OBJDIR}/DHandlePosn.$O ${OBJDIR}/Eq_Handle.$O

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

