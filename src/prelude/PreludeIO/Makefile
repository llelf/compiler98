include Makefile.inc

THISLIB	= PreludeIO
SEARCH	= -P$(LOW) -P../$(MACHINE)
EXTRA_H_FLAGS	= -prelude
EXTRA_C_FLAGS	=

SRCS	= \
	StrError.hs \
	Eq_IOError.hs    Monad_IO.hs      ReadLn.hs 	\
	AppendFile.hs    Print.hs         Show_IOError.hs \
	Catch.hs         Functor_IO.hs    PutStrLn.hs      TFilePath.hs    \
	DIO.hs           GetLine.hs       ReadFile.hs      UserError.hs    \
	DIOError.hs      Interact.hs      ReadIO.hs        WriteFile.hs    \
	PutChar.hs PutStr.hs GetChar.hs GetContents.hs     IoError.hs	   \
	_MkIO.hs
SRCS_T	=\
	DIO.hs          DIOError.hs    GetChar.hs      GetContents.hs \
	Interact.hs     Monad_IO.hs    PutChar.hs      PutStr.hs      \
	PutStrLn.hs     IoError.hs     UserError.hs    TFilePath.hs   \
	WriteFile.hs    Catch.hs       GetLine.hs      Print.hs       \
	Show_IOError.hs Eq_IOError.hs  Functor_IO.hs   _MkIO.hs       \
	ReadIO.hs       ReadLn.hs      StrError.hs  \
	AppendFile.hs   ReadFile.hs


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.



# Here are the dependencies.
${OBJDIR}/Catch.$O:        ${OBJDIR}/DIO.$O
${OBJDIR}/Eq_IOError.$O:   ${OBJDIR}/DIOError.$O
${OBJDIR}/IoError.$O:      ${OBJDIR}/DIO.$O
${OBJDIR}/Monad_IO.$O:     ${OBJDIR}/DIO.$O ${OBJDIR}/DIOError.$O # ../Prelude/CMonad.hi
${OBJDIR}/Functor_IO.$O:   # ../Prelude/CFunctor.hi
${OBJDIR}/Show_IOError.$O: ${OBJDIR}/DIOError.$O ${OBJDIR}/StrError.$O
ifneq "T" "${TRACING}"
${OBJDIR}/Show_IOError.$O: ${OBJDIR}/StrError.$O
endif
${OBJDIR}/UserError.$O:    ${OBJDIR}/DIOError.$O
${OBJDIR}/DIOError.$O:     #${OBJDIR}/DErrNo.$O
${OBJDIR}/ReadLn.$O:       ${OBJDIR}/ReadIO.$O
${OBJDIR}/_MkIO.$O:        ${OBJDIR}/DIO.$O


# And now for C-files dependencies.
Catch.c:        DIO.c
Eq_IOError.c:   DIOError.c #DErrNo.c
IoError.c:      DIO.c
Monad_IO.c:     DIO.c DIOError.c # ../Prelude/CMonad.hi
Functor_IO.c:   # ../Prelude/CFunctor.hi
Show_IOError.c: DIOError.c StrError.c #DErrNo.c 
UserError.c:    DIOError.c
DIOError.c:	#DErrNo.c
ReadLn.c:	ReadIO.c
_MkIO.c:	DIO.c

