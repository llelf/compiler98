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
	                 GetLine.hs       ReadFile.hs      UserError.hs    \
	DIOError.hs      Interact.hs      ReadIO.hs        WriteFile.hs    \
	PutChar.hs PutStr.hs GetChar.hs GetContents.hs     IoError.hs
SRCS_T	=\
	DIO.hs          DIOError.hs    GetChar.hs      GetContents.hs \
	                Monad_IO.hs    PutChar.hs      PutStr.hs      \
	PutStrLn.hs     IoError.hs     UserError.hs    TFilePath.hs   \
	WriteFile.hs    Catch.hs       GetLine.hs      Print.hs       \
	Show_IOError.hs Eq_IOError.hs  Functor_IO.hs   _MkIO.hs       \
	ReadIO.hs       ReadLn.hs      StrError.hs  \
	AppendFile.hs   ReadFile.hs
SUSPECT = Interact.hs

SUSPECT_O = $(patsubst %.hs, ${OBJDIR}/%.$O, ${SUSPECT})



# Here are the main rules.
include ../Makefile.common



# Here are some extra rules.

ifeq "T" "${TRACING}"
extra: ${SUSPECT_O}
${SUSPECT_O}: ${OBJDIR}/%.$O : %.hs
	$(HC) -c -T -dbgtrans -notrusted -DTRACING -P$(INCDIR) $(PART_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
endif


# Here are the dependencies.
${OBJDIR}/Eq_IOError.$O:   ${OBJDIR}/DIOError.$O
${OBJDIR}/Monad_IO.$O:     ${OBJDIR}/DIOError.$O # ../Prelude/CMonad.hi
${OBJDIR}/Functor_IO.$O:   # ../Prelude/CFunctor.hi
${OBJDIR}/Show_IOError.$O: ${OBJDIR}/DIOError.$O ${OBJDIR}/StrError.$O
ifneq "T" "${TRACING}"
${OBJDIR}/Show_IOError.$O: ${OBJDIR}/StrError.$O
endif
${OBJDIR}/UserError.$O:    ${OBJDIR}/DIOError.$O
${OBJDIR}/DIOError.$O:     #${OBJDIR}/DErrNo.$O
${OBJDIR}/ReadLn.$O:       ${OBJDIR}/ReadIO.$O


# And now for C-files dependencies.
Eq_IOError.$C:   DIOError.$C #DErrNo.$C
Monad_IO.$C:     DIOError.$C # ../Prelude/CMonad.hi
Functor_IO.$C:   # ../Prelude/CFunctor.hi
Show_IOError.$C: DIOError.$C StrError.$C #DErrNo.$C 
UserError.$C:    DIOError.$C
DIOError.$C:	#DErrNo.$C
ReadLn.$C:	ReadIO.$C

