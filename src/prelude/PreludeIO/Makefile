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
	ReadIO.hs       ReadLn.hs      StrError.hs
SRCS_NT =\
	StdIO.hs
NOTDONE= AppendFile.hs ReadFile.hs


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
#NTOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${NTSRCS})
#
#ifeq "T" "${TRACING}"
#extra: ${NTOBJS}
#$(NTOBJS): ${OBJDIR}/%.o: %.hs
#	$(X_COMPILE) -c $(PART_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
#endif



# Here are the dependencies.
${OBJDIR}/Catch.o:        ${OBJDIR}/DIO.o
${OBJDIR}/Eq_IOError.o:   ${OBJDIR}/DIOError.o
${OBJDIR}/IoError.o:      ${OBJDIR}/DIO.o
${OBJDIR}/Monad_IO.o:     ${OBJDIR}/DIO.o ${OBJDIR}/DIOError.o # ../Prelude/CMonad.hi
${OBJDIR}/Functor_IO.o:   # ../Prelude/CFunctor.hi
${OBJDIR}/Show_IOError.o: ${OBJDIR}/DIOError.o ${OBJDIR}/StrError.o
ifneq "T" "${TRACING}"
${OBJDIR}/Show_IOError.o: ${OBJDIR}/StrError.o
endif
${OBJDIR}/UserError.o:    ${OBJDIR}/DIOError.o
${OBJDIR}/DIOError.o:     #${OBJDIR}/DErrNo.o
${OBJDIR}/ReadLn.o:       ${OBJDIR}/ReadIO.o
${OBJDIR}/_MkIO.o:        ${OBJDIR}/DIO.o


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

