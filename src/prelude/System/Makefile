include Makefile.inc

THISLIB	= System
SEARCH	= -P$(LOW) -P../PreludeIO

SRCS	= \
	DExitCode.hs Eq_ExitCode.hs Read_ExitCode.hs \
	Ord_ExitCode.hs  Show_ExitCode.hs  \
	GetArgs.hs GetProgName.hs GetEnv.hs \
	ExitWith.hs ExitFailure.hs SystemFun.hs
SRCS_T	= \
	DExitCode.hs Eq_ExitCode.hs Read_ExitCode.hs \
	Ord_ExitCode.hs  Show_ExitCode.hs  \
	ExitWith.hs ExitFailure.hs
NOTDONE = GetArgs.hs GetProgName.hs GetEnv.hs SystemFun.hs


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/Eq_ExitCode.o: Eq_ExitCode.hs ${OBJDIR}/DExitCode.o
${OBJDIR}/Ord_ExitCode.o: Ord_ExitCode.hs ${OBJDIR}/DExitCode.o ${OBJDIR}/Eq_ExitCode.o
${OBJDIR}/Read_ExitCode.o: Read_ExitCode.hs ${OBJDIR}/DExitCode.o
${OBJDIR}/Show_ExitCode.o: Show_ExitCode.hs ${OBJDIR}/DExitCode.o


# C-files dependencies.
Eq_ExitCode.c:   DExitCode.c
Ord_ExitCode.c:  DExitCode.c Eq_ExitCode.c
Read_ExitCode.c: DExitCode.c
Show_ExitCode.c: DExitCode.c

