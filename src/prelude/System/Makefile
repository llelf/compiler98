include Makefile.inc

THISLIB	= System
SEARCH	= -P$(LOW) -P../PreludeIO

SRCS	= \
	DExitCode.hs Eq_ExitCode.hs Read_ExitCode.hs \
	Ord_ExitCode.hs  Show_ExitCode.hs  \
	GetArgs.hs GetProgName.hs GetEnv.hs \
	ExitWith.hs ExitFailure.hs SystemFun.hs
SRCS_T	= $(SRCS)


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/Eq_ExitCode.$O: Eq_ExitCode.hs ${OBJDIR}/DExitCode.$O
${OBJDIR}/Ord_ExitCode.$O: Ord_ExitCode.hs ${OBJDIR}/DExitCode.$O ${OBJDIR}/Eq_ExitCode.$O
${OBJDIR}/Read_ExitCode.$O: Read_ExitCode.hs ${OBJDIR}/DExitCode.$O
${OBJDIR}/Show_ExitCode.$O: Show_ExitCode.hs ${OBJDIR}/DExitCode.$O


# C-files dependencies.
Eq_ExitCode.c:   DExitCode.c
Ord_ExitCode.c:  DExitCode.c Eq_ExitCode.c
Read_ExitCode.c: DExitCode.c
Show_ExitCode.c: DExitCode.c

