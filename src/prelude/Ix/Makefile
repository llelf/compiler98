include Makefile.inc

THISLIB	= Ix
SEARCH	=

SRCS	= \
	Ix_Bool.hs Ix_Char.hs Ix_Int.hs Ix_Integer.hs Ix_Ordering.hs \
	Ix_2.hs Ix_3.hs Ix_4.hs RangeSize.hs

SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/Ix_2.o: Ix_2.hs ${OBJDIR}/RangeSize.o
${OBJDIR}/Ix_3.o: Ix_3.hs ${OBJDIR}/RangeSize.o
${OBJDIR}/Ix_4.o: Ix_4.hs ${OBJDIR}/RangeSize.o


# C-files dependencies.
Ix_2.c: RangeSize.c
Ix_3.c: RangeSize.c
Ix_4.c: RangeSize.c

