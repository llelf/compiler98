include Makefile.inc

THISLIB	= Ix
SEARCH	=
EXTRA_H_FLAGS = -prelude

SRCS	= \
	Ix_Bool.hs Ix_Char.hs Ix_Int.hs Ix_Integer.hs Ix_Ordering.hs \
	Ix_2.hs Ix_3.hs Ix_4.hs RangeSize.hs

SRCS_T	= ${SRCS}


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/Ix_2.$O: Ix_2.hs ${OBJDIR}/RangeSize.$O
${OBJDIR}/Ix_3.$O: Ix_3.hs ${OBJDIR}/RangeSize.$O
${OBJDIR}/Ix_4.$O: Ix_4.hs ${OBJDIR}/RangeSize.$O


# C-files dependencies.
Ix_2.c: RangeSize.c
Ix_3.c: RangeSize.c
Ix_4.c: RangeSize.c

