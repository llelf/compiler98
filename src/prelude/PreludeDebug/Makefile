include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= PreludeDebug
SEARCH	=

SRCS	= 
SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
extra: ${OBJDIR}/${THISLIB}.o
${OBJDIR}/${THISLIB}.o: ${THISLIB}.hs
	${HC} ${PROF} -part -prelude -redefine -c -o $@ $<
