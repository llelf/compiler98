include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= PreludeDebug
SEARCH	= -I../PreludeIO -I../Prelude

SRCS	= 
SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
extra: ${OBJDIR}/${THISLIB}.o
${OBJDIR}/${THISLIB}.o: ${THISLIB}.hs ${OBJDIR}/../Prelude/DEither.o
	${HC} ${PROF} ${SEARCH} -part -prelude -redefine -c -o $@ $<

${OBJDIR}/../PreludeIO/DIO.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIO.o
${OBJDIR}/../Prelude/DEither.o:
	cd ../Prelude; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/Prelude/DEither.o

