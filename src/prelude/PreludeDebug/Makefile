include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= PreludeDebug
SEARCH	= -I../PreludeIO -I../Prelude -I../PackedString

SRCS	= 
SRCS_T	=

ifeq "${BUILDWITH}" "hbc"
HEAP = +RTS -H64M -A8M -RTS
endif
ifeq "${BUILDWITH}" "nhc98"
HEAP = +RTS -H32M -RTS
endif


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
extra: ${OBJDIR}/${THISLIB}.$O
${OBJDIR}/${THISLIB}.$O: ${THISLIB}.hs ${OBJDIR}/../Prelude/DEither.$O ../PackedString/DPackedString.${HISUFFIX}
	cp ../Prelude/DEither.T.hi ./DEither.hi
	${HC} ${HEAP} ${SEARCH} -part -prelude -redefine -c -o $@ $<
	cp PreludeDebug.hi PreludeDebug.T.hi

${OBJDIR}/../PreludeIO/DIO.$O:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIO.$O
${OBJDIR}/../Prelude/DEither.$O:
	cd ../Prelude; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/Prelude/DEither.$O

../PackedString/DPackedString.${HISUFFIX}:
	cd ../PackedString; make $@
