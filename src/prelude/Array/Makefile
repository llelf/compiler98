include Makefile.inc

THISLIB	= Array
SEARCH	= -P../List -P../Ix
EXTRA_H_FLAGS	= -prelude	# to ensure all Ix instances are re-exported
EXTRA_C_FLAGS	=

SRCS	= \
	_E.hs          LowVector.hs \
	AMap.hs        Assocs.hs      Eq_Array.hs    ListArray.hs   \
	Show_Array.hs \
	Accum.hs       Bounds.hs      AIndex.hs      Ord_Array.hs   \
	AccumArray.hs  DArray.hs      Indices.hs     Read_Array.hs  \
	ArrayFun.hs    Elems.hs       IxMap.hs       Replace.hs     \
	ArrayLow.hs

SRCS_T	= $(SRCS)


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/_E.$O: _E.hs
${OBJDIR}/LowVector.$O: LowVector.hs ${OBJDIR}/_E.$O
${OBJDIR}/AMap.$O: AMap.hs ${OBJDIR}/DArray.$O ${OBJDIR}/ArrayFun.$O ${OBJDIR}/AIndex.$O ${OBJDIR}/Bounds.$O
${OBJDIR}/Accum.$O: Accum.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Replace.$O ${OBJDIR}/AIndex.$O
${OBJDIR}/AccumArray.$O: AccumArray.hs ${OBJDIR}/DArray.$O ${OBJDIR}/ArrayFun.$O ${OBJDIR}/Accum.$O
${OBJDIR}/ArrayFun.$O: ArrayFun.hs ${OBJDIR}/DArray.$O
${OBJDIR}/Assocs.$O: Assocs.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Indices.$O ${OBJDIR}/AIndex.$O
${OBJDIR}/Bounds.$O: Bounds.hs ${OBJDIR}/DArray.$O
${OBJDIR}/Elems.$O: Elems.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Indices.$O ${OBJDIR}/AIndex.$O
${OBJDIR}/Eq_Array.$O: Eq_Array.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Bounds.$O ${OBJDIR}/Elems.$O
${OBJDIR}/AIndex.$O: AIndex.hs ${OBJDIR}/DArray.$O
${OBJDIR}/Indices.$O: Indices.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Bounds.$O
${OBJDIR}/IxMap.$O: IxMap.hs ${OBJDIR}/DArray.$O ${OBJDIR}/ArrayFun.$O ${OBJDIR}/AIndex.$O
${OBJDIR}/ListArray.$O: ListArray.hs ${OBJDIR}/DArray.$O ${OBJDIR}/ArrayFun.$O
${OBJDIR}/Ord_Array.$O: Ord_Array.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Assocs.$O ${OBJDIR}/Eq_Array.$O
${OBJDIR}/Read_Array.$O: Read_Array.hs ${OBJDIR}/DArray.$O ${OBJDIR}/ArrayFun.$O
${OBJDIR}/Replace.$O: Replace.hs ${OBJDIR}/DArray.$O ${OBJDIR}/LowVector.$O ${OBJDIR}/_E.$O
${OBJDIR}/Show_Array.$O: Show_Array.hs ${OBJDIR}/DArray.$O ${OBJDIR}/Assocs.$O ${OBJDIR}/Bounds.$O ${OBJDIR}/Elems.$O
${OBJDIR}/../List/Difference.$O:
	cd ../List; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/List/Difference.$O


# C-files dependencies.
LowVector.c:  _E.c
AMap.c:       DArray.c ArrayFun.c AIndex.c Bounds.c
Accum.c:      DArray.c Replace.c  AIndex.c
AccumArray.c: DArray.c ArrayFun.c Accum.c
ArrayFun.c:   DArray.c
Assocs.c:     DArray.c Indices.c  AIndex.c
Bounds.c:     DArray.c
Elems.c:      DArray.c Indices.c  AIndex.c
Eq_Array.c:   DArray.c Assocs.c Elems.c
AIndex.c:     DArray.c
Indices.c:    DArray.c Bounds.c
IxMap.c:      DArray.c ArrayFun.c AIndex.c
ListArray.c:  DArray.c ArrayFun.c
Ord_Array.c:  DArray.c Assocs.c   Eq_Array.c
Read_Array.c: DArray.c ArrayFun.c
Replace.c:    DArray.c LowVector.c _E.c
Show_Array.c: DArray.c Assocs.c   Bounds.c  Elems.c
../List/Difference.c:
	cd ../List; ${MAKE} Difference.c

