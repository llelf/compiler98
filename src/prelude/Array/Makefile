include Makefile.inc

THISLIB	= Array
SEARCH	= -P../List -P../Ix
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	=

SRCS	= \
	AMap.hs        Assocs.hs      Eq_Array.hs    ListArray.hs   \
	Show_Array.hs \
	Accum.hs       Bounds.hs      Index.hs       Ord_Array.hs   \
	AccumArray.hs  DArray.hs      Indices.hs     Read_Array.hs  \
	ArrayFun.hs    Elems.hs       IxMap.hs       Replace.hs     \
	ArrayLow.hs

SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/AMap.o: AMap.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o ${OBJDIR}/Index.o ${OBJDIR}/Bounds.o
${OBJDIR}/Accum.o: Accum.hs ${OBJDIR}/DArray.o ${OBJDIR}/Replace.o ${OBJDIR}/Index.o
${OBJDIR}/AccumArray.o: AccumArray.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o ${OBJDIR}/Accum.o
${OBJDIR}/ArrayFun.o: ArrayFun.hs ${OBJDIR}/DArray.o
${OBJDIR}/Assocs.o: Assocs.hs ${OBJDIR}/DArray.o ${OBJDIR}/Indices.o ${OBJDIR}/Index.o
${OBJDIR}/Bounds.o: Bounds.hs ${OBJDIR}/DArray.o
${OBJDIR}/Elems.o: Elems.hs ${OBJDIR}/DArray.o ${OBJDIR}/Indices.o ${OBJDIR}/Index.o
${OBJDIR}/Eq_Array.o: Eq_Array.hs ${OBJDIR}/DArray.o ${OBJDIR}/Assocs.o
${OBJDIR}/Index.o: Index.hs ${OBJDIR}/DArray.o
${OBJDIR}/Indices.o: Indices.hs ${OBJDIR}/DArray.o ${OBJDIR}/Bounds.o
${OBJDIR}/IxMap.o: IxMap.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o ${OBJDIR}/Index.o
${OBJDIR}/ListArray.o: ListArray.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o
${OBJDIR}/Ord_Array.o: Ord_Array.hs ${OBJDIR}/DArray.o ${OBJDIR}/Assocs.o ${OBJDIR}/Eq_Array.o
${OBJDIR}/Read_Array.o: Read_Array.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o
${OBJDIR}/Replace.o: Replace.hs ${OBJDIR}/DArray.o ${OBJDIR}/ArrayFun.o ${OBJDIR}/Indices.o ${OBJDIR}/Index.o ${OBJDIR}/Bounds.o ${OBJDIR}/../List/Difference.o
${OBJDIR}/Show_Array.o: Show_Array.hs ${OBJDIR}/DArray.o ${OBJDIR}/Assocs.o ${OBJDIR}/Bounds.o ${OBJDIR}/Elems.o
${OBJDIR}/../List/Difference.o:
	cd ../List; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/List/Difference.o


# C-files dependencies.
AMap.c:       DArray.c ArrayFun.c Index.c Bounds.c
Accum.c:      DArray.c Replace.c  Index.c
AccumArray.c: DArray.c ArrayFun.c Accum.c
ArrayFun.c:   DArray.c
Assocs.c:     DArray.c Indices.c  Index.c
Bounds.c:     DArray.c
Elems.c:      DArray.c Indices.c  Index.c
Eq_Array.c:   DArray.c Assocs.c
Index.c:      DArray.c
Indices.c:    DArray.c Bounds.c
IxMap.c:      DArray.c ArrayFun.c Index.c
ListArray.c:  DArray.c ArrayFun.c
Ord_Array.c:  DArray.c Assocs.c   Eq_Array.c
Read_Array.c: DArray.c ArrayFun.c
Replace.c:    DArray.c ArrayFun.c Indices.c Index.c Bounds.c ../List/Difference.c
Show_Array.c: DArray.c Assocs.c   Bounds.c  Elems.c
../List/Difference.c:
	cd ../List; ${MAKE} Difference.c

