include Makefile.inc

THISLIB	= BinArray
SEARCH	= -P../Binary -I../Binary
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	= -I../Binary

SRCS	= \
	UnboxedArray.hs \
	AllocUBA.gc DUnboxedArray.gc RUBA.gc GetUBAEnd.gc Binary_UBA.gc \
	GetUBAFree.gc WUBA.gc \
	cLowUnboxedArray.c
SRCS_T	=


# Here are some extra rules.
cfiles: cfilescheck
cfilescheck:
	cp $(INCDIR)/Binary.hi ../Binary	# big hack around compiler bug!


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJS}: cLowUnboxedArray.h

${OBJDIR}/AllocUBA.o: AllocUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/Binary_UBA.o: Binary_UBA.gc  ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/DUnboxedArray.o: DUnboxedArray.gc
${OBJDIR}/GetUBAEnd.o: GetUBAEnd.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/GetUBAFree.o: GetUBAFree.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/RUBA.o: RUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/WUBA.o: WUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/UnboxedArray.o: UnboxedArray.hs ${OBJDIR}/DUnboxedArray.o ${OBJDIR}/AllocUBA.o ${OBJDIR}/Binary_UBA.o ${OBJDIR}/GetUBAEnd.o ${OBJDIR}/GetUBAFree.o ${OBJDIR}/RUBA.o ${OBJDIR}/WUBA.o#
${OBJDIR}/cLowUnboxedArray.o: cLowUnboxedArray.c
${OBJDIR}/BinArray.o: BinArray.hs ${OBJDIR}/UnboxedArray.o ${OBJS}


# C-files dependencies.
AllocUBA.c:     DUnboxedArray.c
Binary_UBA.c:   DUnboxedArray.c
GetUBAEnd.c:    DUnboxedArray.c
GetUBAFree.c:   DUnboxedArray.c
RUBA.c:         DUnboxedArray.c
WUBA.c:         DUnboxedArray.c
UnboxedArray.c: DUnboxedArray.c AllocUBA.c Binary_UBA.c GetUBAEnd.c GetUBAFree.c RUBA.c WUBA.c
BinArray.c:     UnboxedArray.c 

