include Makefile.inc

THISLIB	= Binary
SEARCH	=
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	=

SRCS	= \
	Bin.hs CBinary.hs Instances.hs LeftLeft.hs \
	PutAt.hs PutAtEnd.hs GetAt.hs GetFAt.hs \
	BinHandle.gc BinPtr.gc GetBitsF.gc GetBits.gc PutBits.gc \
	AlignBin.gc BinIOMode.gc BinLocation.gc \
	ClearBits.gc CloseBin.gc CompareBin.gc \
	CopyBin.gc CopyBytes.gc CopyBits.gc \
	EndBin.gc EqualsBin.gc \
	FreezeBin.gc IsEOFBin.gc \
	OpenBin.gc SkipBits.gc StdMem.gc SizeBin.gc \
	SeekBin.gc TellBin.gc \
	cLowBinary.c
	# SizedBin.hs \
	# SizeofBin.gc DirectPut.gc
SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.
${OBJDIR}/cLowBinary.o: cLowBinary.h cLowBinary.c
${OBJDIR}/AlignBin.o: AlignBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/BinHandle.o: BinHandle.gc
${OBJDIR}/BinIOMode.o: BinIOMode.gc
${OBJDIR}/BinLocation.o: BinLocation.gc ${OBJDIR}/BinIOMode.o
${OBJDIR}/BinPtr.o: BinPtr.gc
${OBJDIR}/Bin.o: Bin.hs ${OBJDIR}/BinPtr.o
${OBJDIR}/CBinary.o: CBinary.hs ${OBJDIR}/BinHandle.o ${OBJDIR}/Bin.o
${OBJDIR}/ClearBits.o: ClearBits.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/CloseBin.o: CloseBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/CompareBin.o: CompareBin.gc cLowBinary.h ${OBJDIR}/BinPtr.o ${OBJDIR}/BinHandle.o
${OBJDIR}/CopyBin.o: CopyBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinLocation.o ${OBJDIR}/OpenBin.o ${OBJDIR}/SizeBin.o
${OBJDIR}/CopyBits.o: CopyBits.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o ${OBJDIR}/GetBits.o ${OBJDIR}/PutBits.o ${OBJDIR}/SeekBin.o
${OBJDIR}/CopyBytes.o: CopyBytes.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/EndBin.o: EndBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/EqualsBin.o: EqualsBin.gc cLowBinary.h ${OBJDIR}/BinPtr.o ${OBJDIR}/BinHandle.o
${OBJDIR}/FreezeBin.o: FreezeBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/GetAt.o: GetAt.hs ${OBJDIR}/CBinary.o ${OBJDIR}/BinHandle.o ${OBJDIR}/Bin.o ${OBJDIR}/SeekBin.o
${OBJDIR}/GetBitsF.o: GetBitsF.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/GetBits.o: GetBits.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/GetFAt.o: GetFAt.hs ${OBJDIR}/CBinary.o ${OBJDIR}/BinHandle.o ${OBJDIR}/Bin.o
${OBJDIR}/IsEOFBin.o: IsEOFBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/Instances.o: Instances.hs ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o ${OBJDIR}/BinLocation.o ${OBJDIR}/Bin.o ${OBJDIR}/OpenBin.o ${OBJDIR}/TellBin.o ${OBJDIR}/AlignBin.o ${OBJDIR}/PutBits.o ${OBJDIR}/GetBits.o ${OBJDIR}/CBinary.o ${OBJDIR}/GetBitsF.o ${OBJDIR}/LeftLeft.o ${OBJDIR}/FreezeBin.o
${OBJDIR}/LeftLeft.o: LeftLeft.hs
${OBJDIR}/OpenBin.o: OpenBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinIOMode.o ${OBJDIR}/BinLocation.o
${OBJDIR}/PutAt.o: PutAt.hs ${OBJDIR}/CBinary.o ${OBJDIR}/BinHandle.o ${OBJDIR}/Bin.o ${OBJDIR}/SeekBin.o
${OBJDIR}/PutAtEnd.o: PutAtEnd.hs ${OBJDIR}/CBinary.o ${OBJDIR}/BinHandle.o ${OBJDIR}/Bin.o ${OBJDIR}/EndBin.o ${OBJDIR}/SeekBin.o
${OBJDIR}/PutBits.o: PutBits.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/SkipBits.o: SkipBits.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/StdMem.o: StdMem.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/SizeBin.o: SizeBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o
${OBJDIR}/SizedBin.o: SizedBin.hs ${OBJDIR}/BinPtr.o ${OBJDIR}/CBinary.o ${OBJDIR}/PutBits.o ${OBJDIR}/GetBits.o ${OBJDIR}/AlignBin.o ${OBJDIR}/SizeofBin.o ${OBJDIR}/EqualsBin.o ${OBJDIR}/CompareBin.o ${OBJDIR}/StdMem.o ${OBJDIR}/GetFAt.o
${OBJDIR}/SeekBin.o: SeekBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o
${OBJDIR}/TellBin.o: TellBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o


#${OBJDIR}/Binary.o: Binary.hs ${OBJS}

#${OBJDIR}/DirectPut.o: DirectPut.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/SizedBin.o ${OBJDIR}/BinPtr.o
#${OBJDIR}/SizeofBin.o: SizeofBin.gc cLowBinary.h ${OBJDIR}/BinHandle.o ${OBJDIR}/BinPtr.o


# C-files dependencies.

AlignBin.c:    BinHandle.c
BinLocation.c: BinIOMode.c
Bin.c:         BinPtr.c
CBinary.c:     BinHandle.c Bin.c
ClearBits.c:   BinHandle.c BinPtr.c
CloseBin.c:    BinHandle.c
CompareBin.c:  BinPtr.c BinHandle.c
CopyBin.c:     BinHandle.c BinLocation.c OpenBin.c SizeBin.c
CopyBits.c:    BinHandle.c BinPtr.c GetBits.c PutBits.c SeekBin.c
CopyBytes.c:   BinHandle.c BinPtr.c
EndBin.c:      BinHandle.c BinPtr.c
EqualsBin.c:   BinPtr.c BinHandle.c
FreezeBin.c:   BinHandle.c
GetAt.c:       CBinary.c BinHandle.c Bin.c SeekBin.c
GetBitsF.c:    BinHandle.c BinPtr.c
GetBits.c:     BinHandle.c
GetFAt.c:      CBinary.c BinHandle.c Bin.c
IsEOFBin.c:    BinHandle.c
Instances.c:   BinHandle.c BinPtr.c BinLocation.c Bin.c OpenBin.c TellBin.c AlignBin.c PutBits.c GetBits.c CBinary.c GetBitsF.c LeftLeft.c FreezeBin.c
OpenBin.c:     BinHandle.c BinIOMode.c BinLocation.c
PutAt.c:       CBinary.c BinHandle.c Bin.c SeekBin.c
PutAtEnd.c:    CBinary.c BinHandle.c Bin.c EndBin.c SeekBin.c
PutBits.c:     BinHandle.c BinPtr.c
SkipBits.c:    BinHandle.c
StdMem.c:      BinHandle.c
SizeBin.c:     BinHandle.c
SizedBin.c:    BinPtr.c CBinary.c PutBits.c GetBits.c AlignBin.c SizeofBin.c EqualsBin.c CompareBin.c StdMem.c GetFAt.c
SeekBin.c:     BinHandle.c BinPtr.c
TellBin.c:     BinHandle.c BinPtr.c

