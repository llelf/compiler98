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
${OBJDIR}/cLowBinary.$O: cLowBinary.h cLowBinary.c
${OBJDIR}/AlignBin.$O: AlignBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/BinHandle.$O: BinHandle.gc
${OBJDIR}/BinIOMode.$O: BinIOMode.gc
${OBJDIR}/BinLocation.$O: BinLocation.gc ${OBJDIR}/BinIOMode.$O
${OBJDIR}/BinPtr.$O: BinPtr.gc
${OBJDIR}/Bin.$O: Bin.hs ${OBJDIR}/BinPtr.$O
${OBJDIR}/CBinary.$O: CBinary.hs ${OBJDIR}/BinHandle.$O ${OBJDIR}/Bin.$O
${OBJDIR}/ClearBits.$O: ClearBits.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/CloseBin.$O: CloseBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/CompareBin.$O: CompareBin.gc cLowBinary.h ${OBJDIR}/BinPtr.$O ${OBJDIR}/BinHandle.$O
${OBJDIR}/CopyBin.$O: CopyBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinLocation.$O ${OBJDIR}/OpenBin.$O ${OBJDIR}/SizeBin.$O
${OBJDIR}/CopyBits.$O: CopyBits.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O ${OBJDIR}/GetBits.$O ${OBJDIR}/PutBits.$O ${OBJDIR}/SeekBin.$O
${OBJDIR}/CopyBytes.$O: CopyBytes.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/EndBin.$O: EndBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/EqualsBin.$O: EqualsBin.gc cLowBinary.h ${OBJDIR}/BinPtr.$O ${OBJDIR}/BinHandle.$O
${OBJDIR}/FreezeBin.$O: FreezeBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/GetAt.$O: GetAt.hs ${OBJDIR}/CBinary.$O ${OBJDIR}/BinHandle.$O ${OBJDIR}/Bin.$O ${OBJDIR}/SeekBin.$O
${OBJDIR}/GetBitsF.$O: GetBitsF.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/GetBits.$O: GetBits.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/GetFAt.$O: GetFAt.hs ${OBJDIR}/CBinary.$O ${OBJDIR}/BinHandle.$O ${OBJDIR}/Bin.$O
${OBJDIR}/IsEOFBin.$O: IsEOFBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/Instances.$O: Instances.hs ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O ${OBJDIR}/BinLocation.$O ${OBJDIR}/Bin.$O ${OBJDIR}/OpenBin.$O ${OBJDIR}/TellBin.$O ${OBJDIR}/AlignBin.$O ${OBJDIR}/PutBits.$O ${OBJDIR}/GetBits.$O ${OBJDIR}/CBinary.$O ${OBJDIR}/GetBitsF.$O ${OBJDIR}/LeftLeft.$O ${OBJDIR}/FreezeBin.$O
${OBJDIR}/LeftLeft.$O: LeftLeft.hs
${OBJDIR}/OpenBin.$O: OpenBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinIOMode.$O ${OBJDIR}/BinLocation.$O
${OBJDIR}/PutAt.$O: PutAt.hs ${OBJDIR}/CBinary.$O ${OBJDIR}/BinHandle.$O ${OBJDIR}/Bin.$O ${OBJDIR}/SeekBin.$O
${OBJDIR}/PutAtEnd.$O: PutAtEnd.hs ${OBJDIR}/CBinary.$O ${OBJDIR}/BinHandle.$O ${OBJDIR}/Bin.$O ${OBJDIR}/EndBin.$O ${OBJDIR}/SeekBin.$O
${OBJDIR}/PutBits.$O: PutBits.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/SkipBits.$O: SkipBits.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/StdMem.$O: StdMem.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/SizeBin.$O: SizeBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O
${OBJDIR}/SizedBin.$O: SizedBin.hs ${OBJDIR}/BinPtr.$O ${OBJDIR}/CBinary.$O ${OBJDIR}/PutBits.$O ${OBJDIR}/GetBits.$O ${OBJDIR}/AlignBin.$O ${OBJDIR}/SizeofBin.$O ${OBJDIR}/EqualsBin.$O ${OBJDIR}/CompareBin.$O ${OBJDIR}/StdMem.$O ${OBJDIR}/GetFAt.$O
${OBJDIR}/SeekBin.$O: SeekBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O
${OBJDIR}/TellBin.$O: TellBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O


#${OBJDIR}/Binary.$O: Binary.hs ${OBJS}

#${OBJDIR}/DirectPut.$O: DirectPut.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/SizedBin.$O ${OBJDIR}/BinPtr.$O
#${OBJDIR}/SizeofBin.$O: SizeofBin.gc cLowBinary.h ${OBJDIR}/BinHandle.$O ${OBJDIR}/BinPtr.$O


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

