include Makefile.inc

THISLIB = List

#SRCS	= \
#	Inits.hs             Tails.hs \
#	Delete.hs            IntersectBy.hs       Intersect.hs \
#        Transpose.hs UnionBy.hs \
#	DeleteBy.hs          Intersperse.hs Union.hs \
#	DeleteFirsts.hs      LookupBy.hs          Unzip4.hs \
#	DeleteFirstsBy.hs    MapAccumL.hs         Unzip5.hs \
#	Difference.hs        MapAccumR.hs         Unzip6.hs \
#	ElemBy.hs            MaximumBy.hs         Unzip7.hs \
#	ElemIndex.hs         MinimumBy.hs         Zip4.hs \
#	ElemIndexBy.hs       NotElemBy.hs         Zip5.hs \
#	GenericDrop.hs       Nub.hs               Zip6.hs \
#	GenericLength.hs     NubBy.hs             Zip7.hs \
#	GenericReplicate.hs  Partition.hs         ZipWith4.hs \
#	GenericSplitAt.hs    Permutations.hs      ZipWith5.hs \
#	GenericTake.hs       Products.hs          ZipWith6.hs \
#	Group.hs             Subsequences.hs      ZipWith7.hs \
#	GroupBy.hs           Sums.hs              \
#	Sort.hs	             SortBy.hs            InsertBy.hs \
#	Find.hs              FindIndex.hs         FindIndices.hs \
#	ElemIndices.hs       GenericIndex.hs      Insert.hs \
#        IsPrefixOf.hs        IsSuffixOf.hs \
#        Unfoldr.hs

SRCS	= \
    DeleteBy.hs Delete.hs \
    DeleteFirstsBy.hs DeleteFirsts.hs \
    Difference.hs ElemBy.hs \
    ElemIndexBy.hs ElemIndex.hs ElemIndices.hs \
    Find.hs FindIndex.hs FindIndices.hs \
    GenericDrop.hs GenericIndex.hs GenericLength.hs \
    GenericReplicate.hs GenericSplitAt.hs GenericTake.hs \
    GroupBy.hs Group.hs \
    Inits.hs InsertBy.hs Insert.hs \
    IntersectBy.hs Intersect.hs \
    Intersperse.hs IsPrefixOf.hs IsSuffixOf.hs \
    LookupBy.hs MapAccumL.hs MapAccumR.hs \
    MaximumBy.hs MinimumBy.hs NotElemBy.hs \
    NubBy.hs Nub.hs \
    Partition.hs Permutations.hs Products.hs \
    SortBy.hs Sort.hs Subsequences.hs \
    Sums.hs Tails.hs Transpose.hs \
    Unfoldr.hs UnionBy.hs Union.hs \
    Unzip4.hs Unzip5.hs Unzip6.hs Unzip7.hs \
    ZipWith4.hs ZipWith5.hs ZipWith6.hs ZipWith7.hs \
    Zip4.hs Zip5.hs Zip6.hs Zip7.hs



OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part
#HEAP = +RTS -H2M -RTS


all:  ${OBJS} ${MAINOBJ}
boot: ${OBJS} ${MAINOBJ}
cfiles: ${CFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${MAINCFILE}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s
 


.SUFFIXES: .hs .o .c
.hs.o:
	$(HC) -c -P../Maybe/ $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -P../Maybe/ -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) -P../Maybe/ $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(

${OBJDIR}/Nub.o: Nub.hs ${OBJDIR}/NubBy.o
${OBJDIR}/Delete.o: Delete.hs ${OBJDIR}/DeleteBy.o
${OBJDIR}/Difference.o: Difference.hs ${OBJDIR}/Delete.o
${OBJDIR}/DeleteFirsts.o: DeleteFirsts.hs ${OBJDIR}/Difference.o
${OBJDIR}/DeleteFirstsBy.o: DeleteFirstsBy.hs ${OBJDIR}/DeleteBy.o
${OBJDIR}/ElemIndex.o: ElemIndex.hs ${OBJDIR}/ElemIndexBy.o
${OBJDIR}/Group.o: Group.hs ${OBJDIR}/GroupBy.o
${OBJDIR}/NotElemBy.o: NotElemBy.hs ${OBJDIR}/ElemBy.o
${OBJDIR}/Nub.o: Nub.hs ${OBJDIR}/NubBy.o
${OBJDIR}/Zip4.o: Zip4.hs ${OBJDIR}/ZipWith4.o
${OBJDIR}/Zip5.o: Zip5.hs ${OBJDIR}/ZipWith5.o
${OBJDIR}/Zip6.o: Zip6.hs ${OBJDIR}/ZipWith6.o
${OBJDIR}/Zip7.o: Zip7.hs ${OBJDIR}/ZipWith7.o
${OBJDIR}/Sort.o: Sort.hs ${OBJDIR}/SortBy.o
${OBJDIR}/SortBy.o: SortBy.hs ${OBJDIR}/InsertBy.o
${OBJDIR}/ElemIndex.o: ElemIndex.hs ${OBJDIR}/FindIndex.o
${OBJDIR}/ElemIndices.o: ElemIndices.hs ${OBJDIR}/FindIndices.o
${OBJDIR}/Find.o: Find.hs ${OBJDIR}/../Maybe/ListToMaybe.o
${OBJDIR}/FindIndex.o: FindIndex.hs  ${OBJDIR}/FindIndices.o ${OBJDIR}/../Maybe/ListToMaybe.o
${OBJDIR}/GenericReplicate.o: GenericReplicate.hs ${OBJDIR}/GenericTake.o
${OBJDIR}/Union.o: Union.hs ${OBJDIR}/Difference.o
${OBJDIR}/../Maybe/ListToMaybe.o:
	cd ../Maybe; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/Maybe/ListToMaybe.o


# C-files dependencies.
Nub.c:            NubBy.c
Delete.c:         DeleteBy.c
Difference.c:     Delete.c
DeleteFirsts.c:   Difference.c
DeleteFirstsBy.c: DeleteBy.c
ElemIndex.c:      ElemIndexBy.c
Group.c:          GroupBy.c
NotElemBy.c:      ElemBy.c
Nub.c:            NubBy.c
Zip4.c:           ZipWith4.c
Zip5.c:           ZipWith5.c
Zip6.c:           ZipWith6.c
Zip7.c:           ZipWith7.c
Sort.c:           SortBy.c
SortBy.c:         InsertBy.c
ElemIndex.c:      FindIndex.c
ElemIndices.c:    FindIndices.c
Find.c:           ../Maybe/ListToMaybe.c
FindIndex.c:      FindIndices.c ../Maybe/ListToMaybe.c
GenericReplicate.c: GenericTake.c
Union.c:          Difference.c
../Maybe/ListToMaybe.c:
	cd ../Maybe; ${MAKE} ListToMaybe.c
