include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/List

VPATH = .

.SUFFIXES: .hi .hs .o


SRCS= DeleteBy.hs Delete.hs Difference.hs NubBy.hs Nub.hs Partition.hs \
      Intersperse.hs Intersect.hs

NOTDONE= Inits.hs DeleteFirsts.hs DeleteFirstsBy.hs ElemBy.hs ElemIndexBy.hs \
	 GenericDrop.hs GenericLength.hs GenericReplicate.hs \
	 GenericSplitAt.hs GenericTake.hs Group.hs GroupBy.hs Sort.hs \
	 Find.hs FindIndex.hs FindIndices.hs ElemIndex.hs ElemIndices.hs \
	 Tails.hs LookupBy.hs MapAccumL.hs \
	 MapAccumR.hs MaximumBy.hs MinimumBy.hs NotElemBy.hs Permutations.hs \
	 Products.hs Subsequences.hs Sums.hs SortBy.hs Transpose.hs Union.hs \
	 Unzip4.hs Unzip5.hs Unzip6.hs Unzip7.hs \
	 Zip4.hs Zip5.hs Zip6.hs Zip7.hs \
	 ZipWith4.hs ZipWith5.hs ZipWith6.hs ZipWith7.hs InsertBy.hs

MAINSRC=List.hs
MAINIFC=$(patsubst %.hs,%.hi,${MAINSRC})

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})

MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

install:

install_ifc:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f ${OBJS} ${MAINOBJ}
