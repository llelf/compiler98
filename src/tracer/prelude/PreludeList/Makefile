include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/PreludeList

VPATH = .

.SUFFIXES: .hi .hs .o .po


SRCS= All.hs And.hs Any.hs Break.hs ConcatMap.hs DropWhile.hs Elem.hs \
	  Foldl.hs Foldr.hs Head.hs Index.hs Iterate.hs Last.hs Length.hs \
	  Lines.hs NotElem.hs Null.hs Or.hs Repeat.hs Reverse.hs Span.hs \
	  SplitAt.hs Tail.hs Take.hs Unlines.hs Words.hs Zip.hs ZipWith.hs \
	  Foldl1.hs Foldr1.hs Init.hs Lookup.hs Maximum.hs Drop.hs \
	  Unzip.hs Unzip3.hs ZipWith3.hs Zip3.hs Unwords.hs

NOTDONE= Cycle.hs \
	 Minimum.hs Product.hs Replicate.hs Scanl.hs Scanl1.hs Scanr.hs \
	 Scanr1.hs Sum.hs TakeWhile.hs
	 
MAINSRC= PreludeList.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

install:

${MAINOBJ}: ${OBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

clean:
	rm -f ${OBJS} ${MAINOBJ}
