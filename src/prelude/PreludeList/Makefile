include Makefile.inc

THISLIB = PreludeList

SRCS	= \
	Append.hs      Elem.hs        Lines.hs       Replicate.hs  \
	Lookup.hs      Reverse.hs     Unlines.hs     Map.hs \
	All.hs         Foldl.hs       Scanl.hs       Unwords.hs	\
	And.hs         Foldl1.hs      Maximum.hs     Scanl1.hs \
	Any.hs         Foldr.hs       Minimum.hs     Scanr.hs  \
        Unzip.hs       Unzip3.hs      ZipWith.hs     Zip3.hs 	\
	Break.hs       Foldr1.hs      NotElem.hs     Scanr1.hs \
	Head.hs        Span.hs        Zip.hs         Concat.hs \
	ConcatMap.hs   Index.hs       Null.hs        SplitAt.hs \
	Cycle.hs       Init.hs        Or.hs          Sum.hs \
	Iterate.hs     Tail.hs        ZipWith3.hs    Words.hs 	\
	Drop.hs        Last.hs        Product.hs     Take.hs \
	DropWhile.hs   Length.hs      Repeat.hs      TakeWhile.hs \
        Filter.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part -prelude -redefine
#HEAP = +RTS -H4M -RTS


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



${OBJS} ${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<

${CFILES} ${MAINCFILE}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<

.SUFFIXES: .hs .o .c .hi

../PreludeList/%.hi : %.hs
	$(HC) -c $(HFLAGS) -o /dev/null $<

# I know that they depoend on .hi, but make is stupid :-(

