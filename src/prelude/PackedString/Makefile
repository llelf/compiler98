include Makefile.inc

THISLIB	= PackedString
SEARCH	= -P$(LOW)

SRCS	= \
	Index.hs            Span.hs \
	Append.hs           Length.hs             SplitAt.hs \
	Break.hs            Substr.hs \
	Concat.hs           Lines.hs              DPackedString.hs \
	Cons.hs             Map.hs                Tail.hs \
	Drop.hs             Nil.hs                Take.hs \
	DropWhile.hs        Null.hs               TakeWhile.hs \
	Eq_PackedString.hs  Ord_PackedString.hs   Unlines.hs \
	Filter.hs           PackString.hs         UnpackPS.hs \
	Foldl.hs            Read_PackedString.hs  Unwords.hs \
	Foldr.hs            Reverse.hs            Words.hs \
	Head.hs             Show_PackedString.hs
SRCS_T	= \
	DPackedString.hs PackString.hs UnpackPS.hs


# Here are the main rules.
include ../Makefile.common


# Here are the dependencies.

${OBJDIR}/Append.o: Append.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/UnpackPS.o ${OBJDIR}/PackString.o
${OBJDIR}/Break.o: Break.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Span.o
${OBJDIR}/Concat.o: Concat.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Nil.o ${OBJDIR}/Append.o
${OBJDIR}/Cons.o: Cons.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Append.o ${OBJDIR}/PackString.o
${OBJDIR}/Drop.o: Drop.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Length.o ${OBJDIR}/Substr.o
${OBJDIR}/DropWhile.o: DropWhile.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Nil.o ${OBJDIR}/Length.o ${OBJDIR}/Index.o ${OBJDIR}/Substr.o
${OBJDIR}/Eq_PackedString.o: Eq_PackedString.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Eval_PackedString.o: Eval_PackedString.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Filter.o: Filter.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Foldl.o: Foldl.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Foldr.o: Foldr.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Head.o: Head.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Index.o
${OBJDIR}/Index.o: Index.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Length.o: Length.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Lines.o: Lines.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Null.o ${OBJDIR}/Break.o ${OBJDIR}/Tail.o
${OBJDIR}/Map.o: Map.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Nil.o: Nil.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o
${OBJDIR}/Null.o: Null.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Length.o
${OBJDIR}/Ord_PackedString.o: Ord_PackedString.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Eq_PackedString.o 
${OBJDIR}/PackString.o: PackString.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Read_PackedString.o: Read_PackedString.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Reverse.o: Reverse.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Show_PackedString.o: Show_PackedString.hs ${OBJDIR}/DPackedString.o 
${OBJDIR}/Span.o: Span.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Length.o ${OBJDIR}/Index.o ${OBJDIR}/Substr.o ${OBJDIR}/Nil.o
${OBJDIR}/SplitAt.o: SplitAt.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Take.o ${OBJDIR}/Drop.o
${OBJDIR}/Substr.o: Substr.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Tail.o: Tail.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Substr.o ${OBJDIR}/Length.o
${OBJDIR}/Take.o: Take.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Substr.o ${OBJDIR}/Length.o
${OBJDIR}/TakeWhile.o: TakeWhile.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/Length.o ${OBJDIR}/Index.o ${OBJDIR}/Substr.o
${OBJDIR}/Unlines.o: Unlines.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/UnpackPS.o: UnpackPS.hs ${OBJDIR}/DPackedString.o # ${OBJDIR}/Length.o ${OBJDIR}/Index.o
${OBJDIR}/Unwords.o: Unwords.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/PackString.o ${OBJDIR}/UnpackPS.o
${OBJDIR}/Words.o: Words.hs ${OBJDIR}/DPackedString.o ${OBJDIR}/DropWhile.o ${OBJDIR}/Null.o ${OBJDIR}/Break.o


# C-files dependencies.

Append.c:     DPackedString.c UnpackPS.c PackString.c
Break.c:      DPackedString.c Span.c
Concat.c:     DPackedString.c Nil.c Append.c
Cons.c:       DPackedString.c Append.c PackString.c
Drop.c:       DPackedString.c Length.c Substr.c
DropWhile.c:  DPackedString.c Nil.c Length.c Index.c Substr.c
Filter.c:     DPackedString.c PackString.c UnpackPS.c
Foldl.c:      DPackedString.c UnpackPS.c
Foldr.c:      DPackedString.c UnpackPS.c
Head.c:       DPackedString.c Index.c
Index.c:      DPackedString.c 
Length.c:     DPackedString.c 
Lines.c:      DPackedString.c Null.c Break.c Tail.c
Map.c:        DPackedString.c PackString.c UnpackPS.c
Nil.c:        DPackedString.c PackString.c
Null.c:       DPackedString.c Length.c
PackString.c: DPackedString.c 
Reverse.c:    DPackedString.c PackString.c UnpackPS.c
Span.c:       DPackedString.c Length.c Index.c Substr.c Nil.c
SplitAt.c:    DPackedString.c Take.c Drop.c
Substr.c:     DPackedString.c PackString.c UnpackPS.c
Tail.c:       DPackedString.c Substr.c Length.c
Take.c:       DPackedString.c Substr.c Length.c
TakeWhile.c:  DPackedString.c Length.c Index.c Substr.c
Unlines.c:    DPackedString.c PackString.c UnpackPS.c
UnpackPS.c:   DPackedString.c # Length.c Index.c
Unwords.c:    DPackedString.c PackString.c UnpackPS.c
Words.c:      DPackedString.c DropWhile.c Null.c Break.c

Eq_PackedString.c:   DPackedString.c 
Eval_PackedString.c: DPackedString.c 
Ord_PackedString.c:  DPackedString.c Eq_PackedString.c 
Read_PackedString.c: DPackedString.c 
Show_PackedString.c: DPackedString.c 

