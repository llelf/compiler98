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

${OBJDIR}/Append.$O: Append.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/UnpackPS.$O ${OBJDIR}/PackString.$O
${OBJDIR}/Break.$O: Break.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Span.$O
${OBJDIR}/Concat.$O: Concat.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Nil.$O ${OBJDIR}/Append.$O
${OBJDIR}/Cons.$O: Cons.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Append.$O ${OBJDIR}/PackString.$O
${OBJDIR}/Drop.$O: Drop.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Length.$O ${OBJDIR}/Substr.$O
${OBJDIR}/DropWhile.$O: DropWhile.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Nil.$O ${OBJDIR}/Length.$O ${OBJDIR}/Index.$O ${OBJDIR}/Substr.$O
${OBJDIR}/Eq_PackedString.$O: Eq_PackedString.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Eval_PackedString.$O: Eval_PackedString.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Filter.$O: Filter.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Foldl.$O: Foldl.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Foldr.$O: Foldr.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Head.$O: Head.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Index.$O
${OBJDIR}/Index.$O: Index.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Length.$O: Length.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Lines.$O: Lines.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Null.$O ${OBJDIR}/Break.$O ${OBJDIR}/Tail.$O
${OBJDIR}/Map.$O: Map.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Nil.$O: Nil.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O
${OBJDIR}/Null.$O: Null.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Length.$O
${OBJDIR}/Ord_PackedString.$O: Ord_PackedString.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Eq_PackedString.$O 
${OBJDIR}/PackString.$O: PackString.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Read_PackedString.$O: Read_PackedString.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Reverse.$O: Reverse.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Show_PackedString.$O: Show_PackedString.hs ${OBJDIR}/DPackedString.$O 
${OBJDIR}/Span.$O: Span.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Length.$O ${OBJDIR}/Index.$O ${OBJDIR}/Substr.$O ${OBJDIR}/Nil.$O
${OBJDIR}/SplitAt.$O: SplitAt.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Take.$O ${OBJDIR}/Drop.$O
${OBJDIR}/Substr.$O: Substr.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Tail.$O: Tail.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Substr.$O ${OBJDIR}/Length.$O
${OBJDIR}/Take.$O: Take.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Substr.$O ${OBJDIR}/Length.$O
${OBJDIR}/TakeWhile.$O: TakeWhile.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/Length.$O ${OBJDIR}/Index.$O ${OBJDIR}/Substr.$O
${OBJDIR}/Unlines.$O: Unlines.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/UnpackPS.$O: UnpackPS.hs ${OBJDIR}/DPackedString.$O # ${OBJDIR}/Length.$O ${OBJDIR}/Index.$O
${OBJDIR}/Unwords.$O: Unwords.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/PackString.$O ${OBJDIR}/UnpackPS.$O
${OBJDIR}/Words.$O: Words.hs ${OBJDIR}/DPackedString.$O ${OBJDIR}/DropWhile.$O ${OBJDIR}/Null.$O ${OBJDIR}/Break.$O


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

