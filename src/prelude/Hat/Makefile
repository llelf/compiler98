include Makefile.inc

THISLIB	= Hat
SEARCH	=
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	= -DVERSION="\"$(VERSION)\""

SRCS	= \
	hat.c \
	TPreludeBuiltinTypes.hs PreludeBuiltin.hs PreludeBasic.hs \
	PreludeTracing.hs CharTracing.hs
SRCS_T	=

# Here are the main rules.
include ../Makefile.common

TPreludeBuiltin.$O: PreludeBuiltin.hs ${OBJDIR}/TPreludeBuiltinTypes.$O
	hmake -ghc -hat -trusted PreludeBuiltin.hs

TPreludeBasic.$O: PreludeBasic.hs ${OBJDIR}/TPreludeBuiltin.$O
	hmake -ghc -hat -trusted PreludeBasic.hs

TPrelude.$O: PreludeTracing.hs ${OBJDIR}/TPreludeBasic.$O
	hmake -ghc -hat -trusted PreludeTracing.hs
        mv TPreludeTracing.$O TPrelude.$O
        mv TPreludeTracing.$HI TPrelude.$HI

TChar.$O: CharTracing.hs ${OBJDIR}/TPreludeBasic.$O
	hmake -ghc -hat -trusted CharTracing.hs
        mv TCharTracing.$O TChar.$O
        mv TCharTracing.$HI TChar.$HI

# single archive for all .*O ?


# Here are the dependencies.
${OBJDIR}/Hat.$O: Hat.hs 
${OBJDIR}/TPreludeBuiltinTypes.$O: ${OBJDIR}/Hat.$O 

# C-files dependencies.
${OBJDIR}/hat.$O: hat.c hat.h art.h
