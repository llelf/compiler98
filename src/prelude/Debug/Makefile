include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= Debug
SEARCH	=

SRCS	=
SRCS_T	=

XSRCS	= DbgStub.hs



# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
XOBJS = $(patsubst %.hs, ${OBJDIR}/%.o, ${XSRCS})
XCFILES = $(patsubst %.hs, %.c, ${XSRCS})

extra: ${XOBJS}
extracfiles: ${XCFILES}
${XOBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(LIB_FLAGS) -o $@ $<
${XCFILES}: %.c : %.hs
	$(HC) -C $(LIB_FLAGS) $<



# Here are the dependencies.

