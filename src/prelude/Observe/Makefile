include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= Observe

SRCS	=
SRCS_T	=
LIBSRCS = Observe.lhs

LHCFILES = $(patsubst %.lhs, %.c, $(filter %.lhs, ${LIBSRCS}))

extracfiles: $(LHCFILES)


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
LHOBJS	= $(patsubst %.lhs, ${OBJDIR}/%.$O, $(filter %.lhs, ${LIBSRCS}))

extra:	${LHOBJS}
${LHOBJS}: ${OBJDIR}/%.$O : %.lhs
	$(HC) -c $(LIB_FLAGS) -o $@ $<
${LHCFILES}: %.c : %.lhs
	$(HC) -C $(LIB_FLAGS) $<

cleanExtraC:
	rm -f ${LHCFILES}



# Here are the dependencies.
