include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= Observe
ifeq "${BUILDWITH}" "nhc98"
EXTRA_H_FLAGS = +RTS -H32M -RTS
endif

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
	$(HC) -c $(LIB_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
${LHCFILES}: %.c : %.lhs
	$(HC) -C $(LIB_FLAGS) $<

cleanExtraC:
	rm -f ${LHCFILES}



# Here are the dependencies.
