include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= NonStd
SEARCH	= -P$(LOW)

SRCS	=
SRCS_T	=
LIBSRCS = \
	NonStdUnsafeCoerce.hs \
	NonStdProfile.hs \
	NonStdEval.hs  \
	NonStdTrace.hs \
	Warning.hs \
	NonStdGetPID.gc

HCFILES = $(patsubst %.hs, %.c, $(filter %.hs, ${LIBSRCS}))
GCFILES = $(patsubst %.gc, %.c, $(filter %.gc, ${LIBSRCS}))

extracfiles: $(HCFILES) $(GCFILES)


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
HOBJS	= $(patsubst %.hs, ${OBJDIR}/%.o, $(filter %.hs, ${LIBSRCS}))
GOBJS	= $(patsubst %.gc, ${OBJDIR}/%.o, $(filter %.gc, ${LIBSRCS}))

extra:	${HOBJS} ${GOBJS}
${HOBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(LIB_FLAGS) -o $@ $<
${GOBJS}: ${OBJDIR}/%.o : %.gc
	$(HC) -c $(LIB_FLAGS) -o $@ $<
${HCFILES}: %.c : %.hs
	$(HC) -C $(LIB_FLAGS) $<
${GCFILES}: %.c : %.gc
	$(HC) -C $(LIB_FLAGS) $<

cleanExtraC:
	rm -f ${HCFILES} ${GCFILES}



# Here are the dependencies.
