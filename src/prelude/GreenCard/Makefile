include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB = GreenCard
SEARCH	= -P$(LOW) -P../PreludeIO

SRCS	= 
SRCS_T	=


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
extra: ${OBJDIR}/${THISLIB}.o

ifneq "${TRACING}" "T"
${OBJDIR}/${THISLIB}.o: ${THISLIB}.gc
	$(HC) -c $(LIB_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
${THISLIB}.c : ${THISLIB}.gc
	$(HC) -C $(LIB_FLAGS) $(EXTRA_H_FLAGS) $<
else
${OBJDIR}/${THISLIB}.o: ${THISLIB}.gc
	$(TR_COMPILE) -c $(LIB_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
endif


# Here are the dependencies.
