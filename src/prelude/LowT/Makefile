include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= LowT
SEARCH	= -P../PreludeIO -P../System -P..

SRCS_T	= \
	PrimHGetChar.hs PrimHGetContents.hs PrimHPutChar.hs PrimHClose.hs \
	PrimExitWith.hs \
	FFIBuiltin.hs PrimPackString.hs PrimUnpackPS.hs CString.hs \
	NonStdTrace.hs \
	LowIO.hs LowSystem.hs LowPS.hs

SRCS_NT	= _Driver.hs PrimIntFromInteger.hs 
OBJS_NT	= $(patsubst %.hs, ${OBJDIR}/%.o, ${SRCS_NT})


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
extra: ${OBJS_NT}
$(OBJS_NT): ${OBJDIR}/%.o: %.hs
	$(X_COMPILE) -c $(PART_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<



# Here are the dependencies.
${OBJDIR}/../PreludeIO/MsgIOError.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.o
${OBJDIR}/../PreludeIO/DIOError.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.o
${OBJDIR}/../PreludeIO/DIO.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIO.o
${OBJDIR}/../IO/DSocket.o:
	cd ../IO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/IO/DSocket.o

${OBJDIR}/_Driver.o: _Driver.hs ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHGetChar.o: PrimHGetChar.hs   ${OBJDIR}/../PreludeIO/DIO.o  ${OBJDIR}/../PreludeIO/DIOError.o
${OBJDIR}/PrimExitWith.o: PrimExitWith.hs ${OBJDIR}/../PreludeIO/DIO.o

