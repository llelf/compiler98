include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB	= LowT
SEARCH	= -P../PreludeIO -P../System -P..

SRCS_T	= \
	PrimHGetChar.hs PrimHGetContents.hs PrimHPutChar.hs PrimHClose.hs \
	PrimExitWith.hs \
	FFIBuiltin.hs PrimPackString.hs PrimUnpackPS.hs CString.hs \
	NonStdTrace.hs \
	LowIO.hs LowSystem.hs LowPS.hs \
	PrimIntegerAbs.hs PrimIntegerAdd.hs PrimIntegerAnd.hs \
	PrimIntegerEq.hs PrimIntegerLe.hs PrimIntegerLt.hs \
	PrimIntegerGe.hs PrimIntegerGt.hs PrimIntegerNe.hs \
	PrimIntegerMul.hs PrimIntegerNeg.hs PrimIntegerOr.hs \
	PrimIntegerQuotRem.hs PrimIntegerSub.hs \
	PrimIntegerFromInt.hs PrimIntFromIntegerC.hs \
	PrimEncodeDouble.hs PrimDoubleFromInteger.hs \
	PrimEncodeFloat.hs  PrimFloatFromInteger.hs \
	PrimDecodeDouble.hs PrimDecodeFloat.hs \
	PrimsFloat.hs PrimsDouble.hs

SRCS_NT	= _Driver.hs PrimIntFromInteger.hs 
EXTRA_T	= NonStdUnsafeCoerce.hs

EXTRA_O = $(patsubst %.hs, ${OBJDIR}/%.o, ${EXTRA_T})

# Here are the main rules.
include ../Makefile.common



# Here are some extra rules.
extra: ${EXTRA_O}
${EXTRA_O}: ${OBJDIR}/%.o : %.hs
	${TR_COMPILE} -c $(LIB_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<


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

