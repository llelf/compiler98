include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB = LowB
SEARCH	= -P.. -P../PreludeIO -P../IO
EXTRA_H_FLAGS	= -prelude
EXTRA_C_FLAGS	=


SRCS	= \
	PrimError.hs Blackhole.hs \
	CHGetChar.hs PrimEqHandlePosn.hs CHPutChar.hs PrimHClose.hs \
 	PrimHFileSize.hs PrimHFlush.hs PrimHGetBuffering.hs PrimHGetChar.hs \
	PrimHGetContents.hs PrimHGetPosn.hs PrimHIsEOF.hs PrimHPutChar.hs \
	PrimHSeek.hs PrimHSetBuffering.hs PrimHSetPosn.hs \
	PrimOpenFile.hs PrimOpenSocket.hs \
	CExitWith.hs PrimExitWith.hs PrimSystem.hs PrimGetArgs.hs \
	PrimGetEnv.hs PrimGetProgName.hs \
	PrimComparePS.hs PrimIndex.hs PrimLength.hs PrimPackString.hs \
	PrimUnpackPS.hs \
	PrimDecodeDouble.hs PrimDecodeFloat.hs PrimDoubleFromInteger.hs \
	PrimEncodeDouble.hs PrimEncodeFloat.hs PrimFloatFromInteger.hs \
	PrimIntFromInteger.hs \
	PrimIntegerAbs.hs PrimIntegerAdd.hs PrimIntegerAnd.hs \
	PrimIntegerEq.hs PrimIntegerFromInt.hs PrimIntegerLe.hs \
	PrimIntegerLt.hs PrimIntegerGe.hs PrimIntegerGt.hs \
	PrimIntegerMul.hs PrimIntegerNe.hs PrimIntegerNeg.hs PrimIntegerOr.hs \
	PrimIntegerQuotRem.hs PrimIntegerSub.hs

SRCS_TRICKY	= \
	_Driver.hs CString.hs LowI.hs LowPS.hs LowSystem.hs LowIO.hs


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
OBJS_TRICKY	= $(patsubst %.hs, ${OBJDIR}/%.o, ${SRCS_TRICKY})
CFILES_TRICKY	= $(patsubst %.hs, %.c, ${SRCS_TRICKY})
extra:  ${OBJS_TRICKY}
cfiles: ${CFILES_TRICKY}
${OBJS_TRICKY}: ${OBJDIR}/%.o : %.hs
	$(HC) -c  $(PART_FLAGS) $(LIB_FLAGS) -o $@ $<
${CFILES_TRICKY}: %.c : %.hs
	$(HC) -C  $(PART_FLAGS) $(LIB_FLAGS) $<

cleanExtraC:
	rm -f ${CFILES_TRICKY}


# Here are the dependencies.
${OBJDIR}/PrimError.o: PrimError.hs  ${OBJDIR}/CExitWith.o
${OBJDIR}/PrimExitWith.o: PrimExitWith.hs ${OBJDIR}/../PreludeIO/DIO.o ${OBJDIR}/CExitWith.o
${OBJDIR}/PrimGetArgs.o: PrimGetArgs.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimGetEnv.o: PrimGetEnv.hs  ${OBJDIR}/CString.o  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimGetProgName.o: PrimGetProgName.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHClose.o: PrimHClose.hs   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHFileSize.o: PrimHFileSize.hs   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHFlush.o: PrimHFlush.hs   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHGetBuffering.o: PrimHGetBuffering.hs   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHGetChar.o: PrimHGetChar.hs   ${OBJDIR}/../PreludeIO/DIO.o  ${OBJDIR}/../PreludeIO/DIOError.o  ${OBJDIR}/CHGetChar.o
${OBJDIR}/PrimHGetContents.o: PrimHGetContents.hs  ${OBJDIR}/../PreludeIO/DIO.o  ${OBJDIR}/CHGetChar.o
${OBJDIR}/PrimHGetPosn.o: PrimHGetPosn.hs   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHIsEOF.o: PrimHIsEOF.hs  ${OBJDIR}/../PreludeIO/DIOError.o   ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHPutChar.o: PrimHPutChar.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHPutChar.o: PrimHPutChar.hs  ${OBJDIR}/CHPutChar.o
${OBJDIR}/PrimHSeek.o: PrimHSeek.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHSetBuffering.o: PrimHSetBuffering.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimHSetPosn.o: PrimHSetPosn.hs  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimIntegerGe.o: PrimIntegerGe.hs  ${OBJDIR}/PrimIntegerLt.o
${OBJDIR}/PrimIntegerGt.o: PrimIntegerGt.hs  ${OBJDIR}/PrimIntegerLe.o
${OBJDIR}/PrimIntegerNe.o: PrimIntegerNe.hs  ${OBJDIR}/PrimIntegerEq.o
${OBJDIR}/PrimOpenFile.o: PrimOpenFile.hs ${OBJDIR}/CString.o  ${OBJDIR}/../PreludeIO/DIO.o
${OBJDIR}/PrimSystem.o: PrimSystem.hs  ${OBJDIR}/CString.o  ${OBJDIR}/../PreludeIO/DIOError.o  ${OBJDIR}/../PreludeIO/DIO.o  #${OBJDIR}/../PreludeIO/DErrNo.o
${OBJDIR}/PrimOpenSocket.o: PrimOpenSocket.hs ${OBJDIR}/../IO/DSocket.o ${OBJDIR}/CString.o ${OBJDIR}/../PreludeIO/DIO.o

${OBJDIR}/../PreludeIO/MsgIOError.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.o
${OBJDIR}/../PreludeIO/DIOError.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.o
${OBJDIR}/../PreludeIO/DIO.o:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIO.o
#${OBJDIR}/../DErrNo/DErrNo.o:
#	cd ../$(MACHINE); ${MAKE} ${BUILDDIR}/${OBJ}/prelude/DErrNo/DErrNo.o
${OBJDIR}/../IO/DSocket.o:
	cd ../IO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/IO/DSocket.o

${OBJDIR}/_Driver.o: _Driver.hs ${OBJDIR}/../PreludeIO/DIO.o


# And now for the C-files dependencies.
PrimError.c:          CExitWith.c
PrimExitWith.c:       ../PreludeIO/DIO.c CExitWith.c
PrimGetArgs.c:        ../PreludeIO/DIO.c
PrimGetEnv.c:         CString.c  ../PreludeIO/DIO.c
PrimGetProgName.c:    ../PreludeIO/DIO.c
PrimHClose.c:         ../PreludeIO/DIO.c
PrimHFileSize.c:      ../PreludeIO/DIO.c
PrimHFlush.c:         ../PreludeIO/DIO.c
PrimHGetBuffering.c:  ../PreludeIO/DIO.c
PrimHGetChar.c:       ../PreludeIO/DIO.c  ../PreludeIO/DIOError.c  CHGetChar.c
PrimHGetContents.c:   ../PreludeIO/DIO.c  CHGetChar.c
PrimHGetPosn.c:       ../PreludeIO/DIO.c
PrimHIsEOF.c:         ../PreludeIO/DIOError.c   ../PreludeIO/DIO.c
PrimHPutChar.c:       ../PreludeIO/DIO.c
PrimHPutChar.c:       CHPutChar.c
PrimHSeek.c:          ../PreludeIO/DIO.c
PrimHSetBuffering.c:  ../PreludeIO/DIO.c
PrimHSetPosn.c:       ../PreludeIO/DIO.c
PrimIntegerGe.c:      PrimIntegerLt.c
PrimIntegerGt.c:      PrimIntegerLe.c
PrimIntegerNe.c:      PrimIntegerEq.c
PrimOpenFile.c:       CString.c  ../PreludeIO/DIO.c
PrimSystem.c:         CString.c  ../PreludeIO/DIOError.c  ../PreludeIO/DIO.c
PrimOpenSocket.c:     ../IO/DSocket.c CString.c ../PreludeIO/DIO.c
_Driver.c:            ../PreludeIO/DIO.c

../PreludeIO/MsgIOError.c:
	cd ../PreludeIO; ${MAKE} DIOError.c
../PreludeIO/DIOError.c:
	cd ../PreludeIO; ${MAKE} DIOError.c
../PreludeIO/DIO.c:
	cd ../PreludeIO; ${MAKE} DIO.c
#../$(MACHINE)/DErrNo.c:
#	cd ../$(MACHINE);${MAKE} DErrNo.c
../IO/DSocket.c:
	cd ../IO;        ${MAKE} DSocket.c

