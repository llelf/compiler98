include Makefile.inc

EXCLUDE_THISLIB_HS = yes
THISLIB = LowB
SEARCH	= -P.. -P../PreludeIO -P../IO
EXTRA_H_FLAGS	= -prelude
EXTRA_C_FLAGS	=


SRCS	= \
	PrimError.hs Blackhole.hs \
	PrimOpenSocket.hs \
	CExitWith.hs PrimExitWith.hs PrimGetArgs.hs \
	PrimComparePS.hs PrimIndex.hs PrimLength.hs PrimPackString.hs \
	PrimUnpackPS.hs \
	PrimDecodeDouble.hs PrimDecodeFloat.hs PrimDoubleFromInteger.hs \
	PrimEncodeDouble.hs PrimEncodeFloat.hs PrimFloatFromInteger.hs \
	PrimIntFromInteger.hs \
	PrimIntegerAbs.hs PrimIntegerAdd.hs PrimIntegerAnd.hs \
	PrimIntegerEq.hs PrimIntegerFromInt.hs PrimIntegerLe.hs \
	PrimIntegerLt.hs PrimIntegerGe.hs PrimIntegerGt.hs \
	PrimIntegerMul.hs PrimIntegerNe.hs PrimIntegerNeg.hs PrimIntegerOr.hs \
	PrimIntegerQuotRem.hs PrimIntegerSub.hs  Ieee.hs


#	PrimSystem.hs PrimGetEnv.hs PrimGetProgName.hs \
#	CHGetChar.hs PrimEqHandlePosn.hs CHPutChar.hs PrimHClose.hs \
# 	PrimHFileSize.hs PrimHFlush.hs PrimHGetBuffering.hs PrimHGetChar.hs \
#	PrimHGetContents.hs PrimHGetPosn.hs PrimHIsEOF.hs PrimHPutChar.hs \
#	PrimHSeek.hs PrimHSetBuffering.hs PrimHSetPosn.hs \
#	PrimOpenFile.hs \

SRCS_TRICKY	= \
	_Driver.hs CString.hs LowI.hs LowPS.hs LowSystem.hs
#	LowIO.hs


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
OBJS_TRICKY	= $(patsubst %.hs, ${OBJDIR}/%.$O, ${SRCS_TRICKY})
CFILES_TRICKY	= $(patsubst %.hs, %.c, ${SRCS_TRICKY})
extra:  ${OBJS_TRICKY}
cfiles: ${CFILES_TRICKY}
${OBJS_TRICKY}: ${OBJDIR}/%.$O : %.hs
	$(HC) -c  $(LIB_FLAGS) -o $@ $<
${CFILES_TRICKY}: %.c : %.hs
	$(HC) -C  $(LIB_FLAGS) $<

cleanExtraC:
	rm -f ${CFILES_TRICKY}


# Here are the dependencies.
${OBJDIR}/PrimError.$O: PrimError.hs  ${OBJDIR}/CExitWith.$O
${OBJDIR}/PrimExitWith.$O: PrimExitWith.hs ${OBJDIR}/../PreludeIO/DIO.$O ${OBJDIR}/CExitWith.$O
${OBJDIR}/PrimGetArgs.$O: PrimGetArgs.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimGetEnv.$O: PrimGetEnv.hs  ${OBJDIR}/CString.$O  ${OBJDIR}/../PreludeIO/DIO.$O ${OBJDIR}/../PreludeIO/DIOError.$O
${OBJDIR}/PrimGetProgName.$O: PrimGetProgName.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHClose.$O: PrimHClose.hs   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHFileSize.$O: PrimHFileSize.hs   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHFlush.$O: PrimHFlush.hs   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHGetBuffering.$O: PrimHGetBuffering.hs   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHGetChar.$O: PrimHGetChar.hs   ${OBJDIR}/../PreludeIO/DIO.$O  ${OBJDIR}/../PreludeIO/DIOError.$O  ${OBJDIR}/CHGetChar.$O
${OBJDIR}/PrimHGetContents.$O: PrimHGetContents.hs  ${OBJDIR}/../PreludeIO/DIO.$O  ${OBJDIR}/CHGetChar.$O
${OBJDIR}/PrimHGetPosn.$O: PrimHGetPosn.hs   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHIsEOF.$O: PrimHIsEOF.hs  ${OBJDIR}/../PreludeIO/DIOError.$O   ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHPutChar.$O: PrimHPutChar.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHPutChar.$O: PrimHPutChar.hs  ${OBJDIR}/CHPutChar.$O
${OBJDIR}/PrimHSeek.$O: PrimHSeek.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHSetBuffering.$O: PrimHSetBuffering.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimHSetPosn.$O: PrimHSetPosn.hs  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimIntegerGe.$O: PrimIntegerGe.hs  ${OBJDIR}/PrimIntegerLt.$O
${OBJDIR}/PrimIntegerGt.$O: PrimIntegerGt.hs  ${OBJDIR}/PrimIntegerLe.$O
${OBJDIR}/PrimIntegerNe.$O: PrimIntegerNe.hs  ${OBJDIR}/PrimIntegerEq.$O
${OBJDIR}/PrimOpenFile.$O: PrimOpenFile.hs ${OBJDIR}/CString.$O  ${OBJDIR}/../PreludeIO/DIO.$O
${OBJDIR}/PrimSystem.$O: PrimSystem.hs  ${OBJDIR}/CString.$O  ${OBJDIR}/../PreludeIO/DIOError.$O  ${OBJDIR}/../PreludeIO/DIO.$O  #${OBJDIR}/../PreludeIO/DErrNo.$O
${OBJDIR}/PrimOpenSocket.$O: PrimOpenSocket.hs ${OBJDIR}/../IO/DSocket.$O ${OBJDIR}/CString.$O ${OBJDIR}/../PreludeIO/DIO.$O

${OBJDIR}/../PreludeIO/MsgIOError.$O:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.$O
${OBJDIR}/../PreludeIO/DIOError.$O:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIOError.$O
${OBJDIR}/../PreludeIO/DIO.$O:
	cd ../PreludeIO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/PreludeIO/DIO.$O
#${OBJDIR}/../DErrNo/DErrNo.$O:
#	cd ../$(MACHINE); ${MAKE} ${BUILDDIR}/${OBJ}/prelude/DErrNo/DErrNo.$O
${OBJDIR}/../IO/DSocket.$O:
	cd ../IO; ${MAKE} ${BUILDDIR}/${OBJ}/prelude/IO/DSocket.$O

${OBJDIR}/_Driver.$O: _Driver.hs ${OBJDIR}/../PreludeIO/DIO.$O


# And now for the C-files dependencies.
PrimError.c:          CExitWith.c
PrimExitWith.c:       ../PreludeIO/DIO.c CExitWith.c
PrimGetArgs.c:        ../PreludeIO/DIO.c
PrimGetEnv.c:         CString.c  ../PreludeIO/DIO.c ../PreludeIO/DIOError.c
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

