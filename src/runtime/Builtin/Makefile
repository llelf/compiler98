include Makefile.inc

VPATH = $(INCDIR)

OBJDIR=${BUILDDIR}/${OBJ}/runtime/Builtin

SRCS = \
	primDecodeDouble.c       primEncodeDouble.c	\
	primFloatFromInteger.c   primIntegerFromInt.c      \
	primDecodeFloat.c        primEncodeFloat.c	\
	primDoubleFromInteger.c              primIntFromInteger.c \
	primPackString.c  lengthPS.c unpackPS.c indexPS.c primComparePS.c \
	cOpen.c cHClose.c cHIsEOF.c cHFileSize.c \
	cHFlush.c cHGetPosn.c cHSetPosn.c cHSeek.c \
	cHGetBuffering.c cHSetBuffering.c \
	primEqHandlePosn.c \
	cGetProgName.c cGetEnv.c cGetArgs.c cProfile.c cExitWith.c cSystem.c \
	cEval.c \
	cOpenSocket.c \
	primVector.c primIndex.c \
        cBits.c  cDoesFileExist.c

LINKS = haskell2c.h twopass.h

links:
	@for f in ${LINKS}; do \
		ln -s ${INCDIR}/$$f $$f >/dev/null 2>&1 || echo "Link $$f already exists."; \
	done

OBJS = $(patsubst %.c,${OBJDIR}/%.o,${SRCS})

CFLAGS= -I../Kernel -I${INCDIR} ${DCFG} ${BUGFIX} ${OPT} 

${OBJS}: ${OBJDIR}/%.o: %.c
	${CC} -c ${CFLAGS} -o $@ $<

all: links ${OBJS}

${OBJDIR}:
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

clean:
	rm -f ${OBJDIR}/*.o

primDecodeDouble.o: primDecodeDouble.c haskell2c.h
primDecodeFloat.o: primDecodeFloat.c haskell2c.h
primDoubleFromInteger.o: primDoubleFromInteger.c haskell2c.h
primEncodeDouble.o: primEncodeDouble.c  haskell2c.h
primEncodeFloat.o:primEncodeFloat.c  haskell2c.h
primFloatFromInteger.o: primFloatFromInteger.c haskell2c.h
primGetChar.o: primGetChar.c haskell2c.h
primIntFromInteger.o: primIntFromInteger.c haskell2c.h
primIntegerFromInt.o: primIntegerFromInt.c haskell2c.h
primPutChar.o: primPutChar.c haskell2c.h
primSeq.o: primSeq.c haskell2c.h
cExitWith.o: cExitWith.c haskell2c.h
cGetEnv.o: cGetEnv.c haskell2c.h
cHFileSize.o: cHFileSize.c haskell2c.h
cHGetBuffering.o: cHGetBuffering.c haskell2c.h
cHIsEOF.o: cHIsEOF.c haskell2c.h
cHSetBuffering.o: cHSetBuffering.c haskell2c.h
cOpen.o: cOpen.c haskell2c.h
cProgName.o: cProgName.c haskell2c.h
cGetArgs.o: cGetArgs.c haskell2c.h
cHClose.o: cHClose.c haskell2c.h
cHFlush.o: cHFlush.c haskell2c.h
cHGetPosn.o: cHGetPosn.c haskell2c.h
cHSeek.o: cHSeek.c haskell2c.h
cHSetPosn.o: cHSetPosn.c haskell2c.h
cProfile.o: cProfile.c haskell2c.h
cSystem.o: cSystem.c haskell2c.h
cOpenSocket.o: cOpenSocket.c haskell2c.h
cBits.o: cBits.c haskell2c.h
cDoesFileExist.o: cDoesFileExist.c haskell2c.h
