include Makefile.inc

OBJDIR=${BUILDDIR}/${OBJ}/runtime/Mk

SRCS_PROG = \
	mkBlockBuffering.c        mkIOErrorHFileSize.c      mkIOErrorHSetBuffering.c  mkMkArray.c               mkTuple4.c  \
	mkComplex.c               mkIOErrorHFlush.c         mkIOErrorHSetPosn.c       mkRatio.c                 mkTuple5.c  \
	mkCons.c                  mkIOErrorHGetBuffering.c  mkIOErrorOpen.c           mkRight.c                 mkTuple6.c  \
	mkExitFailure.c           mkIOErrorHGetPosn.c       mkIOErrorUser.c           mkTuple10.c               mkTuple7.c  \
	mkIO.c                    mkIOErrorHIsEOF.c         mkJust.c                  mkTuple2.c                mkTuple8.c  \
	mkIOErrorEOF.c            mkIOErrorHSeek.c          mkLeft.c                  mkTuple3.c                mkTuple9.c

SRCS_HAND = \
	mkHandlePosn.c   \
	mkSmallIntegerU.c mkInt.c mkFloat.c mkDouble.c \
	mkPackedString.c \
	mkForeign.c       mkStablePtr.c \
	mkCInt.c

OBJS_PROG = $(patsubst %.c,${OBJDIR}/%.o,${SRCS_PROG})
OBJS_HAND = $(patsubst %.c,${OBJDIR}/%.o,${SRCS_HAND})

OBJS = ${OBJS_PROG} ${OBJS_HAND}

DATA2C = ../../data2c/data2c

CFLAGS= -I../Kernel -I${INCDIR} ${DCFG} ${OPT} ${ENDIAN}

${OBJS}: ${OBJDIR}/%.o: %.c
	${CC} -c ${CFLAGS} -o $@ $<

LINKS = mk.h mkByHand.h mkByProg.h

links:
	@for f in ${LINKS}; do \
		ln -s ${INCDIR}/$$f $$f >/dev/null 2>&1 || echo "Link $$f already exists."; \
	done

all: links ${OBJS}

${OBJDIR}:
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

rebuild:
	$(DATA2C) ../../prelude/Prelude/D2.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D3.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D4.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D5.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D6.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D7.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D8.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D9.hs	>> log
	$(DATA2C) ../../prelude/Prelude/D10.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DBool.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DEither.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DList.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DMaybe.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DOrdering.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DRatio.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DUnit.hs	>> log
	$(DATA2C) ../../prelude/Prelude/DVoid.hs	>> log
	$(DATA2C) ../../prelude/PreludeIO/DIO.hs	>> log
	$(DATA2C) ../../prelude/PreludeIO/DIOError.hs	>> log
	$(DATA2C) ../../prelude/Array/DArray.hs   	>> log
	$(DATA2C) ../../prelude/Complex/DComplex.hs	>> log
	$(DATA2C) ../../prelude/IO/BufferMode.hs	>> log
	$(DATA2C) ../../prelude/IO/IOMode.hs		>> log
	$(DATA2C) ../../prelude/IO/SeekMode.hs		>> log
	$(DATA2C) ../../prelude/IO/DSocket.hs		>> log
	$(DATA2C) ../../prelude/System/DExitCode.hs	>> log

clean:
	rm -f ${OBJDIR}/*.o
