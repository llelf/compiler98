include Makefile.inc

THISLIB = IO

SRCS	= \
	HFlush.hs         HPrint.hs        HSetPosn.hs \
        IsEOF.hs          SeekMode.hs      BufferMode.hs \
        HGetChar.hs       HPutChar.hs      DHandlePosn.hs \
        IsEOFError.hs     Eq_HandlePosn.hs HGetContents.hs \
        HPutStr.hs        IOMode.hs        IsUserError.hs   \
	HClose.hs         HGetPosn.hs      HSeek.hs \
        IoeGetFileName.hs OpenFile.hs      HFileSize.hs \
        HIsEOF.hs         HSetBuffering.hs IoeGetHandle.hs \
        HGetBuffering.hs  HGetFileName.hs  HPutStrLn.hs \
	NotDoneYet.hs     DSocket.hs       OpenSocket.hs \
	IoeGetErrorString.hs	HGetLine.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

LOW = ../LowB/
HFLAGS = -part
#HEAP = +RTS -H2M -RTS


all:  ${OBJS} ${MAINOBJ}
boot: ${OBJS} ${MAINOBJ}
cfiles: ${CFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${MAINCFILE}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s


 
.SUFFIXES: .hs .o .c
.hs.o:
	$(HC) -P../PreludeIO/ -P$(LOW) -c $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c -P../PreludeIO/ -P$(LOW) $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -P$(LOW) -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C -P../PreludeIO/ -P$(LOW) $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS -P$(LOW) $<


# I know that they depend on .hi, but make is stupid :-(

${OBJDIR}/Eq_HandlePosn.o: Eq_HandlePosn.hs ${OBJDIR}/DHandlePosn.o

Eq_HandlePosn.c: DHandlePosn.c

