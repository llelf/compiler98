include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/PreludeIO

VPATH = .

.SUFFIXES: .hi .hs .o

SRCS= DIO.hs DIOError.hs GetChar.hs GetContents.hs Interact.hs \
	  Monad_IO.hs PutChar.hs PutStr.hs PutStrLn.hs Fail.hs UserError.hs \
	  TFilePath.hs WriteFile.hs Catch.hs GetLine.hs Print.hs \
	Show_IOError.hs

NTSRCS= StdIO.hs

NOTDONE= AppendFile.hs Eq_IOError.hs Functor_IO.hs \
	 ReadFile.hs ReadIO.hs ReadLn.hs \
	 Show_IOError.hs

MAINSRC= PreludeIO.hs
MAINOBJ=$(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

OBJS=$(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
NTOBJS=$(patsubst %.hs,${OBJDIR}/%.o,${NTSRCS})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

$(NTOBJS): ${OBJDIR}/%.o: %.hs
	$(COMPILE) -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${NTOBJS} ${MAINSRC}
	$(DBG_COMPILE) -o $@ ${MAINSRC}

install:

clean:
	rm -f ${OBJS} ${NTOBJS} ${MAINOBJ}
