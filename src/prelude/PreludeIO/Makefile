include Makefile.inc

THISLIB = PreludeIO

SRCS	= DErrNo.hs \
	Eq_IOError.hs    Monad_IO.hs      ReadLn.hs 	\
	AppendFile.hs    Print.hs         Show_IOError.hs \
	Catch.hs         Functor_IO.hs    PutStrLn.hs      TFilePath.hs    \
	DIO.hs           GetLine.hs       ReadFile.hs      UserError.hs    \
	DIOError.hs      Interact.hs      ReadIO.hs        WriteFile.hs    \
	PutChar.hs PutStr.hs GetChar.hs GetContents.hs     IoError.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -P${LOW} -P./ -part -prelude -redefine
#HEAP = +RTS -H2M -RTS


all:  ${OBJS}  ${MAINOBJ}
boot: ${OBJS}  ${MAINOBJ}
cfiles: ${CFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) ${CFILES} ${MAINCFILE}
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f *.hi ${OBJDIR}/*.o *.o *.c *.s
 


.SUFFIXES: .hs .o .c

${OBJS} ${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<

${CFILES} ${MAINCFILE}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<

#../Prelude/%.hi : ../Prelude/%.hs
#	cd ../Prelude; ${MAKE} $@


# I know that they depoend on .hi, but make is stupid :-(
${OBJDIR}/Catch.o: Catch.hs ${OBJDIR}/DIO.o
${OBJDIR}/Eq_IOError.o: Eq_IOError.hs ${OBJDIR}/DIOError.o
${OBJDIR}/Fail.o: Fail.hs ${OBJDIR}/DIO.o
${OBJDIR}/Monad_IO.o: Monad_IO.hs ${OBJDIR}/DIO.o ${OBJDIR}/DIOError.o # ../Prelude/CMonad.hi
${OBJDIR}/Functor_IO.o: Functor_IO.hs # ../Prelude/CFunctor.hi
${OBJDIR}/Show_IOError.o: Show_IOError.hs ${OBJDIR}/DIOError.o
${OBJDIR}/UserError.o: UserError.hs ${OBJDIR}/DIOError.o


# And now for C-files dependencies.
Catch.c:        DIO.c
Eq_IOError.c:   DIOError.c
Fail.c:         DIO.c
Monad_IO.c:     DIO.c DIOError.c # ../Prelude/CMonad.hi
Functor_IO.c:   # ../Prelude/CFunctor.hi
Show_IOError.c: DIOError.c
UserError.c:    DIOError.c

