include Makefile.inc

THISLIB = GreenCard

SRCS	=


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC	= ${THISLIB}.gc

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.gc,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.gc,%.c,${MAINSRC})

LOW = ../LowB/
IO = ../PreludeIO/
HFLAGS = -P./ -part -redefine


all: ${OBJS} ${MAINOBJ}
boot:
cfiles: ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) *.c
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	rm -f ${OBJDIR}/*.o *.hi *.o *.c *.s *_.hs


 
.SUFFIXES: .hs .o .gc .c
.hs.o:
	$(HC) -P$(LOW) -P$(IO) -c $(HFLAGS) $<
.gc.o:
	$(HC) -P$(LOW) -P$(IO) -c $(HFLAGS) $<
${MAINOBJ}: ${OBJDIR}/%.o : %.gc
	$(HC) -c -P$(LOW) -P$(IO) $(HFLAGS) +CTS -lib -CTS -o $@ $<
${MAINCFILE}: %.c : %.gc
	$(HC) -C -P$(LOW) -P$(IO) $(HFLAGS) +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(
