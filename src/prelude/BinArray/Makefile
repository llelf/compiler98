include Makefile.inc

THISLIB = BinArray

HSSRCS  = UnboxedArray.hs
GCSRCS  = AllocUBA.gc DUnboxedArray.gc RUBA.gc GetUBAEnd.gc Binary_UBA.gc \
          GetUBAFree.gc WUBA.gc
CSRCS   = cLowUnboxedArray.c


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC	= ${THISLIB}.hs

HSOBJS = $(patsubst %.hs,${OBJDIR}/%.o,${HSSRCS})
GCOBJS = $(patsubst %.gc,${OBJDIR}/%.o,${GCSRCS})
COBJS = $(patsubst %.c,${OBJDIR}/%.o,${CSRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

OBJS = ${GCOBJS} ${HSOBJS} ${COBJS}

HSCFILES = $(patsubst %.hs,%.c,${HSSRCS})
GCCFILES = $(patsubst %.gc,%.c,${GCSRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

BIN = ../Binary/
HFLAGS = -part
CFLAGS = -I$(INCDIR)
CC = gcc
 
all: ${OBJS} ${MAINOBJ}
boot:
cfiles: ${HSCFILES} ${GCCFILES} ${MAINCFILE}
fromC:
	$(HC) -c -d $(OBJDIR) -I$(BIN) *.c
${OBJDIR}:
	mkdir -p ${OBJDIR}
objdir: ${OBJDIR}
${MAINOBJ}: ${OBJS}
clean:
	-rm -f ${OBJDIR}/*.o *.hi *.o *_.hs *_.c ${HSCFILES} ${GCCFILES} ${MAINCFILE}



.SUFFIXES: .c .hs .gc .o

#.hs.o:
#	$(HC) -c $(HFLAGS) -P$(BIN) $<
#.gc.o:
#	$(HC) -c $(HFLAGS) -P$(BIN) $<
#.c.o:
#	$(CC) -c $(CFLAGS) $<
##.hs.c:
##	$(HC) -C $(HFLAGS) -P$(BIN) $<

${HSOBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -P$(BIN) -o $@ $<
${GCOBJS}: ${OBJDIR}/%.o : %.gc
	${HC} ${HFLAGS} -I$(INCDIR)/ -P$(BIN) -I$(BIN) -c -o $@ $<
${COBJS}: ${OBJDIR}/%.o : %.c
	$(CC) -c ${CFLAGS} -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -o $@ $<

${HSCFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) -P$(BIN) $<
${GCCFILES}: %.c : %.gc
	${HC} -C ${HFLAGS} -P$(BIN) -I$(BIN) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) $(HFLAGS) -P$(BIN) +CTS -lib -CTS $<



# I know that they depend on .hi, but make is stupid :-(
${OBJS}: cLowUnboxedArray.h

${OBJDIR}/AllocUBA.o: AllocUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/Binary_UBA.o: Binary_UBA.gc  ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/DUnboxedArray.o: DUnboxedArray.gc
${OBJDIR}/GetUBAEnd.o: GetUBAEnd.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/GetUBAFree.o: GetUBAFree.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/RUBA.o: RUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/WUBA.o: WUBA.gc ${OBJDIR}/DUnboxedArray.o
${OBJDIR}/UnboxedArray.o: UnboxedArray.hs ${OBJDIR}/DUnboxedArray.o ${OBJDIR}/AllocUBA.o ${OBJDIR}/Binary_UBA.o ${OBJDIR}/GetUBAEnd.o ${OBJDIR}/GetUBAFree.o ${OBJDIR}/RUBA.o ${OBJDIR}/WUBA.o#
${OBJDIR}/cLowUnboxedArray.o: cLowUnboxedArray.c
${OBJDIR}/BinArray.o: BinArray.hs ${OBJDIR}/UnboxedArray.o ${OBJS}


# C-files dependencies.
AllocUBA.c:     DUnboxedArray.c
Binary_UBA.c:   DUnboxedArray.c
GetUBAEnd.c:    DUnboxedArray.c
GetUBAFree.c:   DUnboxedArray.c
RUBA.c:         DUnboxedArray.c
WUBA.c:         DUnboxedArray.c
UnboxedArray.c: DUnboxedArray.c AllocUBA.c Binary_UBA.c GetUBAEnd.c GetUBAFree.c RUBA.c WUBA.c
BinArray.c:     UnboxedArray.c 

