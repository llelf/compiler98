include Makefile.inc

THISLIB = Complex

SRCS	= \
	ImagPart.hs            Read_Complex.hs \
	Cis.hs                 Magnitude.hs           Floating_Complex.hs \
	Conjugate.hs           MkPolar.hs             RealPart.hs \
	DComplex.hs            Num_Complex.hs         Show_Complex.hs \
	Eq_Complex.hs          Phase.hs               \
	Fractional_Complex.hs  Polar.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

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


 
.SUFFIXES: .hs .c .o
.hs.o:
	$(HC) -c $(HFLAGS) $<
.hs.c:
	$(HC) -C $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -CTS -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -CTS $<


# I know that they depend on .hi, but make is stupid :-(
${OBJDIR}/Cis.o: Cis.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Conjugate.o: Conjugate.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Eq_Complex.o: Eq_Complex.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Floating_Complex.o: Floating_Complex.hs ${OBJDIR}/DComplex.o ${OBJDIR}/Magnitude.o ${OBJDIR}/Phase.o ${OBJDIR}/Fractional_Complex.o ${OBJDIR}/Num_Complex.o
${OBJDIR}/Fractional_Complex.o: Fractional_Complex.hs ${OBJDIR}/DComplex.o ${OBJDIR}/Num_Complex.o
${OBJDIR}/ImagPart.o: ImagPart.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Magnitude.o: Magnitude.hs ${OBJDIR}/DComplex.o
${OBJDIR}/MkPolar.o: MkPolar.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Num_Complex.o: Num_Complex.hs ${OBJDIR}/DComplex.o ${OBJDIR}/Magnitude.o ${OBJDIR}/Eq_Complex.o ${OBJDIR}/Show_Complex.o
${OBJDIR}/Phase.o: Phase.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Read_Complex.o: Read_Complex.hs ${OBJDIR}/DComplex.o
${OBJDIR}/RealPart.o: RealPart.hs ${OBJDIR}/DComplex.o
${OBJDIR}/Show_Complex.o: Show_Complex.hs ${OBJDIR}/DComplex.o ${OBJDIR}/RealPart.o
${OBJDIR}/Polar.o: Polar.hs ${OBJDIR}/DComplex.o ${OBJDIR}/Magnitude.o  ${OBJDIR}/Phase.o


# C-files dependencies.
Cis.c:        DComplex.c
Conjugate.c:  DComplex.c
Eq_Complex.c: DComplex.c
ImagPart.c:   DComplex.c
Magnitude.c:  DComplex.c
MkPolar.c:    DComplex.c
Phase.c:      DComplex.c
RealPart.c:   DComplex.c
Polar.c:      DComplex.c Magnitude.c  Phase.c

Floating_Complex.c:   DComplex.c Magnitude.c Phase.c Fractional_Complex.c Num_Complex.c
Fractional_Complex.c: DComplex.c Num_Complex.c
Num_Complex.c:        DComplex.c Magnitude.c Eq_Complex.c Show_Complex.c
Read_Complex.c:       DComplex.c
Show_Complex.c:       DComplex.c RealPart.c
