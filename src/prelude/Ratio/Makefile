include Makefile.inc

THISLIB = Ratio

SRCS	= \
        DRatio.hs            TRational.hs \
        Eq_Ratio.hs          Ord_Ratio.hs         Show_Ratio.hs \
        Num_Ratio.hs         Read_Ratio.hs        Real_Ratio.hs \
        Fractional_Ratio.hs  RealFrac_Ratio.hs    Enum_Ratio.hs \
	ApproxRational.hs    RatioCon.hs \
	Denominator.hs       Numerator.hs \
        Prec.hs              Reduce.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = PrelRatio.hs ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

HFLAGS = -part -redefine -prelude # -P../Prelude/ 
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
	rm -f $(OBJDIR)/*.o *.hi *.o *.c *.s


 
.SUFFIXES: .hs .o .c
.hs.o:
	$(HC) -c $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(NHEAP) +CTS -lib -redefine -CTS -o $@ $<
# removed -P../Prelude/ from above compile line

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C $(NHEAP) +CTS -lib -redefine -CTS $<
# removed -P../Prelude/ from above compile line

../Prelude/%.hi : ../Prelude/%.hs
	cd ../Prelude; ${MAKE} $@


# I know that they depend on .hi, but make is stupid :-(

${OBJDIR}/Fractional_Ratio.o : Fractional_Ratio.hs ${OBJDIR}/RatioCon.o
${OBJDIR}/Num_Ratio.o : Num_Ratio.hs ${OBJDIR}/Reduce.o
${OBJDIR}/RatioCon.o : RatioCon.hs ${OBJDIR}/Reduce.o
${OBJDIR}/Show_Ratio.o : Show_Ratio.hs ${OBJDIR}/Prec.o
${OBJDIR}/Read_Ratio.o : Read_Ratio.hs ${OBJDIR}/Prec.o ${OBJDIR}/RatioCon.o
${OBJDIR}/Real_Ratio.o : Real_Ratio.hs
${OBJDIR}/Enum_Ratio.o : Enum_Ratio.hs


Fractional_Ratio.c : Fractional_Ratio.hs RatioCon.c
Num_Ratio.c : Num_Ratio.hs Reduce.c
RatioCon.c : RatioCon.hs Reduce.c
Show_Ratio.c : Show_Ratio.hs Prec.c
Read_Ratio.c : Read_Ratio.hs Prec.c RatioCon.c
