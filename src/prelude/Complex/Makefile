include Makefile.inc

THISLIB	= Complex
SEARCH	=
EXTRA_H_FLAGS	=
EXTRA_C_FLAGS	=

SRCS	= \
	ImagPart.hs            Read_Complex.hs \
	Cis.hs                 Magnitude.hs           Floating_Complex.hs \
	Conjugate.hs           MkPolar.hs             RealPart.hs \
	DComplex.hs            Num_Complex.hs         Show_Complex.hs \
	Eq_Complex.hs          Phase.hs               \
	Fractional_Complex.hs  Polar.hs
SRCS_T	=


# Here are the main rules.
include ../Makefile.common



# Here are the dependencies.
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

