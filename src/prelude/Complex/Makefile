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
SRCS_T	= ${SRCS}


# Here are the main rules.
include ../Makefile.common



# Here are the dependencies.
${OBJDIR}/Cis.$O: Cis.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Conjugate.$O: Conjugate.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Eq_Complex.$O: Eq_Complex.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Floating_Complex.$O: Floating_Complex.hs ${OBJDIR}/DComplex.$O ${OBJDIR}/Magnitude.$O ${OBJDIR}/Phase.$O ${OBJDIR}/Fractional_Complex.$O ${OBJDIR}/Num_Complex.$O
${OBJDIR}/Fractional_Complex.$O: Fractional_Complex.hs ${OBJDIR}/DComplex.$O ${OBJDIR}/Num_Complex.$O
${OBJDIR}/ImagPart.$O: ImagPart.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Magnitude.$O: Magnitude.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/MkPolar.$O: MkPolar.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Num_Complex.$O: Num_Complex.hs ${OBJDIR}/DComplex.$O ${OBJDIR}/Magnitude.$O ${OBJDIR}/Eq_Complex.$O ${OBJDIR}/Show_Complex.$O
${OBJDIR}/Phase.$O: Phase.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Read_Complex.$O: Read_Complex.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/RealPart.$O: RealPart.hs ${OBJDIR}/DComplex.$O
${OBJDIR}/Show_Complex.$O: Show_Complex.hs ${OBJDIR}/DComplex.$O ${OBJDIR}/RealPart.$O
${OBJDIR}/Polar.$O: Polar.hs ${OBJDIR}/DComplex.$O ${OBJDIR}/Magnitude.$O  ${OBJDIR}/Phase.$O


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

