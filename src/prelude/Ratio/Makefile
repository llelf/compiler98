include Makefile.inc

THISLIB	= Ratio
SEARCH	=

SRCS	= \
        DRatio.hs            TRational.hs \
        Eq_Ratio.hs          Ord_Ratio.hs         Show_Ratio.hs \
        Num_Ratio.hs         Read_Ratio.hs        Real_Ratio.hs \
        Fractional_Ratio.hs  RealFrac_Ratio.hs    Enum_Ratio.hs \
	ApproxRational.hs    RatioCon.hs \
	Denominator.hs       Numerator.hs \
        Prec.hs              Reduce.hs
SRCS_T	= ${SRCS}


# Here are the main rules.
include ../Makefile.common


# Here are some extra rules.
../Prelude/%.hi : ../Prelude/%.hs
	cd ../Prelude; ${MAKE} $@


# Here are the dependencies.

${OBJDIR}/Fractional_Ratio.$O : Fractional_Ratio.hs ${OBJDIR}/RatioCon.$O
${OBJDIR}/Num_Ratio.$O : Num_Ratio.hs ${OBJDIR}/Reduce.$O
${OBJDIR}/RatioCon.$O : RatioCon.hs ${OBJDIR}/Reduce.$O
${OBJDIR}/Show_Ratio.$O : Show_Ratio.hs ${OBJDIR}/Prec.$O
${OBJDIR}/Read_Ratio.$O : Read_Ratio.hs ${OBJDIR}/Prec.$O ${OBJDIR}/RatioCon.$O
${OBJDIR}/Real_Ratio.$O : Real_Ratio.hs
${OBJDIR}/Enum_Ratio.$O : Enum_Ratio.hs


Fractional_Ratio.c : Fractional_Ratio.hs RatioCon.c
Num_Ratio.c : Num_Ratio.hs Reduce.c
RatioCon.c : RatioCon.hs Reduce.c
Show_Ratio.c : Show_Ratio.hs Prec.c
Read_Ratio.c : Read_Ratio.hs Prec.c RatioCon.c

