include Makefile.inc

THISLIB = Prelude

#SRCS	= \
#	Sequence.hs             Enum_Int.hs             Num_Float.hs 	\
#	AndAnd.hs               Enum_Integer.hs         Num_Int.hs 	\
#	ApplyM.hs               Enum_Ordering.hs        Num_Integer.hs 	\
#	AsTypeOf.hs             Enum_Unit.hs            NumericEnumFrom.hs 	\
#	Eq_2.hs                 NumericEnumFromThen.hs 	\
#	Bounded_2.hs            Eq_3.hs                 Odd.hs 	\
#	Bounded_3.hs            Eq_4.hs                 OrOr.hs 	\
#	Bounded_Bool.hs         Eq_Bool.hs              Ord_2.hs 	\
#	Bounded_Char.hs         Eq_Char.hs              Ord_3.hs 	\
#	Bounded_Int.hs          Eq_Double.hs            Ord_4.hs 	\
#	Bounded_Ordering.hs     Eq_Either.hs            Ord_Bool.hs 	\
#	Bounded_Unit.hs         Eq_Float.hs             Ord_Char.hs 	\
#	CBounded.hs             Eq_Int.hs               Ord_Double.hs 	\
#	CEnum.hs                Eq_Integer.hs           Ord_Either.hs 	\
#	CEq.hs                  Eq_List.hs              Ord_Float.hs 	\
#	Eq_Maybe.hs             Ord_Int.hs 	\
#	CFloating.hs            Eq_Ordering.hs          Ord_Integer.hs 	\
#	CFractional.hs          Eq_Unit.hs              Ord_List.hs 	\
#	CFunctor.hs             Error.hs                Ord_Maybe.hs 	\
#	CIntegral.hs            Ord_Ordering.hs 	\
#	CIx.hs                  Ord_Unit.hs 	\
#	CMonad.hs               Otherwise.hs 	\
#	CRealFloat.hs           Flip.hs                 RealFrac_Float.hs 	\
#	CRealFrac.hs            Floating_Double.hs      Real_Double.hs 	\
#	CNum.hs                 RealFloat_Double.hs 	\
#	COrd.hs                 Even.hs                 RealFloat_Float.hs 	\
#	CReal.hs                Filter.hs               RealFrac_Double.hs 	\
#	Compose.hs              Floating_Float.hs       Real_Float.hs 	\
#	Concat.hs               FractionalRaise.hs      Real_Int.hs 	\
#	Const.hs                Fractional_Double.hs    Real_Integer.hs 	\
#	Curry.hs                Fractional_Float.hs     Sequence_.hs 	\
#	D10.hs D11.hs D12.hs      FromIntegral.hs         Snd.hs 	\
#	D2.hs                   FromRealFrac.hs          	\
#	D3.hs                   Fst.hs                  Subtract.hs 	\
#	D4.hs                   Functor_List.hs \
#	D5.hs                   Functor_Maybe.hs        TRational.hs 	\
#	D6.hs                   Gcd.hs                  TString.hs 	\
#	D7.hs                   Guard.hs                Uncurry.hs 	\
#	D8.hs                   Id.hs                   Undefined.hs 	\
#	D9.hs                   Integral_Int.hs         Unless.hs 	\
#	DBool.hs                Integral_Integer.hs     Until.hs 	\
#	DEither.hs              Lcm.hs                  When.hs 	\
#	DList.hs		_EnumFromThen.hs 	\
#	DMaybe.hs               MapM.hs                 _EnumFromThenTo.hs 	\
#	DOrdering.hs            MapM_.hs                _EnumFromTo.hs 	\
#	Maybe.hs                _EnumFromToDec.hs 	\
#	DUnit.hs                _EnumFromToInc.hs 	\
#	DVoid.hs                _EnumInRange.hs 	\
#	Dollar.hs               _EnumIndex.hs 	\
#	Done.hs                 _EnumRange.hs 	\
#	Either.hs               Monad_List.hs           _EqInteger.hs 	\
#	Enum_Bool.hs            Monad_Maybe.hs          _Filter.hs 	\
#	Enum_Char.hs            Not.hs                  _Foldr.hs 	\
#	Enum_Double.hs          NumRaise.hs             _FromEnum.hs 	\
#	Enum_Float.hs           Num_Double.hs           _ToEnum.hs \
#	_TupleIndex.hs	       _TupleRange.hs \
#	_Zap_Stack.hs _Zap_Arg.hs \
#	_Apply1.hs	_Apply2.hs	_Apply3.hs	_Apply4.hs

SRCS = \
	AndAnd.hs       ApplyM.hs       AsTypeOf.hs         Seq.hs \
	CBounded.hs     Bounded_2.hs    Bounded_3.hs        Bounded_Bool.hs \
	Bounded_Char.hs Bounded_Int.hs  Bounded_Ordering.hs Bounded_Unit.hs \
	CEnum.hs        CEq.hs          CFloating.hs        CFractional.hs \
        CFunctor.hs     CIntegral.hs    CIx.hs              CMonad.hs \
        CNum.hs         COrd.hs         CReal.hs            CRealFloat.hs \
        CRealFrac.hs    Compose.hs      Const.hs            Curry.hs \
        D10.hs          D11.hs          D12.hs              D2.hs \
        D3.hs           D4.hs           D5.hs               D6.hs \
        D7.hs           D8.hs           D9.hs               DBool.hs \
        DEither.hs      DList.hs        DMaybe.hs           DOrdering.hs \
        DUnit.hs        Dollar.hs       DollarBang.hs       Either.hs \
        Enum_Bool.hs    Enum_Char.hs    Enum_Double.hs      Enum_Float.hs \
        Enum_Int.hs     Enum_Integer.hs Enum_Ordering.hs    Enum_Unit.hs \
        Eq_2.hs         Eq_3.hs         Eq_4.hs             Eq_Bool.hs \
        Eq_Char.hs      Eq_Double.hs    Eq_Either.hs        Eq_Float.hs \
        Eq_Int.hs       Eq_Integer.hs   Eq_List.hs          Eq_Maybe.hs \
        Eq_Ordering.hs  Eq_Unit.hs      Error.hs            Even.hs \
        Flip.hs         Floating_Double.hs                  Floating_Float.hs \
        FractionalRaise.hs              Fractional_Double.hs \
        Fractional_Float.hs             FromIntegral.hs     RealToFrac.hs \
        Fst.hs          Functor_List.hs Functor_Maybe.hs    Gcd.hs \
        Id.hs           Integral_Int.hs Integral_Integer.hs Lcm.hs \
        Sequence.hs     Sequence_.hs \
        MapM.hs         MapM_.hs        Maybe.hs            Monad_List.hs \
        Monad_Maybe.hs  Not.hs          NumRaise.hs         Num_Double.hs \
        Num_Float.hs    Num_Int.hs      Num_Integer.hs      NumericEnumFrom.hs \
        NumericEnumFromThen.hs          Odd.hs              OrOr.hs \
        Ord_2.hs        Ord_3.hs        Ord_4.hs            Ord_Bool.hs \
        Ord_Char.hs     Ord_Double.hs   Ord_Either.hs       Ord_Float.hs \
        Ord_Int.hs      Ord_Integer.hs  Ord_List.hs         Ord_Maybe.hs \
        Ord_Ordering.hs Ord_Unit.hs     Otherwise.hs \
        RealFloat_Double.hs             RealFloat_Float.hs \
        RealFrac_Double.hs              RealFrac_Float.hs \
        Real_Double.hs  Real_Float.hs   Real_Int.hs         Real_Integer.hs \
        Snd.hs          Subtract.hs \
        TString.hs      Uncurry.hs      Undefined.hs        Until.hs \
        _Apply1.hs      _Apply2.hs      _Apply3.hs          _Apply4.hs \
        _EnumFromThen.hs                _EnumFromThenTo.hs  _EnumFromTo.hs \
        _EnumFromToDec.hs               _EnumFromToInc.hs   _EnumInRange.hs \
        _EnumIndex.hs   _EnumRange.hs   _EqInteger.hs       _Filter.hs \
        _Foldr.hs       _FromEnum.hs    _ToEnum.hs          _TupleIndex.hs \
        _TupleRange.hs  _Zap_Arg.hs     _Zap_Stack.hs       _NplusK.hs

# Temporarily removed due to compilation problems.
#        Floating_Double.hs                  Floating_Float.hs \
#        Gcd.hs  Lcm.hs  NumRaise.hs


OBJDIR = ${BUILDDIR}/${OBJ}/prelude/${THISLIB}
MAINSRC = ${THISLIB}.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})
CFILES = $(patsubst %.hs,%.c,${SRCS})
MAINCFILE = $(patsubst %.hs,%.c,${MAINSRC})

LOW = ../LowB/

HFLAGS = -P$(LOW) -P../PreludeList/ -P../Ratio/ -part -prelude -redefine  


all:  ${OBJS} ${MAINOBJ}
boot: all
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
#.hs.o:
#	$(HC) -c $(HFLAGS) $<

${OBJS}: ${OBJDIR}/%.o : %.hs
	$(HC) -c $(HFLAGS) -o $@ $<
${MAINOBJ}: ${OBJDIR}/%.o : %.hs
	rm -f Prelude.hi $(OBJDIR)/Prelude.o
	$(HC) -c +CTS -lib -CTS -prelude -redefine -P../PreludeList/ -P../PreludeText/ -P../PreludeIO/ -P../Ix/ -P../Ratio/ -P$(LOW) -o $@ $<

${CFILES}: %.c : %.hs
	$(HC) -C $(HFLAGS) $<
${MAINCFILE}: %.c : %.hs
	$(HC) -C +CTS -lib -CTS -prelude -redefine -P../PreludeList/ -P../PreludeText/ -P../PreludeIO/ -P../Ix/ -P../Ratio/ -P$(LOW) $<

../Prelude/%.hi : %.hs
	$(HC) -c $(HFLAGS) -o /dev/null $<



# I know that they depoend on .hi, but make is stupid :-(

${OBJDIR}/CEnum.o: CEnum.hs ${OBJDIR}/_EnumFromTo.o  ${OBJDIR}/_EnumFromThenTo.o
${OBJDIR}/Enum_Bool.o: Enum_Bool.hs ${OBJDIR}/_EnumFromTo.o  ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/Enum_Char.o: Enum_Char.hs ${OBJDIR}/_EnumFromTo.o ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/Enum_Ordering.o: Enum_Ordering.hs ${OBJDIR}/_EnumFromTo.o ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/_EnumFromThen.o: _EnumFromThen.hs ${OBJDIR}/_EnumFromToInc.o ${OBJDIR}/_EnumFromToDec.o
${OBJDIR}/_EnumFromThenTo.o: _EnumFromThenTo.hs ${OBJDIR}/_EnumFromToInc.o ${OBJDIR}/_EnumFromToDec.o
${OBJDIR}/_EnumIndex.o: _EnumIndex.hs ${OBJDIR}/_EnumInRange.o
${OBJDIR}/_EnumRange.o: _EnumRange.hs ${OBJDIR}/_EnumFromTo.o
${OBJDIR}/_EnumFromTo.o: _EnumFromTo.hs ${OBJDIR}/_FromEnum.o ${OBJDIR}/_ToEnum.o


# And for C-files dependencies:

CEnum.c:           _EnumFromTo.c    _EnumFromThenTo.c
Enum_Bool.c:       _EnumFromTo.c    _EnumFromThen.c
Enum_Char.c:       _EnumFromTo.c    _EnumFromThen.c
Enum_Ordering.c:   _EnumFromTo.c    _EnumFromThen.c
_EnumFromThen.c:   _EnumFromToInc.c _EnumFromToDec.c
_EnumFromThenTo.c: _EnumFromToInc.c _EnumFromToDec.c
_EnumIndex.c:      _EnumInRange.c
_EnumRange.c:      _EnumFromTo.c
_EnumFromTo.c:     _FromEnum.c      _ToEnum.c
