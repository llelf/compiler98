include Makefile.inc

THISLIB = Prelude
SEARCH	= -P$(LOW) -P../PreludeList -P../Ratio
EXTRA_H_FLAGS	= -prelude -P../PreludeText -P../PreludeIO -P../Ix  \
		  -P$(LOW) -P../PreludeDebug
EXTRA_C_FLAGS	=

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
        MapM.hs         MapM_.hs        Maybefun.hs         Monad_List.hs \
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
        _TupleRange.hs  _Zap_Arg.hs     _Zap_Stack.hs       _NplusK.hs \
	_Id.hs

# The following are used for the tracing Prelude
SRCS_T	= \
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
        Fst.hs          Functor_List.hs Functor_Maybe.hs    Gcd.hs \
        FractionalRaise.hs      FromIntegral.hs             RealToFrac.hs \
        Fractional_Double.hs    Fractional_Float.hs \
        Id.hs           Integral_Int.hs Integral_Integer.hs Lcm.hs \
        Sequence.hs     Sequence_.hs                                \
        MapM.hs         MapM_.hs        Maybefun.hs         Monad_List.hs \
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
        _Filter.hs \
        _Foldr.hs       _FromEnum.hs    _ToEnum.hs          _NplusK.hs \
	_EnumFromThen.hs _EnumFromThenTo.hs \
	_EnumFromTo.hs _EnumFromToDec.hs _EnumFromToInc.hs _EnumInRange.hs \
	_EnumIndex.hs _EnumRange.hs
# The following are also used for the tracing Prelude,
# however, they are not traced themselves.
SRCS_NT = _EqInteger.hs StartDbg.hs _Id.hs

#NTSRCS	= StartDbg.hs
PLSRCS	= _Apply1.hs _Apply2.hs _Apply3.hs _Apply4.hs _Zap_Arg.hs _Zap_Stack.hs
NOTDONE	= _TupleIndex.hs _TupleRange.hs


# Here are the main rules.
include ../Makefile.common



# Here are some extra rules.
#NTOBJS = $(patsubst %.hs, ${OBJDIR}/%.o, ${NTSRCS})
PLOBJS = $(patsubst %.hs, ${OBJDIR}/%.o, ${PLSRCS})

ifeq "T" "${TRACING}"
extra: ${NTOBJS} ${PLOBJS}
#$(NTOBJS): ${OBJDIR}/%.o: %.hs
#	$(X_COMPILE) -c $(PART_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
$(PLOBJS): ${OBJDIR}/%.o: %.hs
	${HC} -c $(PART_FLAGS) $(EXTRA_H_FLAGS) -o $@ $<
endif



# Here are the dependencies.

${OBJDIR}/CEnum.o: CEnum.hs ${OBJDIR}/_EnumFromTo.o  ${OBJDIR}/_EnumFromThenTo.o ${OBJDIR}/Subtract.o
${OBJDIR}/Enum_Bool.o: Enum_Bool.hs ${OBJDIR}/_EnumFromTo.o  ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/Enum_Char.o: Enum_Char.hs ${OBJDIR}/_EnumFromTo.o ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/Enum_Ordering.o: Enum_Ordering.hs ${OBJDIR}/_EnumFromTo.o ${OBJDIR}/_EnumFromThen.o
${OBJDIR}/_EnumFromThen.o: _EnumFromThen.hs ${OBJDIR}/_EnumFromToInc.o ${OBJDIR}/_EnumFromToDec.o
${OBJDIR}/_EnumFromThenTo.o: _EnumFromThenTo.hs ${OBJDIR}/_EnumFromToInc.o ${OBJDIR}/_EnumFromToDec.o
${OBJDIR}/_EnumIndex.o: _EnumIndex.hs ${OBJDIR}/_EnumInRange.o
${OBJDIR}/_EnumRange.o: _EnumRange.hs ${OBJDIR}/_EnumFromTo.o
${OBJDIR}/_EnumFromTo.o: _EnumFromTo.hs ${OBJDIR}/_FromEnum.o ${OBJDIR}/_ToEnum.o

# ${OBJDIR}/Enum_Integer.o: Enum_Integer.hs ${OBJDIR}/NumericEnumFrom.o ${OBJDIR}/NumericEnumFromThen.o 

# And for C-files dependencies:

CEnum.c:           _EnumFromTo.c    _EnumFromThenTo.c  Subtract.c
Enum_Bool.c:       _EnumFromTo.c    _EnumFromThen.c
Enum_Char.c:       _EnumFromTo.c    _EnumFromThen.c
Enum_Ordering.c:   _EnumFromTo.c    _EnumFromThen.c
_EnumFromThen.c:   _EnumFromToInc.c _EnumFromToDec.c
_EnumFromThenTo.c: _EnumFromToInc.c _EnumFromToDec.c
_EnumIndex.c:      _EnumInRange.c
_EnumRange.c:      _EnumFromTo.c
_EnumFromTo.c:     _FromEnum.c      _ToEnum.c



