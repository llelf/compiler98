include Makefile.inc

OBJDIR = ${BUILDDIR}/${OBJ}/tracer/prelude/Prelude

VPATH = .

.SUFFIXES: .hi .hs .o .po


SRCS= AndAnd.hs ApplyM.hs CEnum.hs CEq.hs Seq.hs Strict.hs CFractional.hs \
	  CFunctor.hs CIntegral.hs CMonad.hs CMonadPlus.hs CMonadZero.hs \
	  CNum.hs COrd.hs CReal.hs Compose.hs Concat.hs Const.hs \
	  D2.hs D3.hs D4.hs D5.hs D6.hs D7.hs D8.hs D9.hs D10.hs D11.hs D12.hs\
	  DBool.hs DEither.hs DList.hs DMaybe.hs DOrdering.hs \
	  DUnit.hs Dollar.hs Enum_Bool.hs Enum_Char.hs Enum_Int.hs \
	  Enum_Ordering.hs Eq_2.hs Eq_3.hs Eq_4.hs Eq_Bool.hs Eq_Char.hs \
	  Eq_Int.hs Eq_List.hs Eq_Ordering.hs Error.hs Filter.hs Flip.hs \
	  Fst.hs Functor_List.hs Id.hs Integral_Int.hs MonadPlus_List.hs \
	  MonadZero_List.hs Monad_List.hs Not.hs Num_Int.hs OrOr.hs \
	  Ord_Bool.hs Ord_Char.hs Ord_Int.hs Ord_Ordering.hs Otherwise.hs \
	  Real_Int.hs Snd.hs TString.hs _Filter.hs _Foldr.hs _FromEnum.hs \
	  Curry.hs Uncurry.hs Gcd.hs \
	  Eq_Float.hs Eq_Integer.hs Eq_Maybe.hs Eq_Double.hs Eq_Either.hs \
	  Ord_2.hs Ord_3.hs Ord_4.hs Ord_List.hs Ord_Integer.hs Ord_Either.hs \
	  Num_Double.hs Num_Float.hs Num_Integer.hs \
	  Even.hs CRealFrac.hs CFloating.hs CRealFloat.hs \
	  Fractional_Float.hs Fractional_Double.hs Integral_Integer.hs \
	  RealFloat_Double.hs RealFloat_Float.hs \
	  RealFrac_Float.hs RealFrac_Double.hs \
	  Undefined.hs \
	  Floating_Double.hs Floating_Float.hs \
	  NumericEnumFrom.hs NumericEnumFromThen.hs \
	  Real_Double.hs Real_Float.hs Real_Integer.hs Enum_Integer.hs \
	  Ord_Double.hs Ord_Float.hs \
	  Sequence.hs Accumulate.hs MapM.hs MapM_.hs Maybe.hs \
	  Either.hs

#	  Eval_Fun.hs Eval_Int.hs Eval_Char.hs \
#	  Eval_Float.hs Eval_Double.hs Eval_Integer.hs \

NTSRCS= StartDbg.hs
PLSRCS= _Apply1.hs _Apply2.hs _Apply3.hs _Apply4.hs _Zap_Arg.hs _Zap_Stack.hs 

NOTDONE= AsTypeOf.hs Atan2.hs Bounded_2.hs Bounded_3.hs \
	 Bounded_Bool.hs Bounded_Char.hs Bounded_Int.hs Bounded_Ordering.hs \
	 Bounded_Unit.hs CBounded.hs CIx.hs \
	 DVoid.hs Done.hs \
	 Enum_Double.hs Enum_Float.hs Enum_Unit.hs \
	 Eq_Unit.hs FractionalRaise.hs FromIntegral.hs \
	 FromRealFrac.hs \
	 Functor_Maybe.hs Guard.hs Lcm.hs \
	 MonadPlus_Maybe.hs MonadZero_Maybe.hs \
	 Monad_Maybe.hs NumRaise.hs \
	 Odd.hs Ord_Double.hs \
	 Ord_Float.hs Ord_Maybe.hs Ord_Unit.hs \
	 Pred.hs  \
	 Subtract.hs Succ.hs \
	 Unless.hs Until.hs When.hs _Apply1.hs _Apply2.hs \
	 _Apply3.hs _Apply4.hs _EnumFromThen.hs _EnumFromThenTo.hs \
	 _EnumFromTo.hs _EnumFromToDec.hs _EnumFromToInc.hs _EnumInRange.hs \
	 _EnumIndex.hs _EnumRange.hs _ToEnum.hs _TupleIndex.hs _TupleRange.hs
	 
MAINSRC= Prelude.hs

OBJS = $(patsubst %.hs,${OBJDIR}/%.o,${SRCS})
PLOBJS = $(patsubst %.hs,${OBJDIR}/%.o,${PLSRCS})
NTOBJS = $(patsubst %.hs,${OBJDIR}/%.o,${NTSRCS})
MAINOBJ = $(patsubst %.hs,${OBJDIR}/%.o,${MAINSRC})

MAINIFC= $(patsubst %.hs,%.hi,${MAINSRC})

$(OBJS): ${OBJDIR}/%.o: %.hs
	$(DBG_COMPILE) -o $@ $<

$(NTOBJS): ${OBJDIR}/%.o: %.hs
	$(COMPILE) -P../PreludeIO/ -P../LowB/ -o $@ $<

$(PLOBJS): ${OBJDIR}/%.o: %.hs
	${NHC98} -c $(PROF) -part -prelude -redefine -o $@ $<

all: ${MAINOBJ}

${OBJDIR}: ${OBJDIRS}
	mkdir -p ${OBJDIR}

objdir: ${OBJDIR}

${MAINOBJ}: ${OBJS} ${NTOBJS} ${PLOBJS} ${MAINSRC}
	$(DBG_COMPILE) -P../PreludeIO/ -P../PreludeList/ -P../PreludeText/ -P../PreludeDebug/ -o $@ ${MAINSRC}

install:
	cp ${MAINIFC} ${IFCDEST}

clean:
	rm -f ${OBJS} ${NTOBJS} ${PLOBJS} ${MAINOBJ}
