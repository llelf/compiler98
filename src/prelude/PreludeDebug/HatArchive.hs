module Prelude
  (Trace,trustedFun,trustedNm,hidden,NmType,SR
  ,E(E),myseq,sameAs
  ,mkTRoot,mkTAp1,mkTAp2,mkTAp3,mkTAp4,mkTAp5,mkTAp6,mkTAp7,mkTAp8,mkTAp9
  ,mkTAp10,mkTAp11,mkTAp12,mkTNm,mkTInd,mkTHidden,mkTSatA,mkTSatB,mkTSatC
  ,mkTSatALonely,mkTSatBLonely,mkTSatCLonely
  ,mkNTInt,mkNTChar,mkNTInteger,mkNTRational,mkNTFloat,mkNTDouble
  ,mkNTId',mkNTId,mkNTConstr',mkNTConstr,mkNTTuple,mkNTFun,mkNTCase
  ,mkNTLambda,mkNTDummy,mkNTCString,mkNTIf,mkNTGuard,mkNTContainer
  ,mkNoSR,mkSR',mkSR
  ) where

import PackedString (PackedString,packString)
import PreludeBuiltin (NmType)
import HatBuiltin (CStructure)
-- import Ratio (Ratio(..))	-- remove Rationals for now

newtype FileTrace = FileTrace Int
newtype SR        = SR        Int

data E a = E a			-- E to protect a closure from evaluation

myseq a b = _seq a b		-- MAGIC (bytecode version of seq)

newtype Trace = Trace CStructure
foreign import   "primTrustedFun" trustedFun :: Trace -> Bool
foreign import   "primHidden"     hidden     :: Trace -> Bool
--foreign import "primTracePtr"   traceptr   :: Trace -> FileTrace
--foreign import mkTrace :: FileTrace -> Bool -> Bool -> Trace

{-
data Trace     = Trace  { traceptr    :: FileTrace
                        , trustedFun  :: Bool
                        , hidden      :: Bool
                        }
-}

foreign import "primSameTrace" sameAs :: Trace -> Trace -> Bool
--sameAs :: Trace -> Trace -> Bool
--sameAs t1 t2 = primSameTrace (traceptr t1) (traceptr t2)

----
-- Trace constructors
----

foreign import "primTRoot"
 mkTRoot :: Trace

foreign import "primTAp1"
 mkTAp1 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp2"
 mkTAp2 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp3"
 mkTAp3 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp4"
 mkTAp4 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp5"
 mkTAp5 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp6"
 mkTAp6 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp7"
 mkTAp7 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp8"
 mkTAp8 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp9"
 mkTAp9 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp10"
 mkTAp10 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp11"
 mkTAp11 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp12"
 mkTAp12 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> Trace	-- arg 12
	-> SR		-- src ref
	-> Trace	-- result


foreign import "primTNm"
 mkTNm :: Trace		-- trace of Nm
	-> NmType	-- NmType
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTInd"
 mkTInd :: Trace		-- trace 1
	-> Trace	-- trace 2
	-> Trace	-- result

foreign import "primTHidden"
 mkTHidden :: Trace	-- trace
	-> Trace	-- result

foreign import "primTSatA"
 mkTSatA :: Trace	-- trace of unevaluated expr
	-> Trace	-- result

foreign import "primTSatB"
 mkTSatB :: Trace	-- original SatA
	-> Trace	-- result

foreign import "primTSatC"
 mkTSatC :: Trace	-- original SatB (or SatA)
	-> Trace	-- trace of reduced value
	-> Trace	-- result

foreign import "primTSatALonely"
 mkTSatALonely :: Trace	-- trace of unevaluated expr
	-> Trace	-- result

foreign import "primTSatBLonely"
 mkTSatBLonely :: Trace	-- original SatA
	-> Trace	-- result

foreign import "primTSatCLonely"
 mkTSatCLonely :: Trace	-- original SatB (or SatA)
	-> Trace	-- trace of reduced value
	-> Trace	-- result


{-
----
-- Trace constructor implementations
----

mkTRoot =
    mkTrace primTRoot False False
mkTAp1 tap tfn targ1 sr = 
    mkTrace (primTAp1 (traceptr tap)
                      (traceptr tfn)
                      (traceptr targ1)
                      sr)
            (trustedFun tfn)
            False
mkTAp2 tap tfn targ1 targ2 sr = 
    mkTrace (primTAp2 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     sr)
            (trustedFun tfn)
            False

mkTAp3 tap tfn targ1 targ2 targ3 sr = 
    mkTrace (primTAp3 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     sr)
            (trustedFun tfn)
            False

mkTAp4 tap tfn targ1 targ2 targ3 targ4 sr = 
    mkTrace (primTAp4 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     sr)
            (trustedFun tfn)
            False

mkTAp5 tap tfn targ1 targ2 targ3 targ4 targ5 sr =
    mkTrace (primTAp5 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     sr)
            (trustedFun tfn)
            False

mkTAp6 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 sr =
    mkTrace (primTAp6 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     sr)
            (trustedFun tfn)
            False

mkTAp7 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 sr =
    mkTrace (primTAp7 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     sr)
            (trustedFun tfn)
            False

mkTAp8 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 sr =
    mkTrace (primTAp8 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     sr)
            (trustedFun tfn)
            False

mkTAp9 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 sr =
    mkTrace (primTAp9 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     (traceptr targ9)
                     sr)
            (trustedFun tfn)
            False

mkTAp10 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        sr =
    mkTrace (primTAp10 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     (traceptr targ9)
                     (traceptr targ10)
                     sr)
            (trustedFun tfn)
            False

mkTAp11 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        targ11 sr =
    mkTrace (primTAp11 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     (traceptr targ9)
                     (traceptr targ10)
                     (traceptr targ11)
                     sr)
            (trustedFun tfn)
            False

mkTAp12 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        targ11 targ12 sr =
    mkTrace (primTAp12 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     (traceptr targ9)
                     (traceptr targ10)
                     (traceptr targ11)
                     (traceptr targ12)
                     sr)
            (trustedFun tfn)
            False


mkTNm tnm nm sr = 
    mkTrace (primTNm (traceptr tnm)
                    nm
                    sr)
            (trustedNm nm)
            False
mkTInd t1 t2 = 
    mkTrace (primTInd (traceptr t1)
                     (traceptr t2))
            (trustedFun t1)
            False
mkTHidden t1 = 
    mkTrace (primTHidden (traceptr t1))
            (trustedFun t1)
            True
mkTSatA t1 = 
    mkTrace (primTSatA (traceptr t1))
            (trustedFun t1)
            False
mkTSatB t1 = 
    mkTrace (primTSatB (traceptr t1))
            (trustedFun t1)
            False
mkTSatC torig tnew = 
    mkTrace (primTSatC (traceptr torig) (traceptr tnew))
            (trustedFun torig)
            False
-}


----
-- NmType constructors
----
foreign import "primNTInt"
 mkNTInt	:: Int -> NmType
foreign import "primNTChar"
 mkNTChar	:: Char -> NmType
foreign import "primNTInteger"
 mkNTInteger	:: Integer -> NmType
foreign import "primNTRational"
 primNTRational	:: Integer -> Integer -> NmType
foreign import "primNTFloat"
 mkNTFloat	:: Float -> NmType
foreign import "primNTDouble"
 mkNTDouble	:: Double -> NmType
foreign import "primNTId"
 mkNTId		:: CStructure -> NmType	--   replaced by this one at runtime
foreign import "primNTConstr"
 mkNTConstr	:: CStructure -> NmType	--   replaced by this one at runtime
foreign import "primNTTuple"
 mkNTTuple	:: NmType
foreign import "primNTFun"
 mkNTFun	:: NmType
foreign import "primNTCase"
 mkNTCase	:: NmType
foreign import "primNTLambda"
 mkNTLambda	:: NmType
foreign import "primNTDummy"
 mkNTDummy	:: NmType
foreign import "primNTCString"
 mkNTCString	:: PackedString -> NmType
foreign import "primNTIf"
 mkNTIf		:: NmType
foreign import "primNTGuard"
 mkNTGuard	:: NmType
foreign import "primNTContainer"
 mkNTContainer	:: NmType

mkNTRational	:: Rational -> NmType
mkNTRational x	= primNTRational 0 0	-- dummy (constructor :% not available)
mkNTId'		:: Int -> NmType	-- dummy for compile time only
mkNTId' x	= undefined x		-- dummy for compile time only
mkNTConstr'	:: Int -> NmType	-- dummy for compile time only
mkNTConstr' x	= undefined x		-- dummy for compile time only

foreign import "primTrustedNm" trustedNm	:: NmType -> Bool

{-
----
-- NmType constructor implementations
----
mkNTInt		= primNTInt
mkNTChar	= primNTChar
mkNTInteger	= primNTInteger
mkNTRational x	= primNTRational 0 0	-- dummy (constructor :% not available)
mkNTFloat	= primNTFloat
mkNTDouble	= primNTDouble
mkNTId' x	= undefined x		-- dummy for compile time only
mkNTId x	= primNTId x		--   replaced by this one at runtime
mkNTConstr x	= primNTConstr x	--   replaced by this one at runtime
mkNTTuple	= primNTTuple
mkNTFun		= primNTFun
mkNTCase	= primNTCase
mkNTLambda	= primNTLambda
mkNTDummy	= primNTDummy
mkNTCString	= primNTCString
mkNTIf		= primNTIf
mkNTGuard	= primNTGuard
mkNTContainer	= primNTContainer
-}

----
-- SR constructors
----
foreign import "primSR0"
 mkNoSR		:: SR

mkSR'		:: Int -> SR		-- dummy for compile time only
mkSR' x		= undefined x		-- dummy

foreign import "primSR3"
 mkSR		:: CStructure -> SR	-- is replaced by this one at runtime

--mkNoSR	= primSR0
--mkSR x	= primSR3 x


{-
----
-- Primitive stuff
----
foreign import primSameTrace :: FileTrace -> FileTrace -> Bool

foreign import primTRoot :: FileTrace

foreign import primTAp1 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp2 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp3 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp4 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp5 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp6 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp7 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp8 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> FileTrace		-- arg8
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp9 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> FileTrace		-- arg8
			-> FileTrace		-- arg9
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp10 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> FileTrace		-- arg8
			-> FileTrace		-- arg9
			-> FileTrace		-- arg10
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp11 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> FileTrace		-- arg8
			-> FileTrace		-- arg9
			-> FileTrace		-- arg10
			-> FileTrace		-- arg11
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTAp12 :: FileTrace		-- application
			-> FileTrace		-- fn
			-> FileTrace		-- arg1
			-> FileTrace		-- arg2
			-> FileTrace		-- arg3
			-> FileTrace		-- arg4
			-> FileTrace		-- arg5
			-> FileTrace		-- arg6
			-> FileTrace		-- arg7
			-> FileTrace		-- arg8
			-> FileTrace		-- arg9
			-> FileTrace		-- arg10
			-> FileTrace		-- arg11
			-> FileTrace		-- arg12
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTNm :: FileTrace		-- trace of Nm
			-> NmType		-- NmType
			-> SR			-- src ref
			-> FileTrace		-- result

foreign import primTInd :: FileTrace		-- trace 1
			-> FileTrace		-- trace 2
			-> FileTrace		-- result

foreign import primTHidden :: FileTrace		-- trace
			-> FileTrace		-- result

foreign import primTSatA :: FileTrace		-- trace of unevaluated expr
			-> FileTrace		-- result

foreign import primTSatB :: FileTrace		-- original SatA
			-> FileTrace		-- result

foreign import primTSatC :: FileTrace		-- original SatB (or SatA)
			-> FileTrace		-- trace of reduced value
			-> FileTrace		-- result


foreign import primNTInt	:: Int -> NmType
foreign import primNTChar	:: Char -> NmType
foreign import primNTInteger	:: Integer -> NmType
foreign import primNTRational	:: Integer -> Integer -> NmType
foreign import primNTFloat	:: Float -> NmType
foreign import primNTDouble	:: Double -> NmType
foreign import primNTId		:: CStructure -> NmType
foreign import primNTConstr	:: CStructure -> NmType
foreign import primNTTuple	:: NmType
foreign import primNTFun	:: NmType
foreign import primNTCase	:: NmType
foreign import primNTLambda	:: NmType
foreign import primNTDummy	:: NmType
foreign import primNTCString	:: PackedString -> NmType
foreign import primNTIf		:: NmType
foreign import primNTGuard	:: NmType
foreign import primNTContainer	:: NmType


foreign import primSR0	:: SR
foreign import primSR3	:: CStructure -> SR

-}
