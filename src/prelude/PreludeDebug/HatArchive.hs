module Prelude
  (Trace(..),NmType,SR
  ,E(E),cSeq,myseq,sameAs
  ,mkTRoot,mkTAp1,mkTAp2,mkTAp3,mkTAp4,mkTAp5,mkTAp6,mkTAp7,mkTAp8,mkTAp9
  ,mkTAp10,mkTAp11,mkTAp12,mkTNm,mkTInd,mkTHidden,mkTSatA,mkTSatB,mkTSatC
  ,mkNTInt,mkNTChar,mkNTInteger,mkNTRational,mkNTFloat,mkNTDouble
  ,mkNTId',mkNTId,mkNTConstr',mkNTConstr,mkNTTuple,mkNTFun,mkNTCase
  ,mkNTLambda,mkNTDummy,mkNTCString,mkNTIf,mkNTGuard,mkNTContainer
  ,mkNoSR,mkSR',mkSR
  ) where

import PackedString (PackedString)
import HatBuiltin (CStructure)
-- import Ratio (Ratio(..))	-- remove Rationals for now

newtype NmType    = NmType    Int
newtype FileTrace = FileTrace Int
newtype SR        = SR        Int

data E a = E a			-- E to protect a closure from evaluation

myseq a b = cSeq a (E b)	-- need our own version of seq
cSeq primitive 2 :: a -> (E b) -> b


data Trace     = Trace  { traceptr    :: FileTrace
                        , trustedFun  :: Bool
                        , hidden      :: Bool
                        }

sameAs :: Trace -> Trace -> Bool
sameAs t1 t2 = primSameTrace (traceptr t1) (traceptr t2)

----
-- Trace constructors
----

mkTRoot :: Trace

mkTAp1 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> SR		-- src ref
	-> Trace	-- result

mkTAp2 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> SR		-- src ref
	-> Trace	-- result

mkTAp3 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> SR		-- src ref
	-> Trace	-- result

mkTAp4 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> SR		-- src ref
	-> Trace	-- result

mkTAp5 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> SR		-- src ref
	-> Trace	-- result

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


mkTNm :: Trace		-- trace of Nm
	-> NmType	-- NmType
	-> SR		-- src ref
	-> Trace	-- result

mkTInd :: Trace		-- trace 1
	-> Trace	-- trace 2
	-> Trace	-- result

mkTHidden :: Trace	-- trace
	-> Trace	-- result

mkTSatA :: Trace	-- trace of unevaluated expr
	-> Trace	-- result

mkTSatB :: Trace	-- original SatA
	-> Trace	-- result

mkTSatC :: Trace	-- original SatB (or SatA)
	-> Trace	-- trace of reduced value
	-> Trace	-- result


----
-- Trace constructor implementations
----

-- Note: All of these suffer from excess laziness.  We need to ensure
-- somehow that the prims are called before returning the outer
-- Trace constructor.

mkTRoot =
    let t = primTRoot
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = False
                    , hidden     = False
                    }
mkTAp1 tap tfn targ1 sr =
    let t = primTAp1 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }
mkTAp2 tap tfn targ1 targ2 sr =
    let t = primTAp2 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }
mkTAp3 tap tfn targ1 targ2 targ3 sr =
    let t = primTAp3 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }
mkTAp4 tap tfn targ1 targ2 targ3 targ4 sr =
    let t = primTAp4 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp5 tap tfn targ1 targ2 targ3 targ4 targ5 sr =
    let t = primTAp5 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp6 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 sr =
    let t = primTAp6 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp7 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 sr =
    let t = primTAp7 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp8 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 sr =
    let t = primTAp8 (traceptr tap)
                     (traceptr tfn)
                     (traceptr targ1)
                     (traceptr targ2)
                     (traceptr targ3)
                     (traceptr targ4)
                     (traceptr targ5)
                     (traceptr targ6)
                     (traceptr targ7)
                     (traceptr targ8)
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp9 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 sr =
    let t = primTAp9 (traceptr tap)
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
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp10 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        sr =
    let t = primTAp10 (traceptr tap)
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
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp11 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        targ11 sr =
    let t = primTAp11 (traceptr tap)
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
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTAp12 tap tfn targ1 targ2 targ3 targ4 targ5 targ6 targ7 targ8 targ9 targ10
        targ11 targ12 sr =
    let t = primTAp12 (traceptr tap)
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
                     sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun tfn
                    , hidden     = False
                    }

mkTNm tnm nm sr =
    let t = primTNm (traceptr tnm)
                    nm
                    sr
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedNm nm
                    , hidden     = False
                    }
mkTInd t1 t2 =
    let t = primTInd (traceptr t1)
                     (traceptr t2)
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun t1
                    , hidden     = False
                    }
mkTHidden t1 =
    let t = primTHidden (traceptr t1)
    in
    if hidden t1 then t1 else			-- collapse Hidden chains
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun t1
                    , hidden     = True
                    }
mkTSatA t1 =
    let t = primTSatA (traceptr t1)
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun t1
                    , hidden     = False
                    }
mkTSatB t1 =
    let t = primTSatB (traceptr t1)
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun t1
                    , hidden     = False
                    }
mkTSatC torig tnew =
    let t = primTSatC (traceptr torig)
                      (traceptr tnew)
    in
    t `myseq` Trace { traceptr   = t
                    , trustedFun = trustedFun torig
                    , hidden     = False
                    }


----
-- NmType constructors
----
mkNTInt		:: Int -> NmType
mkNTChar	:: Char -> NmType
mkNTInteger	:: Integer -> NmType
mkNTRational	:: Rational -> NmType
mkNTFloat	:: Float -> NmType
mkNTDouble	:: Double -> NmType
mkNTId'		:: Int -> NmType	-- dummy for compile time only
mkNTId		:: CStructure -> NmType	--   replaced by this one at runtime
mkNTConstr'	:: Int -> NmType	-- dummy for compile time only
mkNTConstr	:: CStructure -> NmType	--   replaced by this one at runtime
mkNTTuple	:: NmType
mkNTFun		:: NmType
mkNTCase	:: NmType
mkNTLambda	:: NmType
mkNTDummy	:: NmType
mkNTCString	:: PackedString -> NmType
mkNTIf		:: NmType
mkNTGuard	:: NmType
mkNTContainer	:: NmType

foreign import "primTrustedFun" trustedNm	:: NmType -> Bool

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
mkNTConstr' x	= undefined x		-- dummy for compile time only
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


----
-- SR constructors
----
mkNoSR		:: SR
mkSR'		:: Int -> SR		-- dummy for compile time only
mkSR		:: CStructure -> SR	-- is replaced by this one at runtime

mkNoSR		= primSR0
mkSR' x		= undefined x		-- dummy
mkSR x		= primSR3 x


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
