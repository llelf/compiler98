module HatArchive where

import PackedString (PackedString)
import HatBuiltin (CStructure,NmType,SR,FileTrace)
-- import Ratio (Ratio(..))	-- remove Rationals for now


data E a = E a			-- E to protect a closure from evaluation

myseq a b = cSeq a (E b)	-- need our own version of seq
cSeq primitive 2 :: a -> (E b) -> b


data Trace     = Trace  { traceptr    :: FileTrace
                        , trustedFun  :: Bool
                        , hidden      :: Bool
                        }

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
--mkNTRational	:: Rational -> NmType
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
--mkNTRational ((R n _) :% (R d _))	= primNTRational n d
mkNTFloat	= primNTFloat
mkNTDouble	= primNTDouble
mkNTId'	    x	= undefined x		-- dummy for compile time only
mkNTId	 	= primNTId		-- dummy for compile time only
mkNTConstr' x	= undefined x		-- dummy for compile time only
mkNTConstr	= primNTConstr		-- dummy for compile time only
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
mkSR		= primSR3


----
-- Primitive stuff
----
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
--foreign import primNTRational	:: Integer -> Integer -> NmType
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
