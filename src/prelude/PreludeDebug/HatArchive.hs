module HatArchive where

import PreludeBuiltin (Filed)

{-
data Filed a			-- an abstract type, variable 'a' is phantom
-}


data Trace = Trace   { traceptr    :: Filed Trace
                     , trustedFun  :: Bool
                     , hidden      :: Bool
                     }
data NmType = NmType { nmtypeptr   :: Filed NmType
                     , trustedNm   :: Bool
                     }
newtype SR = SR      { _sr         :: Filed SR
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
    Trace { traceptr   = primTRoot
          , trustedFun = False
          , hidden     = False
          }
mkTAp1 tap tfn targ1 sr =
    Trace { traceptr   = primTAp1 (traceptr tap)
                                  (traceptr tfn)
                                  (traceptr targ1)
                                  (_sr sr)
          , trustedFun = trustedFun tap && trustedFun tfn
          , hidden     = False
          }
mkTAp2 tap tfn targ1 targ2 sr =
    Trace { traceptr   = primTAp2 (traceptr tap)
                                  (traceptr tfn)
                                  (traceptr targ1)
                                  (traceptr targ2)
                                  (_sr sr)
          , trustedFun = trustedFun tap && trustedFun tfn
          , hidden     = False
          }
mkTAp3 tap tfn targ1 targ2 targ3 sr =
    Trace { traceptr   = primTAp3 (traceptr tap)
                                  (traceptr tfn)
                                  (traceptr targ1)
                                  (traceptr targ2)
                                  (traceptr targ3)
                                  (_sr sr)
          , trustedFun = trustedFun tap && trustedFun tfn
          , hidden     = False
          }
mkTAp4 tap tfn targ1 targ2 targ3 targ4 sr =
    Trace { traceptr   = primTAp4 (traceptr tap)
                                  (traceptr tfn)
                                  (traceptr targ1)
                                  (traceptr targ2)
                                  (traceptr targ3)
                                  (traceptr targ4)
                                  (_sr sr)
          , trustedFun = trustedFun tap && trustedFun tfn
          , hidden     = False
          }
mkTAp5 tap tfn targ1 targ2 targ3 targ4 targ5 sr =
    Trace { traceptr   = primTAp5 (traceptr tap)
                                  (traceptr tfn)
                                  (traceptr targ1)
                                  (traceptr targ2)
                                  (traceptr targ3)
                                  (traceptr targ4)
                                  (traceptr targ5)
                                  (_sr sr)
          , trustedFun = trustedFun tap && trustedFun tfn
          , hidden     = False
          }
mkTNm tnm nm sr =
    Trace { traceptr   = primTNm (traceptr tnm)
                                 (nmtypeptr nm)
                                 (_sr sr)
          , trustedFun = trustedNm nm
          , hidden     = False
          }
mkTInd t1 t2 =
    Trace { traceptr   = primTInd (traceptr t1)
                                  (traceptr r2)
          , trustedFun = trustedFun t1
          , hidden     = False
          }
mkTHidden t1 =
    Trace { traceptr   = primTHidden (traceptr t1)
          , trustedFun = trustedFun t1
          , hidden     = True
          }
mkTSatA t1 =
    Trace { traceptr   = primTSatA (traceptr t1)
          , trustedFun = trustedFun t1
          , hidden     = False
          }
mkTSatB t1 =
    Trace { traceptr   = primTSatB (traceptr t1)
          , trustedFun = trustedFun t1
          , hidden     = False
          }
mkTSatC torig tnew =
    Trace { traceptr   = primTSatC (traceptr torig)
                                   (traceptr tnew)
          , trustedFun = trustedFun torig
          , hidden     = False
          }


----
-- NmType constructors
----
mkNTInt		:: Int -> NmType
mkNTChar	:: Char -> NmType
mkNTInteger	:: Integer -> NmType
mkNTRational	:: Integer -> Integer -> NmType
mkNTFloat	:: Float -> NmType
mkNTDouble	:: Double -> NmType
mkNTId		:: Int -> NmType
mkNTConstr	:: Int -> NmType
mkNTTuple	:: NmType
mkNTFun		:: NmType
mkNTCase	:: NmType
mkNTLambda	:: NmType
mkNTDummy	:: NmType
mkNTCString	:: NmType
mkNTCString	:: PackedString -> NmType
mkNTIf		:: NmType
mkNTGuard	:: NmType
mkNTContainer	:: NmType

----
-- SR constructors
----
mkSR0		:: SR
mkSR3		:: Int -> SR


----
-- Primitive stuff
----
foreign import primTRoot :: Filed Trace

foreign import primTAp1 :: Filed Trace		-- application
			-> Filed Trace		-- fn
			-> Filed Trace		-- arg1
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTAp2 :: Filed Trace		-- application
			-> Filed Trace		-- fn
			-> Filed Trace		-- arg1
			-> Filed Trace		-- arg2
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTAp3 :: Filed Trace		-- application
			-> Filed Trace		-- fn
			-> Filed Trace		-- arg1
			-> Filed Trace		-- arg2
			-> Filed Trace		-- arg3
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTAp4 :: Filed Trace		-- application
			-> Filed Trace		-- fn
			-> Filed Trace		-- arg1
			-> Filed Trace		-- arg2
			-> Filed Trace		-- arg3
			-> Filed Trace		-- arg4
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTAp5 :: Filed Trace		-- application
			-> Filed Trace		-- fn
			-> Filed Trace		-- arg1
			-> Filed Trace		-- arg2
			-> Filed Trace		-- arg3
			-> Filed Trace		-- arg4
			-> Filed Trace		-- arg5
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTNm :: Filed Trace		-- trace of Nm
			-> Filed NmType		-- NmType
			-> Filed SR		-- src ref
			-> Filed Trace		-- result

foreign import primTInd :: Filed Trace		-- trace 1
			-> Filed Trace		-- trace 2
			-> Filed Trace		-- result

foreign import primTHidden :: Filed Trace	-- trace
			-> Filed Trace		-- result

foreign import primTSatA :: Filed Trace		-- trace of unevaluated expr
			-> Filed Trace		-- result

foreign import primTSatB :: Filed Trace		-- original SatA
			-> Filed Trace		-- result

foreign import primTSatC :: Filed Trace		-- original SatB (or SatA)
			-> Filed Trace		-- trace of reduced value
			-> Filed Trace		-- result

