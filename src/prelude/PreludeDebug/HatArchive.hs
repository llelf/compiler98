module HatArchive where

import PreludeBuiltin (FilePtr)

-- data FilePtr a	-- an abstract type

data Trace  = Trace
data NmType = NmType
data SR     = SR


----
-- Trace
----

mkTAp1 :: FilePtr Trace		-- trace of application
	-> FilePtr Trace	-- fn
	-> FilePtr Trace	-- arg 1
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTAp2 :: FilePtr Trace		-- trace of application
	-> FilePtr Trace	-- fn
	-> FilePtr Trace	-- arg 1
	-> FilePtr Trace	-- arg 2
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTAp3 :: FilePtr Trace		-- trace of application
	-> FilePtr Trace	-- fn
	-> FilePtr Trace	-- arg 1
	-> FilePtr Trace	-- arg 2
	-> FilePtr Trace	-- arg 3
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTAp4 :: FilePtr Trace		-- trace of application
	-> FilePtr Trace	-- fn
	-> FilePtr Trace	-- arg 1
	-> FilePtr Trace	-- arg 2
	-> FilePtr Trace	-- arg 3
	-> FilePtr Trace	-- arg 4
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTAp5 :: FilePtr Trace		-- trace of application
	-> FilePtr Trace	-- fn
	-> FilePtr Trace	-- arg 1
	-> FilePtr Trace	-- arg 2
	-> FilePtr Trace	-- arg 3
	-> FilePtr Trace	-- arg 4
	-> FilePtr Trace	-- arg 5
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTNm :: FilePtr Trace		-- trace of Nm
	-> FilePtr NmType	-- NmType
	-> FilePtr SR		-- src ref
	-> FilePtr Trace	-- result

mkTInd :: FilePtr Trace		-- trace 1
	-> FilePtr Trace	-- trace 2
	-> FilePtr Trace	-- result

mkTHidden :: FilePtr Trace	-- trace
	-> FilePtr Trace	-- result

mkTSatA :: FilePtr Trace	-- trace of application
	-> FilePtr Trace	-- result

mkTSatB :: FilePtr Trace	-- original SAT
	-> FilePtr Trace	-- trace of blackhole
	-> FilePtr Trace	-- result

mkTSatC :: FilePtr Trace	-- original SAT
	-> FilePtr Trace	-- trace of value
	-> FilePtr Trace	-- result



----
-- NmType
----

mkNTInt		:: Int -> FilePtr NmType
mkNTChar	:: Char -> FilePtr NmType
mkNTInteger	:: Integer -> FilePtr NmType
mkNTRational	:: Integer -> Integer -> FilePtr NmType
mkNTFloat	:: Float -> FilePtr NmType
mkNTDouble	:: Double -> FilePtr NmType
mkNTId		:: Int -> FilePtr NmType
mkNTConstr	:: Int -> FilePtr NmType
mkNTTuple	:: FilePtr NmType
mkNTFun		:: FilePtr NmType
mkNTCase	:: FilePtr NmType
mkNTLambda	:: FilePtr NmType
mkNTDummy	:: FilePtr NmType
mkNTCString	:: FilePtr NmType
mkNTCString	:: PackedString -> FilePtr NmType
mkNTIf		:: FilePtr NmType
mkNTGuard	:: FilePtr NmType
mkNTContainer	:: FilePtr NmType


----
-- SR
----

mkSR0		:: FilePtr SR
mkSR3		:: Int -> FilePtr SR

