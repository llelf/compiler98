module ForeignObj
  ( ForeignObj			-- abstract, instance of: Eq
  , makeForeignObj		-- :: Addr -> IO () -> IO ForeignObj
  , foreignObjToAddr		-- :: ForeignObj -> Addr
  , withForeignObj		-- :: ForeignObj -> (Addr -> IO a) -> IO a
  , touchForeignObj		-- :: ForeignObj -> IO ()
  ) where

import FFIBuiltin (ForeignObj,Addr)
import IOExtras (unsafePerformIO)

-- Note that the type of makeForeignObj expects the finalizer to already
-- have been applied to the Addr, and here we also need to wrap it inside
-- an `unsafePerformIO'.  Furthermore, we then need to wrap that inside
-- a box so that the primitive call does not evaluate it on the way in!

data _E a = _E a	-- just a box to protect arg from evaluation


-- The only way to create a ForeignObj.
makeForeignObj			:: Addr -> IO () -> IO ForeignObj
makeForeignObj a f		 = primForeignObjC a (_E (unsafePerformIO f))

-- A different name for the same thing.
newForeignObj			:: Addr -> IO () -> IO ForeignObj
newForeignObj = makeForeignObj


-- Note that `primForeignObjC' does not strictly conform to the FFI
-- standard.  It is not legal to return a ForeignObj as the result of
-- a foreign import.  To return a ForeignObj from C, you have to first
-- get it as an Addr, then attach the finaliser using `makeForeignObj'.
-- However, in order to implement the latter, we need one single instance
-- of returning a ForeignObj, and this is it.  ***Do not do it elsewhere!
foreign import primForeignObjC  :: Addr -> a -> IO ForeignObj


-- Get the hidden Addr out of a ForeignObj.
foreign cast foreignObjToAddr	:: ForeignObj -> Addr

-- But it is impossible to do the opposite!
--foreign cast addrToForeignObj	:: Addr       -> ForeignObj	-- WRONG!


-- New operation suggested by Marcin Kowalcsycz.
-- It is a safer way to use the older, less safe, `foreignObjToAddr'.
withForeignObj  :: ForeignObj -> (Addr -> IO a) -> IO a
withForeignObj fo action = action (foreignObjToAddr fo)
{- Note that GHC probably requires the following implementation:
  do
    res <- action (foreignObjToAddr fo)
    touch fo
    return res
-}

-- `Touching' a foreignObj is just intended to keep it alive across
-- calls which might otherwise allow it to be GC'ed.  Only really
-- an issue in GHC - for nhc98 a null-op is sufficient.
touchForeignObj :: ForeignObj -> IO ()
touchForeignObj fo = return ()
