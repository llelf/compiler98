module ForeignObj
  ( ForeignObj			-- abstract, instance of: Eq
  , makeForeignObj		-- :: Addr -> IO () -> IO ForeignObj
  , foreignObjToAddr		-- :: ForeignObj -> Addr
--, addrToForeignObj		-- :: Addr -> ForeignObj -- no finalizer!
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

makeForeignObj			:: Addr -> IO () -> IO ForeignObj
makeForeignObj a f		 = primForeignObjC a (_E (unsafePerformIO f))

--makeForeignObj		:: Addr -> (Addr->IO()) -> IO ForeignObj
--makeForeignObj a f		 = primForeignObj a (unsafePerformIO (f a))

-- Note that `primForeignObj' is a primitive, not a `foreign import',
-- because the latter FFI has no way to return a complete ForeignObj
-- including finaliser - it can only return the address contained within.
primForeignObj primitive 2	:: Addr -> a -> IO ForeignObj

-- Note that `primForeignObjC' does not strictly conform to the FFI
-- standard.  It is not legal to return a ForeignObj as the result of
-- a foreign import.  To return a ForeignObj from C, you have to first
-- get it as an Addr, then attach the finaliser using `makeForeignObj'.
-- However, in order to implement the latter, we need one single instance
-- of returning a ForeignObj, and this is it.  ***Do not do it elsewhere!
foreign import primForeignObjC  :: Addr -> a -> IO ForeignObj

foreign cast foreignObjToAddr	:: ForeignObj -> Addr
--foreign cast addrToForeignObj	:: Addr       -> ForeignObj


-- New operation suggested by Marcin Kowalcsycz and incorporated into GHC.
-- It is a safer way to use the old unsafe `foreignObjToAddr'.
withForeignObj  :: ForeignObj -> (Addr -> IO a) -> IO a
withForeignObj fo action = action (foreignObjToAddr fo)
{- Note that GHC probably requires the following implementation:
  do
    res <- action (foreignObjToAddr fo)
    touch fo
    return res
-}

-- `Touching' a foreignObj is just intended to keep it alive in GHC across
-- calls which might otherwise allow it to be GC'ed.
touchForeignObj :: ForeignObj -> IO ()
touchForeignObj fo = return ()
