module ForeignObj
  ( ForeignObj			-- abstract, instance of: Eq
  , makeForeignObj		-- :: Addr -> IO () -> IO ForeignObj
  , foreignObjToAddr		-- :: ForeignObj -> Addr
--, addrToForeignObj		-- :: Addr -> ForeignObj -- no finalizer!
  ) where

import FFIBuiltin (ForeignObj,Addr)
import IOExtras (unsafePerformIO)

-- Note that the type of makeForeignObj expects the finalizer to already
-- have been applied to the Addr, and here we also need to wrap it inside
-- an `unsafePerformIO'.

makeForeignObj			:: Addr -> IO () -> IO ForeignObj
makeForeignObj a f		 = primForeignObj a (unsafePerformIO f)

--makeForeignObj		:: Addr -> (Addr->IO()) -> IO ForeignObj
--makeForeignObj a f		 = primForeignObj a (unsafePerformIO (f a))

-- Note that `primForeignObj' is a primitive, not a `foreign import',
-- because the latter FFI has no way to return a complete ForeignObj
-- including finaliser - it can only return the address contained within.
primForeignObj primitive 2	:: Addr -> a -> IO ForeignObj

foreign cast foreignObjToAddr	:: ForeignObj -> Addr
--foreign cast addrToForeignObj	:: Addr       -> ForeignObj

