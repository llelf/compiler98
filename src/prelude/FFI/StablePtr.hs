module FFI
  ( StablePtr		-- abstract
  , newStablePtr	-- :: a -> IO (StablePtr a)
  , deRefStablePtr	-- :: StablePtr a -> IO a
  , freeStablePtr	-- :: StablePtr a -> IO ()
  , castStablePtrToPtr	-- :: StablePtr a -> Ptr ()
  , castPtrToStablePtr	-- :: Ptr () -> StablePtr a
  , stablePtrToAddr	-- :: StablePtr a -> Addr	-- DEPRECATED
  , addrToStablePtr	-- :: Addr -> StablePtr a	-- DEPRECATED
  , makeStablePtr	-- :: a -> IO (StablePtr a)	-- DEPRECATED
  ) where

import FFIBuiltin (StablePtr,Addr)
import Ptr

foreign import "makeStablePtr"  newStablePtr   :: a -> IO (StablePtr a)
foreign import "derefStablePtr" deRefStablePtr :: StablePtr a -> IO a
foreign import "freeStablePtr"  freeStablePtr  :: StablePtr a -> IO ()

castStablePtrToPtr :: StablePtr a -> Ptr ()
castStablePtrToPtr s = Ptr (stablePtrToAddr s)

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr (Ptr a) = addrToStablePtr a

foreign cast stablePtrToAddr :: StablePtr a -> Addr	--DEPRECATED
foreign cast addrToStablePtr :: Addr -> StablePtr a	--DEPRECATED

makeStablePtr = newStablePtr	-- DEPRECATED: only for backwards compatibility
