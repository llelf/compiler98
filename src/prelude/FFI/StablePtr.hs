module StablePtr
  ( StablePtr		-- abstract
  , makeStablePtr	-- :: a -> IO (StablePtr a)
  , deRefStablePtr	-- :: StablePtr a -> IO a
  , freeStablePtr	-- :: StablePtr a -> IO ()
  , stablePtrToAddr	-- :: StablePtr a -> Addr
  , addrToStablePtr	-- :: Addr -> StablePtr a
  ) where

import FFIBuiltin (StablePtr,Addr)

foreign import "makeStablePtr"  makeStablePtr  :: a -> IO (StablePtr a)
foreign import "derefStablePtr" deRefStablePtr :: StablePtr a -> IO a
foreign import "freeStablePtr"  freeStablePtr  :: StablePtr a -> IO ()

foreign cast stablePtrToAddr :: StablePtr a -> Addr
foreign cast addrToStablePtr :: Addr -> StablePtr a

