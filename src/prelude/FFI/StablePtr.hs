module StablePtr
  ( StablePtr		-- abstract
  , makeStablePtr	-- :: a -> IO (StablePtr a)
  , deRefStablePtr	-- :: StablePtr a -> IO a
  , freeStablePtr	-- :: StablePtr a -> IO ()
  , stablePtrToAddr	-- :: StablePtr a -> Addr
  , addrToStablePtr	-- :: Addr -> StablePtr a
  ) where

import FFIBuiltin (StablePtr,Addr)

foreign import "stableInsert"  makeStablePtr  :: a -> IO (StablePtr a)
foreign import "stableRef"     deRefStablePtr :: StablePtr a -> IO a
foreign import "stableRelease" freeStablePtr  :: StablePtr a -> IO ()

foreign cast stablePtrToAddr :: StablePtr a -> Addr
foreign cast addrToStablePtr :: Addr -> StablePtr a

