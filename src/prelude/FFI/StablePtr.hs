module FFI
  ( StablePtr		-- abstract
  , newStablePtr	-- :: a -> IO (StablePtr a)
  , deRefStablePtr	-- :: StablePtr a -> IO a
  , freeStablePtr	-- :: StablePtr a -> IO ()
  , castStablePtrToPtr	-- :: StablePtr a -> Ptr ()
  , castPtrToStablePtr	-- :: Ptr () -> StablePtr a
  ) where

import Ptr

data StablePtr a

foreign import ccall "makeStablePtr"  newStablePtr   :: a -> IO (StablePtr a)
foreign import ccall "derefStablePtr" deRefStablePtr :: StablePtr a -> IO a
foreign import ccall "freeStablePtr"  freeStablePtr  :: StablePtr a -> IO ()

foreign import cast castStablePtrToPtr :: StablePtr a -> Ptr ()
foreign import cast castPtrToStablePtr :: Ptr () -> StablePtr a

