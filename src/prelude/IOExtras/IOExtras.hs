module IOExtras
  ( fixIO		-- :: (a->IO a) -> IO a
  , unsafePerformIO	-- :: IO a -> a
  , unsafeInterleaveIO	-- :: IO a -> IO a

  , IORef		-- data IORef a = <abstract>
    -- instance Eq (IORef a)
  , newIORef		-- :: a -> IO (IORef a)
  , readIORef		-- :: IORef a -> IO a
  , writeIORef		-- :: IORef a -> a -> IO ()

  , module Ix		-- re-export Ix for the benefit of IOArrays
  , IOArray		-- data IOArray ix elt -- mutable arrays
  , newIOArray		-- :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
  , boundsIOArray	-- :: Ix ix => IOArray ix elt -> (ix, ix)
  , readIOArray		-- :: Ix ix => IOArray ix elt -> ix -> IO elt
  , writeIOArray	-- :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
  , freezeIOArray	-- :: Ix ix => IOArray ix elt -> IO (Array ix elt)
    -- instance Eq (IOArray ix elt)

  , performGC		-- :: IO ()
  , trace		-- :: String -> a -> a
  , unsafePtrEq		-- :: a -> a -> Bool
  ) where


import FixIO
import UnsafePerformIO
import Ix

import DIORef
import NewIORef
import ReadIORef
import WriteIORef

--import Array

import DIOArray
import NewIOArray
import BoundsIOArray
import ReadIOArray
import WriteIOArray
import FreezeIOArray

import PerformGC
import NonStdTrace
import UnsafePtrEq

nyi f = trace ("IOExtras."++f++": not implemented")

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = nyi "unsafeInterleaveIO"

