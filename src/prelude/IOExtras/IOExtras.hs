module IOExtras
  ( fixIO		-- :: (a->IO a) -> IO a
  , unsafePerformIO	-- :: IO a -> a
  , unsafeInterleaveIO	-- :: IO a -> IO a

  , IORef		-- data IORef a = <abstract>
    -- instance Eq (IORef a)
  , newIORef		-- :: a -> IO (IORef a)
  , readIORef		-- :: IORef a -> IO a
  , writeIORef		-- :: IORef a -> a -> IO ()

{-
  , IOArray		-- data IOArray ix elt -- mutable arrays
  , newIOArray		-- :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
  , boundsIOArray	-- :: Ix ix => IOArray ix elt -> (ix, ix)
  , readIOArray		-- :: Ix ix => IOArray ix elt -> ix -> IO elt
  , writeIOArray	-- :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
  , freezeIOArray	-- :: Ix ix => IOArray ix elt -> IO (Array ix elt)
    -- instance Eq (IOArray ix elt)
-}

  , performGC		-- :: IO ()
  , trace		-- :: String -> a -> a
  , unsafePtrEq		-- :: a -> a -> Bool
  ) where


import FixIO
import UnsafePerformIO

import DIORef
import NewIORef
import ReadIORef
import WriteIORef

import Array
{-
import DIOArray
import NewIOArray
import BoundsIOArray
import ReadIOArray
import WriteIOArray
import FreezeIOArray
-}

import PerformGC
import NonStdTrace
import UnsafePtrEq

nyi f = trace ("IOExtras."++f++": not implemented")

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO = nyi "unsafeInterleaveIO"


{-
data IOArray ix elt -- mutable arrays indexed by values of type ix
                    -- containing values of type a.
newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)
instance Eq (IOArray ix elt)
-}

data IOArray ix elt = IOArray ()
newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
newIOArray _ _       = nyi "newIOArray" (return (IOArray ()))
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
boundsIOArray _      = nyi "boundsIOArray" (error "Stop")
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
readIOArray _ _      = nyi "readIOArray" (return (error "Stop"))
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
writeIOArray _ _ _   = nyi "writeIOArray" (return ())
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)
freezeIOArray _      = nyi "freezeIOArray" (return (error "Stop"))

