-- Known Haskell compilers and their locations are all stored in an
-- hmakerc file which is maintained with the hmake-config utility.
-- This module no longer deals with that side of things.

-- Here, we just need to read read a single environment variable,
-- and determine some properties of the different compilers that relate
-- specifically to the interpreter.

module HiConfig where

import Compiler
import Config

import System
#ifdef __HBC__
import UnsafePerformIO
#ifdef __HASKELL98__
import GetPid
getProcessID = getPid
#else
getProcessID = return 3154	-- arbitrary number
#endif
#endif
#ifdef __NHC__
import IOExtras (unsafePerformIO)
foreign import "getpid" getProcessID :: IO Int
#endif
#ifdef __GLASGOW_HASKELL__
import IOExts (unsafePerformIO)
import Posix (getProcessID)
#endif

-- Generate a temporary filename unique to this invocation.
tmpfile = unsafePerformIO $ do p <- getProcessID
                               return ("/tmp/Main"++show p)

-- Get an environment variable if it exists, or default to given string
withDefault name def = unsafePerformIO $
   catch (do val <- getEnv name
             if null val then return def else return val)
         (\e-> return def)

-- Ensure that a string has a fixed length by truncating or padding with space
fixlength n s | len > n   = take n s
              | otherwise = s ++ replicate (n-len) ' '
  where len = length s


-- Definitions imported from the environment
hmakeVersion = fixlength 18 ("INSTALLVER" `withDefault` "1.8 or better")


-- What are the differences between compilers?
nonstdCoerceImport c  = case c of
    Nhc98 -> "import NonStdUnsafeCoerce"
    Hbc   -> ""
    Ghc   -> "import PrelGHC(unsafeCoerce#)"
    _     -> ""
nonstdCoerce c  = case c of
    Nhc98 -> "\ncoerce=unsafeCoerce\n"
    Hbc   -> "\ncoerce = id\t-- wrong\n"
    Ghc   -> "\ncoerce :: a -> b\ncoerce = unsafeCoerce#\n"
    _     -> ""
nonstdShow c  = case c of
    Nhc98 -> ""
    Hbc   -> "instance Show (IO a) where\n\ 
             \  showsPrec p x = showString \"<<IO action>>\""
    Ghc   -> "instance Show (IO a) where\n\ 
             \  showsPrec p x = showString \"<<IO action>>\""
    _     -> ""
nonstdShowsType c  = case c of
    Nhc98 -> "showsType"
    Hbc   -> "Operation'showsType'Unknown"
    Ghc   -> "Operation'showsType'Unknown"
    _     -> ""
defaultOptions c = case c of
    Nhc98 -> []
    Hbc   -> []
    Ghc   -> ["-fglasgow-exts"]
    _     -> []

