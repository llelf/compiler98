-- This module controls the configuration parameters for hmake-interactive
-- by (1) allowing environment variables to override default settings
--    (2) taking care of the small differences between Haskell compilers
module HmakeConfig where

-- Known Haskell compilers and their locations are all automatically
-- generated into LocalConfig.
import LocalConfig (hmakeversion, builtby, ghcKnown, hbcKnown, nhcKnown,
                    hbcdir, nhc98dir, ghcdir, ghcver, ghclang, defaultHc)

#ifdef __HBC__
import UnsafePerformIO
#endif
#ifdef __NHC__
import IOExtras (unsafePerformIO)
#endif
#ifdef __GLASGOW_HASKELL__
import IOExts (unsafePerformIO)
#endif
import System

-- Get an environment variable if it exists, or default to given string
withDefault name def = unsafePerformIO $
   catch (do val <- getEnv name
             if null val then return def else return val)
         (\e-> return def)

-- What compilers are possible choices?
data Compiler = Nhc98 | Ghc | Hbc | Unknown String deriving (Eq)
toComp "nhc98" = Nhc98
toComp "ghc"   = Ghc
toComp "hbc"   = Hbc
toComp x       = Unknown x
instance Show Compiler where
  showsPrec p Nhc98 = showString "nhc98"
  showsPrec p Ghc   = showString "ghc"
  showsPrec p Hbc   = showString "hbc"
  showsPrec p (Unknown x) = showString x

-- Which compilers do we actually have available?
knownCompilers  = (if ghcKnown then (Ghc:) else id)
                   ((if hbcKnown then (Hbc:) else id)
                    ((if nhcKnown then (Nhc98:) else id) []))
compilerKnown c = c `elem` knownCompilers

-- What are the differences between compilers?
preludePaths c  = case c of
    Nhc98 -> [nhc98incdir]
    Hbc   -> hbcincpath
    Ghc   -> ghcincpath
    _     -> []
nonstdCoerce c  = case c of
    Nhc98 -> "import NonStdUnsafeCoerce\n\ 
              \coerce=unsafeCoerce"
    Hbc   -> "#define coerce coerceNotFound"
    Ghc   -> "import PrelGHC(unsafeCoerce#)\n\ 
             \coerce :: a -> b\ncoerce = unsafeCoerce#"
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


-- Ensure that a string has a fixed length by truncating or padding with space
fixlength n s | len > n   = take n s
              | otherwise = s ++ replicate (n-len) ' '
  where len = length s

hmakeVersion = fixlength 18 hmakeversion


-- This defines the default compiler hmake will call if none is specified
-- on the commandline.  (First look in the HC variable from the environment,
-- and if it is empty, then select the compiler we used to build hmake.
-- Actually defaultHc, set in LocalConfig, can be either the config variable
-- "builtby" or simply "nhc98".)
defaultCompiler = toComp ("HC" `withDefault` defaultHc)


-- hbc, set up for either Haskell 1.3 or Haskell 98
--     LMLDIR/HBCDIR is the base directory of your hbc installation
--     hbcincpath is a list of directories containing .hi files

hbcincpath =
  let root = "HBCDIR" `withDefault` ("LMLDIR" `withDefault` hbcdir)
  in [root++"/hlib1.3", root++"/hbc_library1.3"]




-- nhc98
--     nhc98incdir is a single directory containing standard .hi files

nhc98incdir= "NHC98INCDIR" `withDefault` nhc98dir




-- GHC, set up for various versions of the Haskell language
--     ghcincdir could be a single directory containing standard .hi files:
--  e.g.  ghcincdir=/usr/lib/ghc-3.02/lib/imports
--     or ghcincdir could be the base for...
--     ghcincpath containing multiple dirs each of which contains .hi files:
--  e.g.  ghcincdir=/usr/lib/ghc-4.04/imports
--        ghcincpath=$ghcincdir/exts $ghcincdir/text


-- Depending on compiler version, we have to choose interface paths.
--   ghcver is the value of __GLASGOW_HASKELL__
--   ghclang takes the values 2,3,4,5, corresponding to 1.2, 1.3, 1.4 and 98.

ghcincpath =
  let ghcincdir = "GHCINCDIR" `withDefault` ghcdir
  in
  if ghclang < 5 then
      -- ghc before Haskell'98
      [ghcincdir]
  else if ghcver < 406 then
      -- ghc for Haskell'98, prior to hslibs
      [ghcincdir++"/std"
      ,ghcincdir++"/exts"
      ,ghcincdir++"/misc"
      ,ghcincdir++"/posix"]
  else
      -- ghc for Haskell'98, with new hslibs
      [ghcincdir++"/std"
      ,ghcincdir++"/data"
      ,ghcincdir++"/lang"
      ,ghcincdir++"/misc"
      ,ghcincdir++"/net"
      ,ghcincdir++"/num"
      ,ghcincdir++"/text"
      ,ghcincdir++"/util"
      ,ghcincdir++"/win32"
      ,ghcincdir++"/posix"]



{-
-- From here on down, definitions are added by the config script.
-- Change these if you like, but be warned that re-running the
-- autoconfiguring step will erase your changes.
hmakeversion
builtby
ghcKnown
hbcKnown
nhcKnown
hbcdir
nhc98dir
ghcdir
ghcver
ghclang
defaultHc
-}
