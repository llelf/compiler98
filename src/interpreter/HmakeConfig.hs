-- Known Haskell compilers and their locations are all automatically
-- generated into an hmake.config file which is added to the shell
-- environment before calling this program, therefore we just need to
-- read the appropriate environment variables.

-- This module controls the configuration parameters for hmake-interactive
-- by (1) allowing environment variables to override default settings
--    (2) taking care of the small differences between Haskell compilers

module HmakeConfig where

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

-- Ensure that a string has a fixed length by truncating or padding with space
fixlength n s | len > n   = take n s
              | otherwise = s ++ replicate (n-len) ' '
  where len = length s


-- Definitions imported from the environment
hmakeVersion = fixlength 18 ("INSTALLVER" `withDefault` "1.8 or better")
builtby      = "BUILTBY"    `withDefault` "unknown"
ghcKnown     = case "ghcknown" `withDefault` "no" of
                 "yes" -> True
                 _     -> False
hbcKnown     = case "hbcknown" `withDefault` "no" of
                 "yes" -> True
                 _     -> False
nhcKnown     = case "nhc98known" `withDefault` "no" of
                 "yes" -> True
                 _     -> False
ghcdir       = "GHCINCDIR" `withDefault` "unknown"
hbcdir       = "HBCDIR" `withDefault` "unknown"
nhc98dir     = "NHC98INCDIR" `withDefault` "unknown"
ghcver       = read ("ghcsym" `withDefault` "0") `asTypeOf` 0
ghclang      = read ("GHC" `withDefault` "0") `asTypeOf` 0

defaultCompiler = toComp ("HC" `withDefault` ("COMP" `withDefault` builtby))


-- What compilers are possible choices?
data Compiler = Nhc98 | Ghc | Hbc | Unknown String deriving (Eq)
toComp "gcc"   = Nhc98	-- to cope with bootstrapping from C.
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
nonstdCoerceImport c  = case c of
    Nhc98 -> "import NonStdUnsafeCoerce"
    Hbc   -> ""
    Ghc   -> "import PrelGHC(unsafeCoerce#)"
    _     -> ""
nonstdCoerce c  = case c of
    Nhc98 -> "\ncoerce=unsafeCoerce\n"
    Hbc   -> "\ncoerce = id	-- wrong\n"
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

