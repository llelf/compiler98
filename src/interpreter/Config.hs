module Config where
-- Mainly a bunch of constants for use in hmake-interactive.

#ifdef __HBC__
import UnsafePerformIO
#endif
#ifdef __NHC__
import IOExtras
#endif
#ifdef __GHC__
import IOExts
#endif
import System

-- get an environment variable if it exists, or default to given string
withDefault name def = unsafePerformIO $ do
  val <- getEnv name
  if null val then return def else return val

data Compiler = Nhc98 | Ghc | Hbc | Unknown deriving (Eq)
toComp "nhc98" = Nhc98
toComp "ghc"   = Ghc
toComp "hbc"   = Hbc
toComp _       = Unknown
instance Show Compiler where
  showsPrec p Nhc98 = showString "nhc98"
  showsPrec p Ghc   = showString "ghc"
  showsPrec p Hbc   = showString "hbc"
  showsPrec p Unknown = showString "unknown"

knownCompilers = [Hbc,Nhc98]

compilerKnown c = c `elem` knownCompilers
preludePaths c  = case c of
    Nhc98   -> [nhc98incdir]
    Hbc     -> hbcincpath
    Ghc     -> ghcincpath
    Unknown -> []
nonstdCoerce c  = case c of
    Nhc98   -> "import NonStdUnsafeCoerce\ncoerce=unsafeCoerce"
    Hbc     -> "#define coerce"
    Ghc     -> "import PrelGHC(unsafeCoerce#)\ncoerce :: a -> b\ncoerce = unsafeCoerce#"
    Unknown -> ""

-- Constants start here
hmakeversion = "1.7.2 (2000-02-07)"
builtby      = Hbc

-- This defines the default compiler hmake will call if none is specified
-- on the commandline.  (First look in the HC variable from the environment,
-- and if it is empty, then select the compiler we used to build hmake.)
defaultCompiler = toComp ("HC" `withDefault` "nhc98")




-- Compiler specific prelude/stdlib path info: please configure to taste
-- for your local installation.


-- hbc, set up for either Haskell 1.3 or Haskell 98
--     LMLDIR/HBCDIR is the base directory of your hbc installation
lmldir   = "LMLDIR" `withDefault` "/usr/local/lib/lmlc"
hbcdir   = "HBCDIR" `withDefault` "/usr/local/lib/lmlc"

--     HBCINCPATH is a list of directories containing .hi files
hbcincpath = [hbcdir++"/hlib1.3", hbcdir++"/hbc_library1.3"]




-- nhc98
--     NHC98INCDIR is a single directory containing standard .hi files
nhc98incdir= "NHC98INCDIR" `withDefault` "/opt/tmp/include/nhc98"




-- ghc, set up for various versions of the Haskell language
--     GHCINCDIR is a single directory containing standard .hi files:
-- ghcincdir=/usr/lib/ghc-3.02/lib/imports
--     GHCINCPATH contains multiple dirs each of which contains .hi files:
-- ghcincdir=/usr/lib/ghc-4.04/imports
-- ghcincpath=$ghcincdir/exts $ghcincdir/text

ghcincdir = "GHCINCDIR" `withDefault`
            "/opt/ghc/fptools/lib/i386-unknown-linux/ghc-3.02/imports"

-- ghcsym is the value of __GLASGOW_HASKELL__
ghcsym    = 302

-- Depending on compiler version, we have to choose interface paths.
-- GHC can take the values 2,3,4,5, corresponding to 1.2, 1.3, 1.4, and 98.
ghc=5

ghcincpath =
  if ghc < 5 then
      -- ghc before Haskell'98
      [ghcincdir]
  else if ghcsym < 406 then
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

