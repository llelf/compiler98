module Config where

import Compiler
import Local (globalConfigFileDir)
import System (getEnv)
import Directory (doesFileExist)
#if defined(__NHC__)
import IOExtras (unsafePerformIO)
#elif defined(__GLASGOW_HASKELL__)
import IOExts (unsafePerformIO)
#elif defined(__HBC__)
import UnsafePerformIO (unsafePerformIO)
#endif

data HmakeConfig = HmakeConfig
  { defaultCompiler :: FilePath
  , knownCompilers  :: [CompilerConfig]
  }
  deriving (Eq,Read)

data CompilerConfig = CompilerConfig
  { compilerStyle      :: HC
  , compilerPath       :: FilePath
  , compilerVersion    :: String
  , includePaths       :: [FilePath]
  , cppSymbols         :: [String]
  , extraCompilerFlags :: [String]
  , isHaskell98        :: Bool
  }
  deriving (Read)

instance Eq CompilerConfig where	-- equality on filename only
  cc1 == cc2   =   compilerPath cc1 == compilerPath cc2

instance Show CompilerConfig where
  showsPrec p cc =
      showString "CompilerConfig"
      . showString "\n      { compilerStyle = " . shows (compilerStyle cc)
      . showString "\n      , compilerPath = " . shows (compilerPath cc)
      . showString "\n      , compilerVersion = " . shows (compilerVersion cc)
      . showString "\n      , includePaths = " . showPaths (includePaths cc)
      . showString "\n      , cppSymbols = " . shows (cppSymbols cc)
      . showString "\n      , extraCompilerFlags = "
                                               . shows (extraCompilerFlags cc)
      . showString "\n      , isHaskell98 = " . shows (isHaskell98 cc)
      . showString "\n      }\n"
      where
        showPaths [] = showString "[]"
        showPaths [x] = showChar '[' . shows x . showChar ']'
        showPaths (x:xs) = showString "[" . shows x . showl xs
          where
            showl []     = showChar '\n'
                           . showString (take 23 (repeat ' '))
                           . showChar ']'
            showl (x:xs) = showChar '\n'
                           . showString (take 23 (repeat ' '))
                           . showChar ',' . shows x . showl xs
  showList [] = showString " []"
  showList (x:xs) = showString "\n    [ " . showsPrec 0 x . showl xs
      where showl []     = showString "    ]"
            showl (x:xs) = showString "    , " . showsPrec 0 x . showl xs


instance Show HmakeConfig where
  showsPrec p hmc = showString "HmakeConfig\n  { defaultCompiler = "
                    . shows (defaultCompiler hmc)
                    . showString "\n  , knownCompilers ="
                    . showList (knownCompilers hmc)
                    . showString "\n  }\n"

readConfig :: FilePath -> HmakeConfig
readConfig file = unsafePerformIO (safeReadConfig file)

safeReadConfig :: FilePath -> IO HmakeConfig
safeReadConfig file = do
    f <- readFile file
    config <- saferead file f
    return config
  where
    -- ensure the value read from the file is fully evaluated
    saferead :: FilePath -> String -> IO HmakeConfig
    saferead path s =
        let val = case [x | (x,t) <- reads s, ("","") <- lex t] of
                       [x] -> x
                       []  -> error ("hmake-config: can't parse config file '"
                                     ++ path++"'")
                       _   -> error ("hmake-config: ambiguous parse of config '"
                                     ++ path++"'")
        in (return $! val)


defaultConfigLocation :: FilePath
defaultConfigLocation = unsafePerformIO $ do
    machine <- getEnv "MACHINE"
    catch (do home <- getEnv "HOME"
              let loc = home ++ "/.hmakerc/" ++ machine
              ok <- doesFileExist loc
              if ok
                then return loc
                else ioError (userError ""))
          (\e-> return (globalConfigFileDir++"/"++machine++"/hmakerc"))


matchCompiler :: String -> HmakeConfig -> CompilerConfig
matchCompiler hc config =
    foldr (\comp other-> if compilerPath comp == hc then comp else other)
          (error ("hmake: the compiler '"++hc++"' is not known.\n"))
          (knownCompilers config)

usualCompiler :: HmakeConfig -> CompilerConfig
usualCompiler config = matchCompiler (defaultCompiler config) config
