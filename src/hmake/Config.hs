module Config where

import Compiler
import System (getEnv)
import Directory (doesFileExist,doesDirectoryExist,createDirectory)
import Monad (when)
import List (nub)
#if defined(__NHC__)
import NHC.IOExtras (unsafePerformIO)
#elif defined(__GLASGOW_HASKELL__)
import IOExts (unsafePerformIO)
#elif defined(__HBC__)
import UnsafePerformIO (unsafePerformIO)
#endif

----
data PersonalConfig = PersonalConfig
  { globalConfig :: HmakeConfig
  , localConfig  :: Maybe HmakeConfig
  }

defaultComp :: PersonalConfig -> FilePath
defaultComp conf =
  case localConfig conf of
    Just local -> defaultCompiler local
    Nothing    -> defaultCompiler (globalConfig conf)

knownComps  :: PersonalConfig -> [CompilerConfig]
knownComps conf =
  case localConfig conf of
    Just local -> nub (knownCompilers local ++ globals)
    Nothing    -> globals
  where
    globals = knownCompilers (globalConfig conf)

----
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

----
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


readPersonalConfig :: (FilePath,Maybe FilePath) -> IO PersonalConfig
readPersonalConfig (global,local) = do
    g <- safeReadConfig global
    l <- case local of
           Just lo -> do l <- safeReadConfig lo
                         return (Just l)
           Nothing -> return Nothing
    return PersonalConfig { globalConfig = g , localConfig  = l }


defaultConfigLocation :: Bool -> (FilePath, Maybe FilePath)
defaultConfigLocation create = unsafePerformIO $ do
    machine <- getEnv "MACHINE"
    global <- getEnv "HMAKEDIR"
    let g = global++"/"++machine++"/hmakerc"
    catch (do home <- getEnv "HOME"
              let dir = home ++ "/.hmakerc"
                  loc = dir ++"/"++ machine
              exists <- doesFileExist loc
              if exists
                then return (g, Just loc)
                else if create then
                   do ok <- doesDirectoryExist dir
                      when (not ok) (createDirectory dir)
                      return (g, Just loc)
                else return (g, Nothing))
          (\e-> return (g, Nothing))


matchCompiler :: String -> PersonalConfig -> CompilerConfig
matchCompiler hc conf =
  case localConfig conf of
      Just local -> foldr search global (knownCompilers local)
      Nothing    -> global
  where
      search comp other = if compilerPath comp == hc then comp else other
      global = foldr search
                     (error ("hmake: the compiler '"++hc++"' is not known.\n"))
                     (knownCompilers (globalConfig conf))

compilerKnown :: String -> PersonalConfig -> Bool
compilerKnown hc config =
    any (\comp -> compilerPath comp == hc) known
  where
    known = knownCompilers (globalConfig config) ++
            case localConfig config of
              Just l  -> knownCompilers l
              Nothing -> []

usualCompiler :: PersonalConfig -> CompilerConfig
usualCompiler config = matchCompiler def config
  where def = case localConfig config of
                Just l  -> defaultCompiler l
                Nothing -> defaultCompiler (globalConfig config)


{-
matchCompiler :: String -> HmakeConfig -> CompilerConfig
matchCompiler hc config =
    foldr (\comp other-> if compilerPath comp == hc then comp else other)
          (error ("hmake: the compiler '"++hc++"' is not known.\n"))
          (knownCompilers config)

compilerKnown :: String -> HmakeConfig -> Bool
compilerKnown hc config =
    any (\comp -> compilerPath comp == hc) (knownCompilers config)

usualCompiler :: HmakeConfig -> CompilerConfig
usualCompiler config = matchCompiler (defaultCompiler config) config
-}
