-- main program for utility hmake-config
module Main where

import Compiler (HC(..))
import Config
import RunAndReadStdout (runAndReadStdout, basename, dirname, unsafePerformIO)
import Directory (doesDirectoryExist,doesFileExist,removeFile,getPermissions
                 ,Permissions(..),renameFile,createDirectory)
import System (exitWith,ExitCode(..),getArgs,getEnv,getProgName)
import List (intersperse,nub,isPrefixOf,sort)
import Char (isDigit)
import Monad (foldM,when)
import Maybe (isJust,fromJust)
import IO (stderr,isDoesNotExistError)
#ifdef __HBC__
import IOMisc (hPutStrLn)
#else
import IO (hPutStrLn)
#endif


main = do
  args <- getArgs
  (config,file,args) <- readConfigFile args
  newconfig <- case args of
    ["list"]       -> do putStrLn ("Config file is:\n    "++file)
                         putStrLn "Known compilers:"
                         mapM_ putStrLn
                               ((reverse . sort
                                 . map (\c-> "    "++compilerPath c
                                             ++"\t("++compilerVersion c++")"))
                                (knownCompilers config))
                         putStrLn "Default compiler:"
                         putStrLn ("    "++defaultCompiler config)
                         return Nothing
    [hc]           -> do -- no command, assume 'add'
                         cc <- configure (hcStyle hc) hc
                         return (Just (config { knownCompilers =
                                              nub (cc: knownCompilers config)}))
    ["add",hc]     -> do cc <- configure (hcStyle hc) hc
                         return (Just (config { knownCompilers =
                                              nub (cc: knownCompilers config)}))
    ["delete",hc]  -> delete config (hcStyle hc) hc
    ["default",hc] -> mkDefault config (hcStyle hc) hc
    _ -> do hPutStrLn stderr ("Usage: hmake-config [configfile] list\n"
                 ++"       hmake-config [configfile] [add|delete|default] hc\n"
                 ++"                  -- hc is name/path of a Haskell compiler")
            exitWith (ExitFailure 1)
            return Nothing -- never reached
  --renameFile file (file++"~")
  when (isJust newconfig) (writeFile file (show (fromJust newconfig)))
  exitWith ExitSuccess

 where
    readConfigFile :: [String] -> IO (HmakeConfig,FilePath,[String])
    readConfigFile args = do
      machine <- catch (getEnv "MACHINE")
                       (\e-> runAndReadStdout "harch")
      case args of
        [file,"list"] -> do config <- parseConfigFile machine file
                            return (config, file, tail args)
        [file,_,_] -> do config <- parseConfigFile machine file
                         return (config, file, tail args)
        [] -> do global <- getEnv "HMAKEDIR"
                 hPutStrLn stderr ("Usage: hmake-config [configfile] list\n"
                  ++"       hmake-config [configfile] [add|delete|default] hc\n"
                  ++"              -- hc is name/path of a Haskell compiler\n"
                  ++"  default configfile is:\n    "
                  ++global++"/"++machine++"/hmakerc")
                 exitWith (ExitFailure 1)
        _ -> do home <- getEnv "HOME"
                let path = home++"/.hmakerc/"++machine
                config <- parseConfigFile machine path
                return (config, path, args)

    parseConfigFile :: String -> FilePath -> IO HmakeConfig
    parseConfigFile machine path =
      catch (safeReadConfig path)
            (\e-> if isDoesNotExistError e
                  then do
                    hPutStrLn stderr ("hmake-config: Warning: "
                                      ++"Config file not found:\n  '"
                                      ++path++"'")
                    globalDir <- getEnv "HMAKEDIR"
                    let global = globalDir++"/"++machine++"/hmakerc"
                    if path == global
                      then newConfigFile path
                      else do
                        hPutStrLn stderr ("hmake-config: Copying from\n  '"
                                          ++global++"'.")
                        catch (do config <- safeReadConfig global
                                  writeFile path (show config)
                                  return config)
                              (\e-> if isDoesNotExistError e
                                    then do
                                      hPutStrLn stderr
                                        ("hmake-config: Warning: "
                                         ++"System config file not found:\n  '"
                                         ++global++"'")
                                      newConfigFile path
                                    else ioError e)
                  else ioError e)
    newConfigFile path = do
      hPutStrLn stderr ("hmake-config: Starting new config from scratch.")
      let config = HmakeConfig {defaultCompiler="unknown", knownCompilers=[]}
      catch (writeFile path (show config))
            (\e -> if isDoesNotExistError e	-- fails because no directory
                   then do createDirectory (dirname path)
                           writeFile path (show config)
                   else ioError e)		-- fails for other reason
      return config


delete, mkDefault :: HmakeConfig -> HC -> String -> IO (Maybe HmakeConfig)
delete config hc path
  | path == defaultCompiler config = do
        hPutStrLn stderr ("hmake-config: cannot delete\n  '"++path
                          ++"'\n  because it is the default compiler.")
        exitWith (ExitFailure 3)
        return undefined -- never reached
  | otherwise =
        return (Just (config { knownCompilers =
                                   filter (\cc-> compilerPath cc /= path)
                                          (knownCompilers config) }))
mkDefault config hc path
  | path `elem` map compilerPath (knownCompilers config)
              = return (Just (config { defaultCompiler = path }))
  | otherwise = do hPutStrLn stderr ("hmake-config: compiler not known:\n  '"
                                     ++path++"'")
                   exitWith (ExitFailure 2)
                   return undefined -- never reached

-- configure for each style of compiler
configure :: HC -> String -> IO CompilerConfig
configure Ghc ghcpath = do
  fullpath <- which ghcpath
  ghcversion <- runAndReadStdout (ghcpath ++ " --version 2>&1 | "
                                  ++"sed 's/^.*version[ ]*\\([0-9.]*\\).*/\\1/'"
                                 )
  let ghcsym = (read (take 3 (filter isDigit ghcversion))) :: Int
      config  = CompilerConfig
			{ compilerStyle = Ghc
			, compilerPath  = ghcpath
			, compilerVersion = ghcversion
			, includePaths  = undefined
			, cppSymbols    = ["__GLASGOW_HASKELL__="++show ghcsym]
			, extraCompilerFlags = []
			, isHaskell98   = ghcsym>=400 }
  if ghcsym < 500
    then do
      dir <- runAndReadStdout ("grep '^\\$libdir=' "++fullpath++" | head -1 | "
                               ++ "sed 's/^\\$libdir=[^/]*\\(.*\\).;/\\1/'")
      let incdir1 = dir++"/imports"
      ok <- doesDirectoryExist incdir1
      if ok
        then return config{ includePaths = ghcDirs ghcsym incdir1 }
        else do
          let incdir2 = dir++"/lib/imports"
          ok <- doesDirectoryExist incdir2
          if ok
            then return config{ includePaths = ghcDirs ghcsym incdir2 }
            else do ioError (userError ("Can't find ghc includes at\n  "
                                        ++incdir1++"\n  "++incdir2))
    else do -- 5.00 and above
      pkgcfg <- runAndReadStdout (fullpath++" -v 2>&1 | head -2 | tail -1 |"
                                  ++" cut -c28- | head -1")
      let libdir  = dirname pkgcfg
          incdir1 = libdir++"/imports"
      ok <- doesDirectoryExist incdir1
      if ok
        then do
          let ghcpkg = dirname fullpath++"/ghc-pkg-"++ghcversion
          pkgs <- runAndReadStdout (ghcpkg++" --list-packages")
          let pkgsOK = filter (`elem`["std","base","haskell98"]) (deComma pkgs)
          idirs <- mapM (\p-> runAndReadStdout
                                  (ghcpkg++" --show-package="++p
                                   ++" --field=import_dirs"))
                        pkgsOK
          return config{ includePaths = pkgDirs libdir idirs }
        else do ioError (userError ("Can't find ghc includes at "++incdir1))
 where
    ghcDirs n root | n < 400   = [root]
                   | n < 406   = map ((root++"/")++) ["std","exts","misc"
                                                     ,"posix"]
                   | otherwise = map ((root++"/")++) ["std","lang","data","net"
                                                     ,"posix","num","text"
                                                     ,"util","hssource"
                                                     ,"win32","concurrent"]
    pkgDirs libdir dirs =
        map (\dir-> if "$libdir" `isPrefixOf` dir
                    then libdir++drop 7 dir
                    else dir)
            (concatMap words dirs)
    deComma pkgs = map (\p-> if last p==',' then init p else p) (words pkgs)

configure Nhc98 nhcpath = do
  fullpath <- which nhcpath
  nhcversion <- runAndReadStdout (nhcpath
                                  ++" --version 2>&1 | cut -d' ' -f2 | head -1")
  dir <- runAndReadStdout ("grep '^NHC98INCDIR' "++fullpath
                           ++ "| cut -c27- | cut -d'}' -f1 | head -1")
  return CompilerConfig { compilerStyle = Nhc98
			, compilerPath  = nhcpath
			, compilerVersion = nhcversion
			, includePaths = [dir]
			, cppSymbols    = ["__NHC__"]
			, extraCompilerFlags = []
			, isHaskell98   = True
			}
configure Hbc hbcpath = do
  let field n = "| cut -d' ' -f"++show n++" | head -1"
  fullpath <- which hbcpath
  wibble <- runAndReadStdout (hbcpath ++ " -v 2>&1 " ++ field 2)
  hbcversion <-
      case wibble of
        "version" -> runAndReadStdout (hbcpath ++ " -v 2>&1 " ++ field 3)
        _         -> runAndReadStdout (hbcpath ++ " -v 2>&1 " ++ field 4)
  dir <- catch (getEnv "HBCDIR")
               (\e-> catch (getEnv "LMLDIR")
                           (\e-> return "/usr/local/lib/lmlc"))
  return CompilerConfig { compilerStyle = Hbc
			, compilerPath  = hbcpath
			, compilerVersion = hbcversion
			, includePaths = map ((dir++"/")++)
                                              ["hlib1.3","hbc_library1.3"]
			, cppSymbols    = ["__HBC__"]
			, extraCompilerFlags = []
			, isHaskell98   = ((hbcversion!!7) >= '5')
			}
configure (Unknown hc) hcpath = do
    hPutStrLn stderr ("hmake-config: the compiler\n  '"++hcpath
                      ++"'\n  does not look like a Haskell compiler.")
    exitWith (ExitFailure 4)
    return undefined  -- never reached

-- Work out which basic compiler.
hcStyle :: String -> HC
hcStyle path = toCompiler (basename path)
  where
    toCompiler :: String -> HC
    toCompiler hc | "gcc" `isPrefixOf` hc = Nhc98
                  | "nhc" `isPrefixOf` hc = Nhc98
                  | "ghc" `isPrefixOf` hc = Ghc
                  | "hbc" `isPrefixOf` hc = Hbc
                  | otherwise             = Unknown hc

-- Get an environment variable if it exists, or default to given string
withDefault name def = unsafePerformIO $
   catch (do val <- getEnv name
             if null val then return def else return val)
         (\e-> return def)

-- Some variables imported from the shell environment
builtby = "BUILTBY" `withDefault` "unknown"

-- Emulate the shell `which` command.
which :: String -> IO String
which cmd =
  let dir = dirname cmd
  in case dir of
    "" -> do -- search the shell environment PATH variable for candidates
             val <- getEnv "PATH"
             let dirs = splitPath "" val
             search <- foldM (\a dir-> testFile a (dir++"/"++cmd)) Nothing dirs
             case search of
               Just x  -> return x
               Nothing -> ioError (userError (cmd++" not found"))
    _  -> do perms <- getPermissions cmd
             if executable perms
               then return cmd
               else ioError (userError (cmd++" is not executable"))
  where
    splitPath :: String -> String -> [String]
    splitPath acc []         = [reverse acc]
    splitPath acc (':':path) = reverse acc : splitPath "" path
    splitPath acc (c:path)   = splitPath (c:acc) path

    testFile :: Maybe String -> String -> IO (Maybe String)
    testFile gotit@(Just _) path = return gotit
    testFile Nothing path = do
        ok <- doesFileExist path
        if ok
          then do
            perms <- getPermissions path
            if executable perms
              then return (Just path)
              else return Nothing
          else return Nothing
