{-# OPTIONS -fglasgow-exts #-}
-- main program for utility hmake-config
module Main where

import Compiler (HC(..))
import Config
import Directory (doesDirectoryExist,doesFileExist,removeFile,getPermissions
                 ,Permissions(..),renameFile,createDirectory)
import System (system,exitWith,ExitCode(..),getArgs,getEnv,getProgName)
import List (intersperse,nub,isPrefixOf,sort)
import Char (isDigit)
import Monad (foldM,when)
import Maybe (isJust,fromJust)
#ifdef __HBC__
import IOMisc (hPutStrLn)
#else
import IO (hPutStrLn)
#endif
import IO (stderr,isDoesNotExistError 
          ,openFile,IOMode(ReadMode),hClose,hGetChar,bracket,isEOFError)

#ifdef __HBC__
import UnsafePerformIO
#ifdef __HASKELL98__
import GetPid
getProcessID = getPid
#else
getProcessID = return 3154      -- arbitrary number
#endif
#endif
#ifdef __NHC__
import IOExtras (unsafePerformIO)
foreign import "getpid" getProcessID :: IO Int
#endif
#ifdef __GLASGOW_HASKELL__
import IOExts (unsafePerformIO)
foreign import "getpid" getProcessID :: IO Int
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
  if ghcsym < 500
    then do
      dir <- runAndReadStdout ("grep '^\\$libdir=' "++fullpath++" | head -1 | "
                               ++ "sed 's/^\\$libdir=[^/]*\\(.*\\).;/\\1/'")
      let incdir1 = dir++"/imports"
      ok <- doesDirectoryExist incdir1
      if ok
        then return CompilerConfig
			{ compilerStyle = Ghc
			, compilerPath  = ghcpath
			, compilerVersion = ghcversion
			, includePaths  = ghcDirs ghcsym incdir1
			, cppSymbols    = ["__GLASGOW_HASKELL__="++show ghcsym]
			, extraCompilerFlags = []
			, isHaskell98   = ghcsym>=400 }
        else do
          let incdir2 = dir++"/lib/imports"
          ok <- doesDirectoryExist incdir2
          if ok
            then return CompilerConfig
			{ compilerStyle = Ghc
			, compilerPath  = ghcpath
			, compilerVersion = ghcversion
			, includePaths  = ghcDirs ghcsym incdir2
			, cppSymbols    = ["__GLASGOW_HASKELL__="++show ghcsym]
			, extraCompilerFlags = []
			, isHaskell98   = ghcsym>=400 }
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
          idirs <- mapM (\p-> runAndReadStdout
                                  (ghcpkg++" --show-package="
                                   ++(if last p==',' then init p else p)
                                   ++" --field=import_dirs"))
                        (words pkgs)
          return CompilerConfig
			{ compilerStyle = Ghc
			, compilerPath  = ghcpath
			, compilerVersion = ghcversion
			, includePaths  = pkgDirs libdir idirs
			, cppSymbols    = ["__GLASGOW_HASKELL__="++show ghcsym]
			, extraCompilerFlags = []
			, isHaskell98   = True }
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


hcStyle :: String -> HC
hcStyle path = toCompiler (basename path)
  where
    toCompiler :: String -> HC
    toCompiler hc | "gcc" `isPrefixOf` hc = Nhc98
                  | "nhc" `isPrefixOf` hc = Nhc98
                  | "ghc" `isPrefixOf` hc = Ghc
                  | "hbc" `isPrefixOf` hc = Hbc
                  | otherwise             = Unknown hc

basename,dirname :: String -> String
basename = reverse .        takeWhile (/='/') . reverse
dirname  = reverse . safetail . dropWhile (/='/') . reverse
  where safetail [] = []
        safetail (_:x) = x


-- Generate a temporary filename unique to this process.
tmpfile :: String -> String
tmpfile root = unsafePerformIO $ do p <- getProcessID
                                    return ("/tmp/"++root++"."++show p)

-- Run a shell command and collect its output.
runAndReadStdout :: String -> IO String
runAndReadStdout cmd = do
    let output = tmpfile "hmakeconfig"
    err <- system ("sh -c \""++cmd++" >"++output++"\"")
    case err of
        ExitFailure _ -> ioError (userError ("Command ("++cmd++") failed"))
	_ -> return ()
    s <- readFileNow output
    removeFile output	-- file will not be removed until readFile closes it
    return (safeinit s)	-- strip trailing newline added by shell
  where
    safeinit []     = []
    safeinit ['\n'] = []
    safeinit [x]    = [x]
    safeinit (x:xs) = x: safeinit xs
    readFileNow f = bracket (openFile f ReadMode) hClose hGetContentsNow 
    hGetContentsNow h = loop ""
      where loop cts = do x <- catch (hGetChar h >>= (return . Just))
                                     (\e->if isEOFError e 
                                          then return $ Nothing
                                          else ioError e)
                          case x of Just c  -> loop (c:cts)
                                    Nothing -> return $ reverse cts

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
