module RunAndReadStdout
  ( runAndReadStdout
  , basename, dirname
  ) where

import System (system,ExitCode(..),getEnv)
import IO (openFile,IOMode(ReadMode),hClose,hGetChar,bracket,isEOFError)
import Directory (removeFile)
import List (isPrefixOf)
import Platform (unsafePerformIO,getProcessID,withDefault,windows)

-- Generate a temporary filename unique to this process.
tmpfile :: String -> String
tmpfile root =
    let tmp = "TEMP" `withDefault` "/tmp" in
    if windows
      then (tmp++"/"++root++".tmp")
      else unsafePerformIO $ do
             p <- getProcessID
             return (tmp++"/"++root++"."++show p)

-- Run a shell command and collect its output.
runAndReadStdout :: String -> IO String
runAndReadStdout cmd = do
    let output = tmpfile "hmakeconfig"
    err <- system ("sh -c \""++cmd++"\" >"++output)
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

-- Analogues of the shell commands of the same name.
basename,dirname :: String -> String
basename = reverse .            takeWhile (not.(`elem`"\\/")) . reverse
dirname  = reverse . safetail . dropWhile (not.(`elem`"\\/")) . reverse
  where safetail [] = []
        safetail (_:x) = x

