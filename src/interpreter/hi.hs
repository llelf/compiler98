module Main where

import IO
import System
import Char
import Directory
import List

import HmakeConfig

--debug x = putStrLn ("DEBUG: "++x)
debug x = return ()

main = do
  options <- getArgs
  let opts = options ++ defaultOptions defaultCompiler
  putStrLn banner
  putStrLn (replicate 43 ' '++ "... Using compiler "++show defaultCompiler++" ...\n")
  putStrLn ("Type :? for help")
  hSetBuffering stdout NoBuffering
  load opts defaultCompiler "Prelude"
  toplevel opts defaultCompiler ["Prelude"]

done = return ()

toplevel options compiler modules = do
  putStr (head modules ++ "> ")
  s <- getLine
  if (null s || all isSpace s) then done else
    case head s of
      ':' -> let ws = words (tail s) in
             if (null ws) then done else
               case head (head ws) of
                 '!' -> do e <- system (unwords ((tail (head ws)):tail ws))
                           done
                 _   -> commands ws options compiler modules
      _   -> evaluate s options compiler modules
  toplevel options compiler modules

evaluate expr options compiler modules | "main " `isPrefixOf` expr = do
  compile options compiler (head modules)
     (run ("./"++head modules) (tail (words expr)))
evaluate expr options compiler modules = do
  let tmpfile = "/tmp/Main"
  f <- openFile (tmpfile++".hs") WriteMode
  hPutStr f (
    "module Main where\n\n" ++
    concatMap (\m-> "import "++m++"\n") modules ++
    "\n" ++ nonstdCoerce compiler ++
    "\n" ++ nonstdShow compiler ++
    "\nmain = let expr  = (" ++ expr ++ ")" ++
    "\n           shown = show expr" ++
    "\n       in case shown of" ++
    "\n           ('<':'<':'I':'O':_) -> coerce expr" ++
    "\n           _                   -> putStrLn shown" ++
    "\n")
  hClose f
  compile options compiler tmpfile (run tmpfile [])

{-
-- scopedEvaluate allows you to play with _unexported_ functions from the
-- current module.  This currently requires the restriction that "main" is
-- not otherwise defined in this module.
scopedEvaluate expr options compiler modules = do
  -- Ok, this isn't fully implemented yet, but the basic scenario is
  -- to copy the module to /tmp/Main.hs, removing a "module ... where"
  -- header if it exists (could replace with "module Main where", but
  -- that is unnecessary).  Add a "main = ..." definition to the end
  -- of the module, compile and run.  Additionally, it would be nice to
  -- search for an existing "main" definition and rename it to "_main",
  -- but that is (a) harder, and (b) potentially time-consuming.
  let tmpfile = "/tmp/Main"
  system ("echo module Main where >"++tmpfile++".hs")
  -- system ("cat " ...)
  hPutStr f (
    "module Main where\n\n" ++
    concatMap (\m-> "import "++m++"\n") modules ++
    "\n" ++ nonstdCoerce compiler ++
    "\n" ++ nonstdShow compiler ++
    "\nmain = let expr  = (" ++ expr ++ ")" ++
    "\n           shown = show expr" ++
    "\n       in case shown of" ++
    "\n           ('<':'<':'I':'O':_) -> coerce expr" ++
    "\n           _                   -> putStrLn shown" ++
    "\n")
  hClose f
  compile options compiler tmpfile (run tmpfile [])
  where
-}

compile options compiler file continue = do
  putStr "[Compiling..."
--debug ("hmake -"++show compiler++" -I. "++unwords options++" "++file++" >/dev/null")
  ok <- system ("hmake -"++show compiler++" -I. "++unwords options++" "++file++" >/dev/null")
  case ok of
    ExitSuccess -> do putStr (delete "[Compiling...")
                      continue
    _           -> putStrLn "...failed]"
 where
  delete []     = ""
  delete (_:xs) = "\BS \BS" ++ delete xs

run file args = system (file++" "++unwords args) >> done

commands :: [String] -> [String] -> Compiler -> [String] -> IO ()
commands ws options compiler modules =
  let target = tail ws in
  do
  command "quit" quit
  command "Quit" quit
  command "load"
      (let mods = if null target then ["Prelude"]
                  else (reverse ("Prelude":target))
       in do loadAll options compiler mods
             toplevel options compiler mods	-- explicit return with new module list
      )
  command "also"
      (let mods = if null target then ["Prelude"]
                  else (reverse target ++ modules)
       in do loadAll options compiler mods
             toplevel options compiler mods	-- explicit return with new module list
       )
  command "reload"
       (loadAll options compiler modules)
  command "freshen"
       (do makeclean ".o" modules
           loadAll options compiler modules)
  command "edit"
      (if null target then do
           e <- system ("$EDITOR " ++ head modules ++ ".hs")
           load options compiler (head modules)
       else do
           e <- system ("$EDITOR " ++ unwords target)
           loadAll options compiler modules
      )
  command "type"
      (do
          let tmpfile = "/tmp/Main"
          f <- openFile (tmpfile++".hs") WriteMode
          hPutStr f (
            "module Main where\n\n" ++
            concatMap (\m-> "import "++m++"\n") modules ++
            "\nmain = let expr  = (" ++ unwords target ++ ")" ++
            "\n       in putStrLn ("++ nonstdShowsType compiler++" expr \"\")"++
            "\n")
          hClose f
          compile options compiler tmpfile (run tmpfile [])
      )
  command "cd"
      (if null target then do
            dir <- getCurrentDirectory
            putStrLn ("Current directory: "++dir)
       else catch (setCurrentDirectory (head target)) print
      )
  command "dir" (getDirectoryContents "." >>= indent)
  command "ls" (getDirectoryContents "." >>= indent)
  command "pwd" (getCurrentDirectory >>= putStrLn)
  command "set"
      (do putStrLn ("Current settings:\n  "++unwords (options++target))
          toplevel (options++target) compiler modules)
  command "unset"
      (do let newopts = options \\ target
          putStrLn ("Current settings:\n  "++unwords newopts)
          toplevel newopts compiler modules)
  command "hc"
      (if null target then putStrLn ("Current compiler: "++show compiler)
       else let newcomp = toComp (head target)
                newopts = (options \\ defaultOptions compiler)
                                   ++ defaultOptions newcomp
            in
            if compilerKnown newcomp then do
               makeclean ".o" modules
               makeclean ".hi" modules
               loadAll newopts newcomp modules
               toplevel newopts newcomp modules	-- explicit return
            else do
               putStrLn ("Compiler "++head target++" not known/configured")
               putStrLn ("Current compiler: "++show compiler)
      )
  command "?" (putStrLn help)
  putStrLn ("[Unknown command :"++head ws++"]")
 where
  command :: String -> IO () -> IO ()
  command name action =
    if head ws `isPrefixOf` name then
      do action
         toplevel options compiler modules
    else done
  quit = do
     putStrLn "[Leaving hmake interactive...]"
     exitWith ExitSuccess
  indent = mapM_ (\x-> putStrLn ("  "++x))

loadAll options compiler modules =
  mapM_ (load options compiler) (reverse modules)

load options compiler mod = do
  normal ".lhs" (
    normal ".hs" (
      normal ".gc" (
        foldr  prelude
              (putStrLn ("[Module "++mod++" not found...]"))
              (preludePaths compiler))))
 where
  normal :: String -> IO () -> IO ()
  normal ext continue = do
    let file = mod++ext
    exist <- doesFileExist file
    if exist then do
        putStr ("[Found module... "++file++"] ")
        compile options compiler file (putChar '\n')
      else continue
  prelude :: String -> IO () -> IO ()
  prelude pp continue = do
    hi <- doesFileExist (pp++"/"++mod++".hi")
    if hi then putStrLn ("[Std   module... "++pp++"/"++mod++".hi]")
          else continue

makeclean ".o"  modules = system ("hmake -clean "++unwords modules)
makeclean ".hi" modules = system ("hmake -realclean "++unwords modules)

--makeclean ext modules = mapM_ (clean ext) modules
--  where
--  clean ext mod = do
--    let file = mod++ext
--    exist <- doesFileExist file
--    if exist then do
--        putStrLn ("[Removing    ... "++file++"]")
--        catch (removeFile file) print
--      else done

--fromOpt prefix opt =
--  if prefix `isPrefixOf` opt then Just (drop (length prefix) opt) else Nothing

{-
scope file mod = do
  system ("sed -e '/^module .* where.*/d' "++file++" >/tmp/"++file)
  return ("/tmp/"++file)
-}

#if defined(__HBC__)
banner = "hi - hmake interactive                (Version: "++hmakeVersion++")"
help   = "hi - help command does not work in hbc"

#else
banner = "\ 
\__   __                 __             _____________________________________
||   ||  ______    ___  || _  ____     hmake interactive (hi):
||___|| || || ||  ___|| ||/  ||__||       Copyright (c) May 2000
||---|| || || || ||__|| ||\\_ ||__         http://www.cs.york.ac.uk/fp/hmake/
||   ||                                Report bugs to: malcolm@cs.york.ac.uk
||   || Version: "++hmakeVersion++"    -------------------------------------"


help = "\ 
\Commands (can be abbreviated to first letter):
  <expr>		evaluate expression
  :type <expr>	 	show type of expression [nhc98 only]
  :quit			quit
  :Quit			quit
  :load mod [mod...]	load modules (note, not filenames)
  :load 		clear all modules
  :also mod [mod...]	load additional modules (note, not filenames)
  :reload 		repeat last load command
  :freshen 		remove, recompile, and reload all current modules
  :module mod		set module scope for evaluating expressions
  :edit file	 	edit filename
  :edit 	 	edit current module
  :cd dir		change directory
  :cd 			show current directory
  :dir 			list current directory
  :hc compiler		set Haskell compiler to use
  :set options		set hmake/compiler options
  :unset options	remove hmake/compiler options
  :!command 		shell escape
  :?	 		display this list of commands"
#endif
