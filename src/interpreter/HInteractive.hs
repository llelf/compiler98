module Main where

import IO
import System
import Char
import Directory
import List
import Maybe

import HmakeConfig
import SimpleLineEditor (delChars, getLineEdited)
import LexModule
import Unlit

--debug x = putStrLn ("DEBUG: "++x)
debug x = return ()
done = return ()

data State = S { options  :: [String]
               , compiler :: Compiler
               , modules  :: [String]
               , scope    :: Maybe String	-- could be `elem` modules
               , scopeText  :: Maybe String	-- modified text of module
               }

main = do
  options <- getArgs
  let opts = options ++ defaultOptions defaultCompiler
  putStrLn banner
  putStrLn (replicate 43 ' '++ "... Using compiler "++show defaultCompiler++" ...\n")
  putStrLn ("Type :? for help")
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  let state = S { options=opts, compiler=defaultCompiler
                , modules=["Prelude"], scope=Nothing, scopeText=Nothing }
  load state "Prelude" (toplevel state)
  putStrLn "[Cannot continue without Prelude...]"
  exitWith (ExitFailure 1)

toplevel :: State -> IO ()
toplevel state = do
  case scope state of
    Nothing  -> putStr (head (modules state)++ "> ")
    Just mod -> putStr (mod++ "*> ")
  s <- getLineEdited
  if (null s || all isSpace s) then done else
    case head s of
      ':' -> let ws = words (tail s) in
             if (null ws) then done else
               case head (head ws) of
                 '!' -> do system (unwords ((tail (head ws)):tail ws)) >> done
                 _   -> commands ws state
      _   -> evaluate s (words s) state
  toplevel state

evaluate expr ("main":args) state =
  case scope state of
    Nothing ->
        let mod = head (modules state) in
        compile mod state (run ("./"++mod) args)
    Just _  -> do
        let tmpfile = "/tmp/Main"
        f <- openFile (tmpfile++".hs") WriteMode
        hPutStr f (fromJust (scopeText state) ++ "\nmain = _ain\n")
        hClose f
        compile tmpfile state (run tmpfile args)
evaluate expr _ state = do
  let tmpfile = "/tmp/Main"
  let modtext = fromMaybe "" (scopeText state)
  let scopem  = maybeToList (scope state)
  f <- openFile (tmpfile++".hs") WriteMode
  hPutStr f (
    "module Main where\n\n" ++
    concatMap (\m-> "import "++m++"\n") (modules state \\ scopem) ++
    "\n" ++ nonstdCoerceImport (compiler state) ++
    modtext ++
    "\n" ++ nonstdCoerce (compiler state) ++
    "\n" ++ nonstdShow (compiler state) ++
    "\nmain = let expr  = (" ++ expr ++ ")" ++
    "\n           shown = show expr" ++
    "\n       in case shown of" ++
    "\n           ('<':'<':'I':'O':_) -> coerce expr" ++
    "\n           _                   -> putStrLn shown" ++
    "\n")
  hClose f
  compile tmpfile state (run tmpfile [])


compile file state continue = do
  putStr "[Compiling..."
  ok <- system ("hmake -"++show (compiler state)++" -I. "
                ++unwords (options state)++" "++file++" >/dev/null")
  case ok of
    ExitSuccess -> do delChars "[Compiling..."
                      continue
    _           -> putStrLn "...failed]"

run file args = system (file++" "++unwords args) >> done

commands :: [String] -> State -> IO ()
commands ws state = let target = tail ws in do
  command "quit" quit
  command "Quit" quit
  command "load"
      (let mods = if null target then ["Prelude"]
                  else nub (reverse ("Prelude":target))
           newstate = state {modules=mods, scope=Nothing, scopeText=Nothing}
       in loadAll newstate (toplevel newstate)	-- explicitly return new modules
      )
  command "also"
      (let mods = if null target then ["Prelude"]
                  else nub (reverse target ++ modules state)
           newstate = state {modules=mods}	-- retain scope if applicable
       in loadAll newstate (toplevel newstate)	-- explicitly return new modules
       )
  command "reload" (loadAll state done)
  command "freshen"
       (do makeclean ".o" (modules state)
           loadAll state done)
  command "module"
       (if null target || length target > 1 then do
           putStrLn "You must give a single module name, in whose scope to evaluate."
        else let mod = head target in do
           loadScope state mod (\text-> toplevel (state { scope=(Just mod)
                                                    , scopeText=(Just text)}))
       )
  command "edit"
      (if null target then
         case scope state of
             Nothing  -> let mod = head (modules state) in
                         edit state mod (load state mod done)
             Just mod -> edit state mod (loadScope state mod (\_->done))
       else do e <- system ("${EDITOR-vi} " ++ unwords target)
               loadAll state done
      )
  command "type"
      (do let tmpfile = "/tmp/Main"
          f <- openFile (tmpfile++".hs") WriteMode
          hPutStr f (
            "module Main where\n\n" ++
            concatMap (\m-> "import "++m++"\n") (modules state) ++
            "\nmain = let expr  = (" ++ unwords target ++ ")" ++
            "\n       in putStrLn ("++ nonstdShowsType (compiler state)++" expr \"\")"++
            "\n")
          hClose f
          compile tmpfile state (run tmpfile [])
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
      (do let newopts = (options state) ++ target
          putStrLn ("Current settings:\n  "++unwords newopts)
          toplevel (state {options=newopts}) )
  command "unset"
      (do let newopts = (options state) \\ target
          putStrLn ("Current settings:\n  "++unwords newopts)
          toplevel (state {options=newopts}) )
  command "hc"
      (if null target then do
            putStrLn ("Current compiler: "++show (compiler state))
       else let newcomp = toComp (head target)
                newopts = ((options state) \\ defaultOptions (compiler state))
                                           ++ defaultOptions newcomp
            in
            if compilerKnown newcomp then do
               makeclean ".o" (modules state)
               makeclean ".hi" (modules state)
               let newstate = state {options=newopts, compiler=newcomp}
               loadAll newstate (toplevel newstate)	-- explicit return
            else do
               putStrLn ("Compiler "++head target++" not known/configured")
               putStrLn ("Current compiler: "++show (compiler state))
      )
  command "?" (putStrLn help)
  putStrLn ("[Unknown command :"++head ws++"]")
 where
  command :: String -> IO () -> IO ()
  command name action =
    if head ws `isPrefixOf` name then action >> toplevel state else done
  quit = do
     putStrLn "[Leaving hmake interactive...]"
     exitWith ExitSuccess
  indent = mapM_ (\x-> putStrLn ("  "++x))

-- find file (a generalisation of earlier implementation of 'load')
findF :: (Bool->String -> (a->IO ()) -> IO ())	-- action when file is found
         -> (String -> (a->IO ()) -> IO ())	-- action when .hi is found
         -> State
         -> String				-- module name
         -> (a -> IO ())			-- success continuation
         -> IO ()
findF action hiaction state mod success = do
  normal True ".lhs" (
    normal False ".hs" (
      normal False ".gc" (
        foldr  prelude
              (putStrLn ("[Module "++mod++" not found...]"))
              (preludePaths (compiler state)))))
 where
--normal :: Bool -> String -> IO b -> IO b
  normal lit ext continue = let file = mod++ext in do
    exist <- doesFileExist file
    if exist then action lit file success else continue
--prelude :: String -> IO b -> IO b
  prelude pp continue = let hifile = pp++"/"++mod++".hi" in do
    hi <- doesFileExist hifile
    if hi then hiaction hifile success else continue

loadAll :: State -> IO () -> IO ()
loadAll state success = foldr (load state) success (reverse (modules state))

load :: State -> String -> IO () -> IO ()
load state mod success =
  findF (\lit file success->
         do putStr ("[Found module... "++file++"] ")
            compile file state (putChar '\n')
            success ())
        (\hifile success->
         do putStrLn ("[Std   module... "++hifile++"]")
            success ())
        state mod (\()->success)
loadScope :: State -> String -> (String->IO ()) -> IO ()
loadScope state =
  findF (\lit file success-> let litf = if lit then (unlit file) else id in
         do putStrLn ("[Entering scope of module... "++file++"]")
            compile file state (do content <- readFile file
                                   success (lexmodule (litf content))))
        (\hifile success->
           putStrLn ("[Cannot enter std module... "++hifile++"]"))
        state

edit :: State -> String -> IO () -> IO ()
edit state mod success =
  findF (\lit file success-> system ("${EDITOR-vi} " ++ file) >> success ())
        (\hifile success->
           putStrLn ("[Cannot edit system file... "++hifile++"]"))
        state mod (\()->success)


--makeclean ".o"  modules = system ("hmake -clean -nhc98 "++unwords modules)
--makeclean ".hi" modules = system ("hmake -realclean -nhc98 "++unwords modules)

makeclean ext modules = mapM_ (clean ext) modules
  where
  clean ext mod = let file = mod++ext in do
    exist <- doesFileExist file
    if exist then do
        putStrLn ("[Removing    ... "++file++"]")
        catch (removeFile file) print
      else done

--fromOpt prefix opt =
--  if prefix `isPrefixOf` opt then Just (drop (length prefix) opt) else Nothing


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
