module SimpleLineEditor
  ( getLineEdited	--	:: {- [String] -> -} IO String
  , delChars		--	:: String -> IO ()
  ) where

import IO
import Monad (when)
import Char
#if USE_READLINE
import Readline
#endif

delChars :: String -> IO ()
delChars []     = return ()
delChars (_:xs) = do putStr "\BS \BS"
                     delChars xs

-- getLineEdited relies on having the terminal in non-buffered mode,
-- therefore please ensure that `hSetBuffering NoBuffering' is called
-- before using this.

#if USE_READLINE

getLineEdited :: String -> IO (Maybe String)
getLineEdited prompt = do
  ms <- readline prompt
  case ms of 
    Nothing -> return ms
    Just s  -> when (not (all isSpace s)) (addHistory s) >> return ms

#else

getLineEdited :: String -> {- [String]-> -}  IO (Maybe String)
getLineEdited prompt {-history-} = putStr prompt >> gl "" 0
  where
  gl s 0 = do		-- s is accumulated line, 0 is cursor position
    c <- hGetChar stdin
    case c of
      '\n'   -> return (Just (reverse s))
      '\DEL' -> do delChars "^?"
                   gl s 0
      '\BS'  -> do delChars "^H"
                   if not (null s) then do
                       putStr ("\BS")
                       gl s 1
                     else gl s 0
  --  '\^J'  -> do delChars "^J"
  --               putChar '\n'
  --               return (reverse s)
      '\^K'  -> do putChar '\n'
                   return (Just "")
      '\^L'  -> do delChars "^L"
                   gl s 0
      c      -> gl (c:s) 0

  gl s n = do		-- s is accumulated line, n(/=0) is cursor position
    c <- hGetChar stdin
    case c of
      '\n'   -> return (Just (reverse s))
      '\DEL' -> do let n1 = n-1
                   delChars "^?"
                   putStr (reverse (take n1 s) ++ " ")
                   putStr (replicate (n1+1) '\BS')
                   gl (take n1 s ++ drop n s) n1
      '\BS'  -> do let n1 = n+1
                   delChars "^H"
                   if n1 <= length s then do
                       putStr (reverse (take n s)++" ")
                       putStr (replicate (n1+1) '\BS')
                       gl s n1
                     else do
                       putStr (reverse s++" ")
                       putStr (replicate (n+1) '\BS')
                       gl s n
  --  '\^J'  -> do delChars "^J"
  --               putStr (reverse (take n s))
  --               putChar '\n'
                   return (Just (reverse s))
      '\^K'  -> do putChar '\n'
                   return (Just "")
      '\^L'  -> do let n1 = n-1
                   delChars "^L"
                   putStr (reverse (take n s))
                   putStr (replicate n1 '\BS')
                   gl s n1
      c      -> do putStr (reverse (take n s))
                   putStr (replicate n '\BS')
                   gl (take n s ++ c: drop n s) n

#endif -- USE_READLINE
