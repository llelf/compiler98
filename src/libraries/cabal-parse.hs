module Main where

import IO
import Char
import System
import Text.ParserCombinators.Poly

-- "cabal-parse" is a simple way of reading information from a .cabal file
-- for use by other external programs, e.g. in a shell script or Makefile.
-- The cmdline arg "-slash" just replaces dots by slashes (e.g. to translate
-- module names from Haskell notation to filepaths).
main = do
    args <- getArgs
    (field,file,munge) <-
        case args of
          [field,file] -> return (map toLower field, file, id)
          ["-slash",field,file] -> return (map toLower field, file, slash)
          _ -> stop "Usage: cabal-parse [-slash] field file"
    content <- readFile file
    case runParser cabalFile (lexToken content) of
      (Left e, _)      -> stop e
      (Right cabal, _) -> do
          case lookup field cabal of
            Just rhs -> case runParser (fieldtype field) rhs of
                          (Left e, _) -> stop e
                          (Right result, _) -> putStrLn (munge result)
            Nothing  -> stop ("field "++field++" not present")

stop :: String -> IO a
stop s = do hPutStrLn stderr ("cabal-parse:\n"++indent 2 s)
            exitFailure
            return undefined

slash = map (\c -> if c=='.' then '/' else c)

-- Mapping from field-name to type of rhs
fieldtype :: String -> Parser Token String
fieldtype f = case f of
                "exposed-modules" -> unWords (list string)
                "other-modules"   -> unWords (list string)
                "c-sources"       -> unWords (list string)
                "hs-source-dirs"  -> unWords (list string)
                "build-depends"   -> unWords (commalist string)
                "extensions"      -> unWords (list string)
                "data-files"      -> unWords (list string)
                "extra-source-files" -> unWords (list string)
                "extra-tmp-files" -> unWords (list string)
                "homepage"        -> url
                "package-url"     -> url
                _                 -> freetext

-- Simple Lexer:
data Token = Word String | Colon | Comma
           | Newline | NewlineIndent
           deriving (Eq,Show)

lexToken :: String -> [Token]
lexToken    []        = []
lexToken (':':ss)     = Colon : lexToken ss
lexToken ('\n':ss)    = case ss of
                          ('-':'-':_)        -> NewlineIndent: lexToken ss
                          (c:cs) | isSpace c -> NewlineIndent: lexToken cs
                          _                  -> Newline : lexToken ss
lexToken ('-':'-':ss) = dropWhile (`notElem`[Newline,NewlineIndent])
                                  (lexToken ss)
lexToken (',':c:ss)   | isSpace c = Comma : lexToken (c:ss)
lexToken (c:ss)       | isSpace c = lexToken ss
lexToken (c:ss)       = accumulate [c] ss

accumulate :: String -> String -> [Token]
accumulate acc []     =  []
accumulate acc (c:cs) | isSpace c || c `elem` ":,"
                      = Word (reverse acc) : lexToken (c:cs)
accumulate acc (c:cs) = accumulate (c:acc) cs

-- Simple Parsers:

-- parse a bunch of keyword/value bindings, without further
-- interpretation of the rhs
cabalFile :: Parser Token [(String,[Token])]
cabalFile = many1 $ do
    (Word key) <- next
    Colon      <- next `adjustErr` (("Missing colon after "++key++"\n")++)
    rhs        <- manyFinally (satisfy (/=Newline)) (has Newline)
    return (map toLower key, rhs)

-- parse a single definite token
has :: Token -> Parser Token ()
has t = do satisfy (==t); return ()

-- parse something whilst ignoring non-significant newlines
acrossLines p = do many (has NewlineIndent)
                   x <- p
                   many (has NewlineIndent)
                   return x

-- parse freeform text
freetext :: Parser Token String
freetext = free ""
  where free s = do n <- next
                    case n of
                      Word w        -> free (reverse w++" "++s)
                      Colon         -> free (':':s)
                      Comma         -> free (',':s)
                      NewlineIndent -> free ('\n':s)
                 `onFail` return (reverse s)

-- parse URL, like freeform text only all spaces squashed
url :: Parser Token String
url = freetext >>= return . filter (not.isSpace)

-- parse a comma-separated list of items
commalist :: Parser Token a -> Parser Token [a]
commalist item = acrossLines (item `sepBy` acrossLines (has Comma))

-- parse a (possibly) comma-separated list of items
list :: Parser Token a -> Parser Token [a]
list item = acrossLines (item `sepBy` acrossLines (optional (has Comma)))

-- parse a single module name, directory name, package name, etc
string :: Parser Token String
string = do (Word w) <- next; return w

-- convert a list of strings into a whitespace-separated string
unWords :: Parser Token [String] -> Parser Token String
unWords p = p >>= return . unwords
