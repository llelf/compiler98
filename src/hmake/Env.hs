module Env(haskellImport) where
import Compat(assocFail, getenvi, splitAtElem, takeWord)
--import Either
import ListUtil(assocDef, chopList)
import Unlit
import Utils(dropto, id')
import Char

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

-- This parser is just a fast hack. It would be nice to have a compiler
-- option to output the import list of a module.

haskellImport kp =
    --filter notFromHbcLibrary .
    (if kp then id else filter notPrelude) .
    map (head . filter (/= "qualified") . tail) .
    filter ((== "import") . head) .
    map tokens .
    filter (/= "") .
    map unindent . lines

tokens s
  = case dropWhile isSpace s of
	"" -> []
	s' -> w : tokens s''
	     where (w, s'') = span ( \ s -> s=='_' || isAlphanum s) s'



notPrelude name =
    let prelude = "Prelude"
    in  take (length prelude) name /= prelude

unindent = dropto (not . isSpace)

