-----------------------------------------------------------------------------
-- |
-- Module      :  Env
-- Copyright   :  ...
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Get the import lists of a module (why is this called "Env"?)
-- No longer used in 'hmake'.
-----------------------------------------------------------------------------

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

-- | Get the import lists for a module. (This parser is just a fast
--   hack. It would be nice to have a compiler option to output the
--   import list of a module.)

haskellImport :: Bool     -- ^ If true, allow the output to contain "Prelude"
              -> String   -- ^ The haskell source with "import Foo" lines
              -> [String] -- ^ The list of imported modules
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

