-----------------------------------------------------------------------------
-- |
-- Module      :  Imports
-- Copyright   :  Malcolm Wallace
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Get the imports for a single Haskell module after performing a cpp.
-----------------------------------------------------------------------------

module Imports
  ( getImports
  ) where

import Char
import ListUtil (takeUntil)
import CppIfdef (cppIfdef)

#if !defined(__HASKELL98__)
#define isAlphaNum isAlphanum
#endif

-- | Get the imports for this Haskell module.
getImports :: FilePath -- ^ The path to the module
           -> [String] -- ^ Definitions, from which to build a symbol table (for cpp)
           -> String   -- ^ The input file to be parsed for imports
           -> [String] -- ^ A list of imported modules
getImports fp defines = leximports fp
                        . unlines . map snd
                        . cppIfdef fp defines [] False False


-- | /leximports/ takes a cpp-ed input and returns the list of imports
leximports :: FilePath -> String -> [String]
leximports fp =
  let
    nestcomment n ('{':'-':cs) | n>=0 = nestcomment (n+1) cs
    nestcomment n ('-':'}':cs) | n>0  = nestcomment (n-1) cs
    nestcomment n (c:cs)       | n>0  = nestcomment n cs
    
    nestcomment 0 ('-':'}':cs)        =
        error ("In file "++fp++"\n"
               ++"    found close comment -} but no matching open {-")
    nestcomment 0 ('-':'-':cs)        =
        if null munch
          || isSpace nextchr
          || nextchr `elem` ",()[]{};\"'`"
          || isAlphaNum nextchr
        then nestcomment 0 (dropWhile (/='\n') munch)
        else '-':'-': nestcomment 0 cs
      where munch = dropWhile (=='-') cs
            nextchr = head munch
    nestcomment 0 ('\'':'"':'\'':cs)  = '\'':'"':'\'': nestcomment 0 cs
    nestcomment 0 ('\\':'"':cs)       = '\\':'"': nestcomment 0 cs
    nestcomment 0 ('"':cs)            = '"': endstring cs
    nestcomment 0 (c:cs)              = c: nestcomment 0 cs
    nestcomment 0 []                  = []
    nestcomment n []                  =
        error ("In file "++fp++"\n    found "++show n
               ++" open comments {- but no matching close -}")

    endstring ('\\':'\\':cs) = '\\':'\\': endstring cs
    endstring ('\\':'"':cs)  = '\\':'"': endstring cs
    endstring ('\\':w:cs)    | isSpace w = stringgap cs
    endstring ('"':cs) = '"': nestcomment 0 cs
    endstring (c:cs)   = c  : endstring cs
    endstring []       = []
    stringgap ('\\':cs) = endstring cs
    stringgap (c:cs)    = stringgap cs

    getmodnames (x:xs)
      | null x || all isSpace x  = getmodnames xs
      | otherwise =
        let ws = concatMap words (x:xs)	-- allow for import spanning several lines.
        in if not (null ws) && head ws == "import" then
               modname (tail ws): getmodnames xs
           else getmodnames xs
    getmodnames [] = []

    modname ws =
      let one = head ws
          two = head (tail ws)
      in
      if one == "qualified" then 
           takeUntil "(-{;" two
      else takeUntil "(-{;" one

  in (getmodnames . lines . nestcomment 0)

----
