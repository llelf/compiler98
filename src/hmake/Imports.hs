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
  ( getImports, cpp, KeepState(..)
  ) where

import SymTab
import ParseLib
import ListUtil (takeUntil)
import Char
import Numeric  (readHex)

#if !defined(__HASKELL98__)
#define isAlphaNum isAlphanum
#endif

-- | Get the imports for this Haskell module.
getImports :: FilePath -- ^ The path to the module
           -> [String] -- ^ Definitions, from which to build a symbol table (for cpp)
           -> String   -- ^ The input file to be parsed for imports
           -> [String] -- ^ A list of imported modules
getImports fp defines inp =
  let syms = foldr (insertST.defval) emptyST defines
  in (leximports fp . cpp fp syms Keep . lines) inp

defval sym =
    let (s,d) = break (=='=') sym
    in if null d then (s,"1") else (s, tail d)

data KeepState = Keep | Drop Int

-- | Return just the list of lines that the real cpp would decide to keep.
cpp :: FilePath -> SymTab String -> KeepState -> [String] -> [String]
cpp _ _ _ [] = []

cpp fp syms Keep (l@('#':x):xs) =
         let ws = words x
             cmd = head ws
             sym = head (tail ws)
             val = let v = tail (tail ws) in
                   if null v then "1" else head v
             down = if definedST sym syms then (Drop 1) else Keep
             up   = if definedST sym syms then Keep else (Drop 1)
             keep str = if gatherDefined fp syms str then Keep else (Drop 1)
         in
         if      cmd == "define" then  cpp fp (insertST (sym,val) syms) Keep xs
         else if cmd == "undef"  then  cpp fp (deleteST sym syms) Keep xs
         else if cmd == "line"   then  cpp fp syms  Keep xs
         else if cmd == "ifndef" then  cpp fp syms  down xs
         else if cmd == "ifdef"  then  cpp fp syms  up   xs
         else if cmd == "if"     then  cpp fp syms (keep (drop 2 x)) xs
         else if cmd == "else"   ||
                 cmd == "elif"   then  cpp fp syms (Drop 1) xs
         else if cmd == "endif"  then  cpp fp syms  Keep xs
         else l: cpp fp syms Keep xs
                                     --error ("Unknown directive #"++cmd++"\n")
cpp fp syms Keep (x:xs) =
    x: cpp fp syms Keep xs

-- Old clauses:
--  | prefix "import " x  = modname (x:xs): cpp syms Keep xs
--  | otherwise           = cpp syms Keep xs

cpp fp syms (Drop n) (('#':x):xs) =
         let ws = words x
             cmd = head ws
             delse    | n==1      = Keep
                      | otherwise = Drop n
             dend     | n==1      = Keep
                      | otherwise = Drop (n-1)
             keep str | n==1      = if gatherDefined fp syms str then Keep
                                    else (Drop 1)
                      | otherwise = Drop n
         in
         if      cmd == "define" ||
                 cmd == "undef"  ||
                 cmd == "line"   then  cpp fp syms (Drop n) xs
         else if cmd == "ifndef" ||
                 cmd == "if"     ||
                 cmd == "ifdef"  then  cpp fp syms (Drop (n+1)) xs
         else if cmd == "elif"   then  cpp fp syms (keep (drop 4 x)) xs
         else if cmd == "else"   then  cpp fp syms delse xs
         else if cmd == "endif"  then  cpp fp syms dend xs
         else cpp fp syms (Drop n) xs
				   --error ("Unknown directive #"++cmd++"\n")
cpp fp syms d@(Drop n) (x:xs) =
  cpp fp syms d xs

-- | /leximports/ takes a cpp-ed list of lines and returns the list of imports
leximports :: FilePath -> [String] -> [String]
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
    endstring ('\\':'"':cs) = '\\':'"': endstring cs
    endstring ('"':cs) = '"': nestcomment 0 cs
    endstring (c:cs)   = c  : endstring cs
    endstring []       = []

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

  in (getmodnames . lines . nestcomment 0 . unlines)

----
gatherDefined fp st inp =
  case papply (parseBoolExp st) inp of
    []      -> error ("In file "++fp++"\n    cannot parse #if directive")
    [(b,_)] -> b
    _       -> error ("In file "++fp++"\n    ambiguous parse for #if directive")

parseBoolExp st =
  do  bracket (skip (char '(')) (parseBoolExp st) (skip (char ')'))
  +++
  do  skip (char '!')
      a <- parseBoolExp st
      parseCont (not a) st
  +++
  do  a <- skip (parseSym st)
      parseCont a st

parseSym st =
  do  skip (string "defined")
      sym <- bracket (skip (char '(')) (skip (many1 alphanum)) (skip (char ')'))
      return (definedST sym st)
  +++
  do  sym <- skip (many1 alphanum)
      parseComparison sym st

parseCont a st =
  do  skip (string "||")
      b <- first (skip (parseBoolExp st))
      return (a || b)
  +++
  do  skip (string "&&")
      b <- first (skip (parseBoolExp st))
      return (a && b)
  +++
  do  return a

parseComparison sym1 st =
  do  op <- parseOp st
      sym2 <- skip (many1 alphanum)
      let val1 = convert sym1 st
      let val2 = convert sym2 st
      return (op val1 val2)
  +++
  do  let val = lookupST sym1 st
      return (if val == Nothing || val == Just "0" then False else True)
  where
    convert sym st =
      case lookupST sym st of
        Nothing  -> safeRead sym
        (Just a) -> safeRead a
    safeRead s =
      case readHex s of
        []        -> 0 :: Integer
        ((n,_):_) -> n :: Integer

parseOp st =
  do  skip (string ">=")
      return (>=)
  +++
  do  skip (char '>')
      return (>)
  +++
  do  skip (string "<=")
      return (<=)
  +++
  do  skip (char '<')
      return (<)
  +++
  do  skip (string "==")
      return (==)
  +++
  do  skip (string "!=")
      return (/=)

----
