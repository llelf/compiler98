module Imports
  ( getImports
  ) where

import SymTab
import ParseLib
import ListUtil (takeUntil)
import Char     (isSpace)
import Numeric  (readHex)

getImports :: [String] -> String -> [String]
getImports defines inp =
  let syms = foldr (insertST.defval) emptyST defines
  in cpp syms Keep (lines inp)

defval sym =
    let (s,d) = break (=='=') sym
    in if null d then (s,"1") else (s, tail d)

data KeepState = Keep | Drop Int

cpp :: SymTab String -> KeepState -> [String] -> [String]
cpp _ _ [] = []

cpp syms Keep (('#':x):xs) =
         let ws = words x
             cmd = head ws
             sym = head (tail ws)
             val = let v = tail (tail ws) in
                   if null v then "1" else head v
             down = if definedST sym syms then (Drop 1) else Keep
             up   = if definedST sym syms then Keep else (Drop 1)
             keep str = if gatherDefined syms str then Keep else (Drop 1)
         in
         if      cmd == "define" then  cpp (insertST (sym,val) syms) Keep xs
         else if cmd == "undef"  then  cpp (deleteST sym syms) Keep xs
         else if cmd == "line"   then  cpp syms  Keep xs
         else if cmd == "ifndef" then  cpp syms  down xs
         else if cmd == "ifdef"  then  cpp syms  up   xs
         else if cmd == "if"     then  cpp syms (keep (drop 2 x)) xs
         else if cmd == "else"   ||
                 cmd == "elif"   then  cpp syms (Drop 1) xs
         else if cmd == "endif"  then  cpp syms  Keep xs
         else cpp syms Keep xs    --error ("Unknown directive #"++cmd++"\n")
cpp syms Keep (x:xs) =
  if prefix "import " x then
       modname x: cpp syms Keep xs
--else if any (not.isSpace) x then []	-- a feeble (and incorrect) effort to prune once all imports have been seen
  else cpp syms Keep xs

cpp syms (Drop n) (('#':x):xs) =
         let ws = words x
             cmd = head ws
             delse    | n==1      = Keep
                      | otherwise = Drop n
             dend     | n==1      = Keep
                      | otherwise = Drop (n-1)
             keep str | n==1      = if gatherDefined syms str then Keep
                                    else (Drop 1)
                      | otherwise = Drop n
         in
         if      cmd == "define" ||
                 cmd == "undef"  ||
                 cmd == "line"   then  cpp syms (Drop n) xs
         else if cmd == "ifndef" ||
                 cmd == "if"     ||
                 cmd == "ifdef"  then  cpp syms (Drop (n+1)) xs
         else if cmd == "elif"   then  cpp syms (keep (drop 4 x)) xs
         else if cmd == "else"   then  cpp syms delse xs
         else if cmd == "endif"  then  cpp syms dend xs
         else cpp syms (Drop n) xs   --error ("Unknown directive #"++cmd++"\n")
cpp syms d@(Drop n) (x:xs) =
  cpp syms d xs

modname s =
  let ws = words s
      one = head (tail ws)
      two = head (tail (tail ws))
  in
  if one == "qualified" then 
       takeUntil "(-{;" two
  else takeUntil "(-{;" one

prefix :: String -> String -> Bool
prefix [] y = True
prefix x [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

----
gatherDefined st inp =
  case papply (parseBoolExp st) inp of
    []      -> error "cannot parse #if directive"
    [(b,_)] -> b
    _       -> error "ambiguous parse for #if directive"

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
  do  skip (char '>')
      return (>)
  +++
  do  skip (string ">=")
      return (>=)
  +++
  do  skip (char '<')
      return (<)
  +++
  do  skip (string "<=")
      return (<=)
  +++
  do  skip (string "==")
      return (==)
  +++
  do  skip (string "!=")
      return (/=)

----
