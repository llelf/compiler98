module Argv(Goal(..),decode,stripGoal) where
import ListUtil(lconcatMap)

data Goal = Object  String
          | Program String

instance Show Goal where
   showsPrec n (Object  name) = showString (name++".o")
   showsPrec n (Program name) = showString name

stripGoal (Object name)  = name
stripGoal (Program name) = name

decode progArgs = 
   (zdefs, defs, dflag, ifdebug, ifnotopt, ifopt, isopt, goalDir, ignoreHi,
    pathSrc, pathPrel, modules)
 where
  isflag [] = False
  isflag (c : _) = c == '+' || c == '-'

  flags = (map tail . filter isflag) progArgs

  wrapGoal file = case reverse file of
                     ('s':'h':'l':'.':name) -> Object  (reverse name)
                     ('c':'g':'.':name) -> Object  (reverse name)
                     ('s':'h':'.':name) -> Object  (reverse name)
                     ('o':'.':name)     -> Object  (reverse name)
                     name               -> Program (reverse name)

  modules = (map wrapGoal . filter (not . isflag)) progArgs

  isopt opt = opt `elem` flags

  ifopt opts s = if or (map isopt opts) then s else ""

  ifnotopt opts s =
    if not (or (map isopt opts)) then s else ""

  ifdebug = ifopt []

  goalDir =
      case filter (\v -> head v == 'd') flags of
          []  -> ""
          [x] -> tail x
          _   -> error "Only one -dobjdir allowed!\n" 

  dflag = isopt "od"

  ignoreHi = (map tail . filter (\v -> head v == 'N')) flags
  defs     = (map tail . filter (\v -> head v == 'D')) flags
  zdefs    = (map tail . filter (\v -> head v == 'Z')) flags
  pathSrc  = (map tail . filter (\v -> head v == 'I')) flags ++
             (map tail . filter (\v -> head v == 'i')) flags
  pathPrel = (map tail . filter (\v -> head v == 'P')) flags

