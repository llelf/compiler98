module CaseHelp(Pattern(..), alt2fun,getTrans,sortCon,sortInt,splitPattern,varExp,varExpT
		,dropPatAs, isExpVar, needLet) where

import Syntax
import PosCode
import State
import IntState
import AssocTree
import TokenId
import Info
import CaseLib
import SyntaxPos
import SyntaxUtil

alt2fun :: Alt Int -> Fun Int
alt2fun (Alt pat gdexps decls) = Fun [pat] gdexps decls

noVar = error "noVar"

-- The following two functions could be simplified no.
-- the expression is always simple if trans isn't empty.

varExpT :: [a] -> PosExp -> CaseFun ([a],Int,PosExp->PosExp,PosExp)
varExpT [] e =
  unitS ([],noVar,id,e)
varExpT trans e@(PosVar pos v) =
  unitS (trans,v,id,e)
varExpT trans e =
 caseUnique >>>= \ v ->
 let pos = getPos e
 in unitS (trans,v,PosExpLet pos [(v,PosLambda pos [] [] e)],PosVar pos v)

varExp :: PosExp -> CaseFun (Int,PosExp->PosExp,PosExp)
varExp  e@(PosVar pos v) =
  unitS (v,id,e)
varExp  e =
 caseUnique >>>= \ v ->
 let pos = getPos e
 in unitS (v,PosExpLet pos [(v,PosLambda pos [] [] e)],PosVar pos v)

getTrans :: ExpI -> [Int]
getTrans (ExpVar _ ident) = [ident]
getTrans (PatAs _ ident p) = ident : getTrans p
getTrans _ = []

fstPat :: Fun Int -> ExpI
fstPat (Fun (p:ps) gdexps decls) = p

isIf :: ExpI -> Bool
isIf p = not (isVar p || isCon p || isExpInt p || isNK p || isExpIrr p)

data Pattern =
    PatternVar [(Exp Int,Fun Int)]
  | PatternCon [(Exp Int,Fun Int)]
  | PatternInt [(Exp Int,Fun Int)]
  | PatternNK  [(Exp Int,Fun Int)]
  | PatternIf  [(Exp Int,Fun Int)]
  | PatternIrr  (Exp Int,Fun Int)

patternTypes :: [(ExpI->Bool ,[(ExpI,Fun Int)] -> [Pattern])]
patternTypes =
	[(isVar,(:[]).PatternVar)
	,(isCon,(:[]).PatternCon)
	,(isExpInt,(:[]).PatternInt)
	,(isNK,(:[]).PatternNK)
	,(isExpIrr,map PatternIrr)
	,(isIf,(:[]).PatternIf)]

splitPattern :: (ExpI,ExpI) -> IntState -> [Fun Int] -> [Pattern]
splitPattern list state funs = 
  (split patternTypes (map (splitFuns list state) funs))
 where
  split pt [] = []
  split [] funs = split patternTypes funs
  split ((p,t):pt) funs =
    case span (p . dropPatAs . fst) funs of
      ([],funs) -> split pt funs
      (vs,funs) -> t vs ++ split pt funs

splitFuns :: (ExpI,ExpI) -> IntState -> Fun Int -> (ExpI,Fun Int)
splitFuns list state (Fun (p:ps) gdexps decls) =
  (simplifyPat list state p,Fun ps gdexps decls)

simplifyPat :: (ExpI,ExpI) -> IntState -> ExpI -> ExpI
simplifyPat list state (ExpList pos ls) =
	case ls of
	  [] -> fst list
	  (x:xs) -> ExpApplication pos [snd list,x,ExpList pos xs]
simplifyPat list state (ExpLit pos (LitString b str)) =
	case str of
	  [] -> fst list
	  (x:xs) -> ExpApplication pos [snd list, ExpLit pos (LitInt b (fromEnum x)),ExpLit pos (LitString b xs)]
simplifyPat list state (ExpLit pos (LitChar b i)) = ExpLit pos (LitInt b (fromEnum i))
simplifyPat list state (PatAs pos ident pat) = PatAs pos ident (simplifyPat list state pat)
simplifyPat list state (ExpApplication pos (ExpApplication _ es':es)) = ExpApplication pos  (map (simplifyPat list state) (es'++es))
simplifyPat list state (ExpDict pat) = simplifyPat list state pat
simplifyPat list state pat = pat

sortInt :: [(ExpI,Fun Int)] -> [(Int,[Fun Int])]
sortInt funs =
  (stableSort
  .map ( \ (pat,fun) -> (getInt pat,fun))
  ) funs
 where
  getInt (PatAs _ _ p) = getInt p
  getInt (ExpLit _ (LitInt b i)) = i

sortCon :: [(ExpI,Fun Int)] -> [(Int,[([Pos], Fun Int)])]
sortCon funs =
  (stableSort 
  . map ( \ (pat,Fun pats gdexps decls) ->
		 case getConArg pat of
		   (con,args) -> (con,(map getPos args,Fun (args++pats) gdexps decls)))
  ) funs
 where
  getConArg (ExpCon _ con) = (con,[])
  getConArg (PatAs _ _ p) = getConArg p
  getConArg (ExpApplication _ (ExpCon _ con:ps)) = (con,ps)

stableSort :: [(Int, b)] -> [(Int, [b])]
stableSort xs = -- I hope !!
 let add (c,f) t = addAT t (++) c [f]
 in listAT (foldr add initAT xs)


needLet (PatternVar  patfuns) = any (not . null . getTrans . fst) patfuns
needLet (PatternCon  patfuns) = any (not . null . getTrans . fst) patfuns
needLet (PatternInt  patfuns) = any (not . null . getTrans . fst) patfuns
needLet (PatternNK   patfuns) = any (not . null . getTrans . fst) patfuns
needLet (PatternIf   patfuns) = any (not . null . getTrans . fst) patfuns
needLet (PatternIrr (pat,fun)) = (not . null . getTrans) pat
