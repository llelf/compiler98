{-
Convert STG-code into a string for readable output.
-}
module StrPos(strPCode,strPExp) where

import Extra(mixLine,mixSpace)
import PosCode

strPCode p code = mixLine (map (strPBinding p "") code)

strPLambda p o (PosLambda pos fvs bvs e) =
    '{' : mixSpace (map (p.snd) fvs) ++ "} \\ {" ++ mixSpace (map (p.snd) bvs) ++ "} ->\n" ++
    strPExp p (' ':o) e
strPLambda p o (PosPrimitive pos fun) =
    "primitive " ++ p fun ++ "\n"
strPLambda p o (PosForeign pos fun str c ie) =
    "foreign "++show ie++" "++show c++" \""++str++"\" " ++ p fun ++ "\n"

strPBinding p o (i,l) = o ++ p i ++ " = " ++ strPLambda p o l ++ "\n"

strPExp p o (PosExpDict e) = "{d}" ++ strPExp p o e
strPExp p o (PosExpLet pos bs e) = o ++ "let\n" ++ concatMap ((++"\n").strPBinding p (' ':o)) bs ++ strPExp p o e
strPExp p o (PosExpCase pos e args) = o ++ "case " ++ strPExp p "" e ++ " of\n" ++ mixLine (map  (strPAlt p (' ':o)) args)
strPExp p o (PosExpApp pos args) = o ++ "(" ++  mixSpace (map (strPExp p (' ':o)) args) ++ ")"
strPExp p o (PosExpThunk pos args) = o ++ "<" ++ mixSpace (map (strPExp p (' ':o)) args) ++ ">"
strPExp p o (PosExpFatBar b e1 e2) = o ++ "fatbar" ++ (if b then " that can fail\n" else "\n") 
				       ++ strPExp p (' ':o) e1 ++ "\n" ++ o ++ "--\n" ++ strPExp p (' ':o) e2
strPExp p o (PosExpFail) = o ++ "fail"
strPExp p o (PosExpIf pos e1 e2 e3) =  o ++ "if " ++ strPExp p (' ':o) e1 ++ o ++ "\n" ++ o ++ "then " ++ strPExp p (' ':o) e2++ o ++ "\n" ++ o ++ "else " ++ strPExp p (' ':o) e3
strPExp p o (PosExpLambda pos bes bvs e) = 
   o ++ "( {" ++ mixSpace (map (p.snd) bes) ++ "} \\ {" ++ mixSpace (map (p.snd) bvs) ++ "} ->\n" ++
  strPExp p (' ':o) e ++ ")"
strPExp p o (PosPrim pos prim) = o ++ strPrim prim
strPExp p o (PosVar  pos i) =  o ++ p i
strPExp p o (PosCon  pos c) =  o ++ p c ++ "{c}"
strPExp p o (PosInt  pos i) =  o ++ show i
strPExp p o (PosChar  pos i) =  o ++ "'" ++ [((toEnum i) :: Char)] ++ "'"
strPExp p o (PosFloat pos i) =  o ++ show i++"F"
strPExp p o (PosDouble pos i) =  o ++ show i
strPExp p o (PosString pos s) =  o ++ show s
strPExp p o (PosInteger pos i) =  o ++ show i++"L"

strPAlt p o (PosAltCon pos c args e) = o ++ p c ++ concatMap ((' ':).p.snd) args ++ " ->\n" ++ strPExp p (' ':o) e
strPAlt p o (PosAltInt pos i    e) = o ++ show i ++ " ->\n" ++ strPExp p (' ':o) e


