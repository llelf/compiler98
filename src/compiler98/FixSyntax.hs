module FixSyntax(fixSyntax) where

import Extra(Pos(..),noPos,strPos,pair,dropJust)
import Syntax
import Kind(Kind(..))
import State
import IntState(IntState,lookupIS,tidIS,strIS)
import TokenId
import DbgId
import Info(Info,isData,isMethod)
import FSLib
import Ratio
import Machine

litFloatInteger b v =
  if floatIsDouble
  then LitDouble b (fromInteger v)
  else LitFloat b (fromInteger v)

litFloatRational b v =
  if floatIsDouble
  then LitDouble b (fromRational v)
  else LitFloat b (fromRational v)


fixSyntax topdecls state tidFun =
  startfs fsTopDecls topdecls state tidFun

		-- concat must be typed for hbc ?
fsTopDecls (DeclsScc depends) = unitS (concat :: ([[Decl Int]] -> [Decl Int])) =>>> mapS fsTopDepend depends

fsTopDepend (DeclsNoRec d) = fsDecl d >>>= \ d -> unitS [d]
fsTopDepend (DeclsRec  ds) = mapS fsDecl ds

fsDecls (DeclsScc depends) = unitS DeclsScc =>>> mapS fsDepend depends

fsDepend (DeclsNoRec d) = unitS DeclsNoRec =>>> fsDecl d
fsDepend (DeclsRec  ds) = unitS DeclsRec   =>>> mapS fsDecl ds

fsDecl d@(DeclPrimitive pos fun arity t) =
  unitS d
fsDecl d@(DeclForeignImp pos _ fun arity cast t) =
  unitS d
fsDecl d@(DeclForeignExp pos _ fun t) =
  unitS d
fsDecl (DeclFun pos fun funs) =
  unitS (DeclFun pos fun) =>>> mapS fsFun funs
fsDecl (DeclPat (Alt pat gdexps decls)) =
  fsExp pat >>>= \ pat ->
  mapS fsGdExp gdexps >>>= \ gdexps ->
  fsDecls decls >>>= \ decls ->
  unitS (DeclPat (Alt pat gdexps decls))  

fsFun  (Fun pats gdexps decls) =
  mapS fsExp pats >>>= \ pats ->
  mapS fsGdExp gdexps >>>= \ gdexps ->
  fsDecls decls >>>= \ decls ->
  unitS (Fun pats gdexps decls)

fsGdExp (g,e) =
  fsExp g >>>= \ g ->
  fsExp e >>>= \ e ->
  unitS (g,e)

fsExp (ExpLambda pos pats exp)  =
  mapS fsExp pats >>>= \ pats ->
  fsExp exp >>>= \ exp ->
  unitS (ExpLambda pos pats exp)

fsExp (ExpLet pos decls exp)    =
  fsDecls decls >>>= \ decls ->
  fsExp exp >>>= \ exp ->
  unitS (ExpLet pos decls exp)

fsExp (ExpDict exp)    =
  fsExp exp >>>= \ exp ->
  unitS (ExpDict exp)

fsExp (ExpCase pos exp alts) =
  unitS (ExpCase pos) =>>> fsExp exp =>>> mapS fsAlt alts

fsExp (ExpIf pos c e1 e2)       =
  unitS (ExpIf pos) =>>> fsExp c =>>> fsExp e1 =>>> fsExp e2

fsExp (ExpApplication pos (ExpApplication _ xs:ys)) =
  fsExp (ExpApplication pos (xs++ys))

#ifdef DBGTRANS
fsExp exp@(ExpApplication p [ExpVar _ fci, ExpDict dict@(Exp2 _ qNum qType), sr, t, l@(ExpLit p2 (LitInteger b i))]) =
    fsState >>>= \state ->
    if tidIS state fci == t_fromConInteger && tidIS state qNum == tNum then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tInt then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInt, Var)), sr, t,
	                             ExpLit p (LitInt b (fromInteger i))])
        else if tidIS state qType == tInteger then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInteger, Var)), sr, t, l])
	else error ("fsExp: strange expr(1) at "++ strPos p)
    else if tidIS state fci == t_patFromConInteger then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tInt then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con)), 
	                             ExpLit p (LitInt b (fromInteger i)), t])
        else if tidIS state qType == tInteger then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con)), l, t])
	else error ("fsExp: strange expr(5) at "++ strPos p)
    else error ("fsExp: strange expr(2) at " ++ strPos p ++
                "\nfci=" ++ show t_fromConInteger ++
                "\npfci=" ++ show t_patFromConInteger ++
                "\n?=" ++ show(tidIS state fci))
fsExp exp@(ExpApplication p [ExpVar _ fci, ExpDict dict@(ExpVar _ _), sr, t, l@(ExpLit p2 (LitInteger b i))]) =
    fsState >>>= \state ->
    if tidIS state fci == t_fromConInteger then
        fsTidFun >>>= \tidFun -> 
	unitS (ExpApplication p [ExpVar p (tidFun (t_ap 1, Var)),
	                         sr, t, ExpApplication p [ExpVar p (tidFun (tfromInteger, Var)),
				                          dict, sr, t],
				 ExpApplication p [ExpVar p (tidFun (t_conInteger, Var)), 
				                   sr, t, l]])
    else if tidIS state fci == t_patFromConInteger then error ("fsExp: strange expr(4) at "++strPos p)
    else error ("fsExp: strange expr(3) at "++ strPos p)
#endif
--- fromInteger {Int Integer Float Double} constant
fsExp exp@(ExpApplication pos [v@(ExpVar _ qfromInteger),(ExpDict v2@(Exp2 _ qNum qType)),l@(ExpLit pl (LitInteger b i))]) =
  fsState >>>= \ state ->
    if tidIS state qfromInteger == tfromInteger && tidIS state qNum == tNum 
    then     if tidIS state qType == tInt        then unitS (ExpLit pl (LitInt b (fromInteger i)))
  	else if tidIS state qType == tIntHash    then unitS (ExpLit pl (LitInt UnBoxed (fromInteger i)))
  	else if tidIS state qType == tInteger    then unitS l
  	else if tidIS state qType == tFloat      then unitS (ExpLit pl (litFloatInteger b i))
  	else if tidIS state qType == tFloatHash  then unitS (ExpLit pl (litFloatInteger UnBoxed i))
  	else if tidIS state qType == tDouble     then unitS (ExpLit pl (LitDouble b (fromInteger i)))
  	else if tidIS state qType == tDoubleHash then unitS (ExpLit pl (LitDouble UnBoxed (fromInteger i)))
  	else if tidIS state qType == tRational   then unitS (ExpLit pl (LitRational b (fromInteger i)))
  	else fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- fromRational {Float Double Rational} constant
fsExp (ExpApplication pos [v@(ExpVar _ qfromRational) , (ExpDict v2@(Exp2 _ qFractional qType)) , l@(ExpLit pl (LitRational b i))]) =
  fsState >>>= \ state ->
    if tidIS state qfromRational == tfromRational && tidIS state qFractional == tFractional
    then     if tidIS state qType == tFloat      then unitS (ExpLit pl (litFloatRational b i))
  	else if tidIS state qType == tFloatHash  then unitS (ExpLit pl (litFloatRational UnBoxed i))
  	else if tidIS state qType == tDouble     then unitS (ExpLit pl (LitDouble b (fromRational i)))
  	else if tidIS state qType == tDoubleHash then unitS (ExpLit pl (LitDouble UnBoxed (fromRational i)))
  	else if tidIS state qType == tRational   then unitS l
  	else fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else  fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- negate {Int Integer Float Double Rational} constant

fsExp (ExpApplication pos [v@(ExpVar pos3 qnegate) , (ExpDict v2@(Exp2 _ qNum qType)) , p]) =
  fsState >>>= \ state ->
  if tidIS state qnegate == tnegate && tidIS state qNum == tNum  then
    fsExp p >>>= \ p ->
    case p of
      ExpLit pos (LitInt b i)      -> unitS (ExpLit pos (LitInt b (-i)))
      ExpLit pos (LitInteger b i)  -> unitS (ExpLit pos (LitInteger b (-i)))
      ExpLit pos (LitFloat b i)    -> unitS (ExpLit pos (LitFloat b (-i)))
      ExpLit pos (LitDouble b i)   -> unitS (ExpLit pos (LitDouble b (-i)))
      ExpLit pos (LitRational b i) -> unitS (ExpLit pos (LitRational b (-i)))
      _ -> fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])  -- Will do p once more :-(
   else
     fsExp (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])

--
-- Transforms (sel class.type args) into (sel (class.type) args)
--
fsExp (ExpApplication pos (v@(ExpVar _ _):ExpDict v2@(Exp2 _ _ _):es)) =
  fsExp (ExpApplication pos (v:ExpDict (ExpApplication pos [v2]):es)) -- Match (sel (class.type dicts) args)

--
-- Transforms (sel (class.type dicts) args) into ((class.type.sel dicts) args)
--
fsExp (ExpApplication pos (ExpVar sp sel : ExpDict (ExpApplication ap (Exp2 _ cls qtyp:args)) : es)) =
  fsState >>>= \ state ->
  if (isMethod . dropJust . lookupIS state) sel && (isData . dropJust . lookupIS state) qtyp then
    fsClsTypSel sp cls qtyp sel >>>= \ fun ->
    mapS fsExp (args++es) >>>= \ args ->
    fsExpAppl pos (fun:args)
  else
    fsExp2 ap cls qtyp >>>= \ fun ->
    mapS fsExp args >>>= \ args ->
    fsExpAppl ap (fun:args) >>>= \ appl ->
    mapS fsExp es >>>= \ es ->
    fsExpAppl pos (ExpVar sp sel : ExpDict appl :es)

--
-- Check if constructor is newtype
-- 
fsExp (ExpApplication pos (econ@(ExpCon cpos con):xs)) =
  fsRealData con >>>= \ realdata ->
  if realdata then
    mapS fsExp xs >>>= \ xs ->
    fsExpAppl pos (econ:xs)
  else
    if length xs < 1 then
      fsId
    else
      mapS fsExp xs >>>= \ xs ->
      fsExpAppl pos xs	-- Can be an application if newtype is isomorphic with a function type

---
--- Nothing to do
---
fsExp (ExpApplication pos xs) =
  mapS fsExp xs >>>= \ xs ->
  fsExpAppl pos xs

---
--- No ExpList anymore
---
fsExp (ExpList  pos es)         = 
  mapS fsExp es >>>= \ es -> 
  fsList >>>= \ (nil,cons) ->
  unitS (foldr (\ h t -> ExpApplication pos [cons,h,t]) nil es)

--- Change con into (con)
fsExp e@(ExpCon pos ident) = fsExp (ExpApplication pos [e])

--- Change Char into Int
--fsExp (ExpLit pos (LitChar b i)) = unitS (ExpLit pos (LitInt b (fromEnum i)))
fsExp (Exp2   pos      i1 i2) =  fsExp2 pos i1 i2

fsExp (PatAs pos i pat)        =  unitS (PatAs pos i) =>>> fsExp pat
fsExp (PatIrrefutable pos pat) = unitS (PatIrrefutable pos) =>>> fsExp pat
fsExp e                 = unitS e

fsAlt (Alt pat gdexps decls)  =
  fsExp pat >>>= \ pat ->
  fsDecls decls >>>= \ decls ->
  mapS fsGdExp gdexps >>>= \ gdexps ->
  unitS (Alt pat gdexps decls)

--------------------------------------------------------------------------------------

