{- ---------------------------------------------------------------------------
Small tweaks based on type information.
optimisation: evaluation of `fromInteger' where possible
Also removes data constructors defined by newtype.
-}
module FixSyntax(fixSyntax) where

import List(intersperse)
import Extra(Pos(..),noPos,strPos,pair,dropJust,strace)
import Syntax
import IdKind(IdKind(..))
import State
import IntState(IntState,lookupIS,tidIS,strIS)
import TokenId
import DbgId
import Info(Info,isData,isMethod)
import FSLib
import Ratio
import Machine
import Id(Id)


litFloatInteger :: a {-boxed-} -> Integer -> Lit a

litFloatInteger b v =
  if floatIsDouble
  then LitDouble b (fromInteger v)
  else LitFloat b (fromInteger v)


litFloatRational :: a {-boxed-} -> Ratio Integer -> Lit a

litFloatRational b v =
  if floatIsDouble
  then LitDouble b (fromRational v)
  else LitFloat b (fromRational v)


{- main function of this pass -}
fixSyntax :: Decls Id 
          -> IntState 
          -> ((TokenId,IdKind) -> Id) 
          -> ([Decl Id]  -- modified declarations
             ,IntState   -- modified internal state
             ,Tree (TokenId,Id))

fixSyntax topdecls state tidFun =
  startfs fsTopDecls topdecls state tidFun


fsTopDecls :: Decls Id -> FSMonad [Decl Id]

fsTopDecls (DeclsScc depends) = 
  unitS (concat :: ([[Decl Int]] -> [Decl Int])) =>>> 
		-- concat must be typed for hbc ?
  mapS fsTopDepend depends


fsTopDepend :: DeclsDepend Id -> FSMonad [Decl Id]

fsTopDepend (DeclsNoRec d) = fsDecl d >>>= \ d -> unitS [d]
fsTopDepend (DeclsRec  ds) = mapS fsDecl ds


fsDecls :: Decls Id -> FSMonad (Decls Id)

fsDecls (DeclsScc depends) = unitS DeclsScc =>>> mapS fsDepend depends


fsDepend :: DeclsDepend Id -> FSMonad (DeclsDepend Id)

fsDepend (DeclsNoRec d) = unitS DeclsNoRec =>>> fsDecl d
fsDepend (DeclsRec  ds) = unitS DeclsRec   =>>> mapS fsDecl ds


fsDecl :: Decl Id -> FSMonad (Decl Id)

fsDecl d@(DeclPrimitive pos fun arity t) =
  unitS d
fsDecl d@(DeclForeignImp pos _ fun arity cast t _) =
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


fsFun :: Fun Id -> FSMonad (Fun Id) 

fsFun  (Fun pats gdexps decls) =
  mapS fsExp pats >>>= \ pats ->
  mapS fsGdExp gdexps >>>= \ gdexps ->
  fsDecls decls >>>= \ decls ->
  unitS (Fun pats gdexps decls)


fsGdExp :: (Exp Id,Exp Id) -> FSMonad (Exp Id,Exp Id)

fsGdExp (g,e) =
  fsExp g >>>= \ g ->
  fsExp e >>>= \ e ->
  unitS (g,e)


fsExp :: Exp Id -> FSMonad (Exp Id)

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
fsExp exp@(ExpApplication p [ExpVar _ fci, ExpDict dict@(Exp2 _ qNum qType),
                             sr, t, l@(ExpLit p2 (LitInteger b i))]) =
    fsState >>>= \state ->
    if tidIS state fci == t_fromConInteger
    && tidIS state qNum == tNum then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tInt then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInt, Var)), sr, t
	                            ,ExpLit p2 (LitInt b (fromInteger i))])
        else if tidIS state qType == tInteger then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInteger, Var))
                                    ,sr, t, l])
        else if tidIS state qType == tFloat then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conFloat, Var)), sr, t
	                            ,ExpLit p2 (litFloatInteger b i)])
        else if tidIS state qType == tDouble then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conDouble, Var)), sr, t
	                            ,ExpLit p2 (LitDouble b (fromInteger i))])
        else if tidIS state qType == tRational then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conRational, Var))
                                    ,sr, t
	                            ,ExpLit p2 (LitRational b (fromInteger i))])
  --    else
  --	    unitS (ExpApplication p [ExpVar p (tidFun (tfromInteger, Var))
  --                                ,dict, sr, t, l])
  	else error ("fsExp: strange expr(1) at "++ strPos p ++
                    "\n  ctx is Num, literal on rhs is not Int or Integer")
    else if tidIS state fci == t_patFromConInteger
         && tidIS state qNum == tNum then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tInt then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitInt b (fromInteger i))
                                    ,PatWildcard p])
        else if tidIS state qType == tInteger then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,l, PatWildcard p])
        else if tidIS state qType == tFloat then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitFloat b (fromInteger i))
                                    ,PatWildcard p])
        else if tidIS state qType == tDouble then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitDouble b (fromInteger i))
                                    ,PatWildcard p])
        else if tidIS state qType == tRational then
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitRational b (fromInteger i))
                                    ,PatWildcard p])
	else error ("fsExp: strange expr(5) at "++ strPos p ++
                    "\n  definite ctx, pat on lhs is not Int or Integer")
    else error ("fsExp: strange expr(2) at " ++ strPos p ++
                "\n  ctx not Num?  neither a lhs pat nor a rhs literal?" ++
                "\n  fci=" ++ show t_fromConInteger ++
                "\n  pfci=" ++ show t_patFromConInteger ++
                "\n  ?=" ++ show(tidIS state fci))

fsExp exp@(ExpApplication p [ExpVar _ fci, ExpDict dict{-@(ExpVar _ _)-},
                             sr, t, l@(ExpLit p2 (LitInteger b i))]) =
  fsState >>>= \state ->
    if tidIS state fci == t_fromConInteger
    || tidIS state fci == t_patFromConInteger then
        fsTidFun >>>= \tidFun -> 
        --strace ("fixSyntax: fromInteger expression or pattern with dictionary ("
        --      ++showExp state dict++")") $
	unitS (ExpApplication p
                 [ExpVar p (tidFun (t_ap 1, Var)), sr, t
                 ,ExpApplication p
                    [ExpVar p (tidFun (tfromInteger, Var))
                    ,dict, sr, t]
                 ,ExpApplication p
                    [ExpVar p (tidFun (t_conInteger, Var))
                    ,sr, t, l]])
    else error ("fsExp: strange expr(3) at "++ strPos p ++
                "\n  variable ctx, neither a lhs pat nor a rhs literal integer")
  where
    showExp state (Exp2 _ a b) =
        "Exp2 "++ show (tidIS state a)++" "++show (tidIS state b)
    showExp state (ExpApplication _ es) =
        "ExpAp ["++ concat (intersperse "," (map (showExp state) es)) ++ "]"
    showExp state (ExpVar _ a) =
        "ExpVar "++ show (strIS state a)
    showExp state e =
        "_"

fsExp exp@(ExpApplication p [ExpVar _ fcr
                            ,ExpDict dict@(Exp2 _ qFractional qType)
                            ,sr, t, l@(ExpLit p2 (LitRational b i))]) =
    fsState >>>= \state ->
    if tidIS state fcr == t_fromConRational
    && tidIS state qFractional == tFractional then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tFloat then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conFloat, Var)), sr, t
	                            ,ExpLit p2 (LitFloat b (fromRational i))])
        else if tidIS state qType == tDouble then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conDouble, Var)), sr, t
	                            ,ExpLit p2 (LitDouble b (fromRational i))])
        else if tidIS state qType == tRational then
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conRational, Var))
                                    ,sr, t, l])
  	else error ("fsExp: strange expr(10) at "++ strPos p ++
                    "\n  ctx is Fractional, literal on rhs is not float/double")
    else if tidIS state fcr == t_patFromConRational
         && tidIS state qFractional == tFractional then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tFloat then
            --strace ("fixSyntax: Float pattern in Fractional context") $
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitFloat b (fromRational i))
                                    ,PatWildcard p])
        else if tidIS state qType == tDouble then
            --strace ("fixSyntax: Double pattern in Fractional context") $
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,ExpLit p2 (LitDouble b (fromRational i))
                                    ,PatWildcard p])
        else if tidIS state qType == tRational then
            --strace ("fixSyntax: Rational pattern in Fractional context") $
	    unitS (ExpApplication p [ExpCon p (tidFun (tR, Con))
	                            ,l ,PatWildcard p])
	else error ("fsExp: strange expr(15) at "++ strPos p ++
                    "\n  definite ctx, pat on lhs is not Float or Double")
    else error ("fsExp: strange expr(12) at " ++ strPos p ++
                "\n  ctx not Fractional?  neither a lhs pat nor a rhs literal?" ++
                "\n  ?=" ++ show(tidIS state fcr))

fsExp exp@(ExpApplication p [ExpVar _ fcr, ExpDict dict{-@(ExpVar _ _)-},
                             sr, t, l@(ExpLit p2 (LitRational b i))]) =
  fsState >>>= \state ->
    if tidIS state fcr == t_fromConRational
    || tidIS state fcr == t_patFromConRational then
        fsTidFun >>>= \tidFun -> 
        --strace ("fixSyntax: fromRational expression or pattern with dictionary ("
        --      ++showExp state dict++")") $
	unitS (ExpApplication p
                 [ExpVar p (tidFun (t_ap 1, Var)), sr, t
                 ,ExpApplication p
                    [ExpVar p (tidFun (tfromRational, Var))
                    ,dict, sr, t]
                 ,ExpApplication p
                    [ExpVar p (tidFun (t_conRational, Var))
                    ,sr, t, l]])
    else error ("fsExp: strange expr(13) at "++ strPos p ++
                "\n  variable ctx, neither a lhs pat nor a rhs literal rational")
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

{-
Check if data constructor is from newtype definition.
If it is, then remove it or replace it by the identity function.
-} 
fsExp (ExpApplication pos (econ@(ExpCon cpos con):xs)) =
  fsRealData con >>>= \ realdata ->
  if realdata then
    mapS fsExp xs >>>= \ xs ->
    fsExpAppl pos (econ:xs)
  else
    if length xs < 1 then
      fsId -- because argument not available, have to replace by identity
    else
      mapS fsExp xs >>>= \ xs ->
      fsExpAppl pos xs	
      -- ^ Can be an application if newtype is isomorphic to a function type
      -- ^ No! \[x] -> unitS x should do, but that doesn't matter.

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


fsAlt :: Alt Id -> FSMonad (Alt Id)

fsAlt (Alt pat gdexps decls)  =
  fsExp pat >>>= \ pat ->
  fsDecls decls >>>= \ decls ->
  mapS fsGdExp gdexps >>>= \ gdexps ->
  unitS (Alt pat gdexps decls)

{- End FixSyntax ------------------------------------------------------------}
