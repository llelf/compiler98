{- ---------------------------------------------------------------------------
Small tweaks based on type information.
optimisation: evaluation of `fromInteger' where possible
Also removes data constructors defined by newtype.
-}
module FixSyntax(fixSyntax) where

import AssocTree(AssocTree)
import List(intersperse)
import Extra(Pos(..),noPos,strPos,pair,dropJust,strace)
import Syntax
import IdKind(IdKind(..))
import State
import IntState(IntState,lookupIS,tidIS,strIS)
import TokenId
import Info(Info,isData,isMethod)
import FSLib(FSMonad,startfs,fsState,fsExpAppl,fsClsTypSel,fsExp2,fsId
            ,fsRealData,fsList,fsTidFun)
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
             ,AssocTree TokenId Id)

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
fsDecl d@(DeclForeignImp pos _ _ fun arity cast t _) =
  unitS d
fsDecl d@(DeclForeignExp pos _ _ fun t) =
  unitS d
fsDecl (DeclFun pos fun funs) =
  unitS (DeclFun pos fun) =>>> mapS fsFun funs
fsDecl (DeclPat (Alt pat rhs decls)) =
  fsExp pat >>>= \ pat ->
  fsRhs rhs >>>= \ rhs ->
  fsDecls decls >>>= \ decls ->
  unitS (DeclPat (Alt pat rhs decls))  


fsFun :: Fun Id -> FSMonad (Fun Id) 

fsFun  (Fun pats rhs decls) =
  mapS fsExp pats >>>= \ pats ->
  fsRhs rhs >>>= \ rhs ->
  fsDecls decls >>>= \ decls ->
  unitS (Fun pats rhs decls)


fsRhs :: Rhs Id -> FSMonad (Rhs Id)

fsRhs (Unguarded e) = fsExp e >>>= \e -> unitS (Unguarded e)
fsRhs (Guarded gdexps) = 
  mapS fsGdExp gdexps >>>= \gdexps -> unitS (Guarded gdexps)


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

fsExp exp@(ExpApplication _ _) =
  fsExp' exp

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


-- Auxiliary for fsExp guaranteed to get ExpApplications only.
fsExp' (ExpApplication pos (ExpApplication _ xs:ys)) =
  fsExp' (ExpApplication pos (xs++ys))

--- fromInteger {Int Integer Float Double} constant
fsExp' exp@(ExpApplication pos [v@(ExpVar _ qfromInteger)
                               ,(ExpDict v2@(Exp2 _ qNum qType))
                               ,l@(ExpLit pl (LitInteger b i))]) =
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
  	else fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- fromRational {Float Double Rational} constant
fsExp' (ExpApplication pos [v@(ExpVar _ qfromRational) 
                           ,(ExpDict v2@(Exp2 _ qFractional qType)) 
                           ,l@(ExpLit pl (LitRational b i))]) =
  fsState >>>= \ state ->
 -- strace (strPos pos++": normal literal Rational expr/pat\n") $
    if tidIS state qfromRational == tfromRational && tidIS state qFractional == tFractional
    then     if tidIS state qType == tFloat      then unitS (ExpLit pl (litFloatRational b i))
  	else if tidIS state qType == tFloatHash  then unitS (ExpLit pl (litFloatRational UnBoxed i))
  	else if tidIS state qType == tDouble     then unitS (ExpLit pl (LitDouble b (fromRational i)))
  	else if tidIS state qType == tDoubleHash then unitS (ExpLit pl (LitDouble UnBoxed (fromRational i)))
  	else if tidIS state qType == tRational   then unitS l
  	else fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- negate {Int Integer Float Double Rational} constant

fsExp' (ExpApplication pos [v@(ExpVar pos3 qnegate) 
                           ,d@(ExpDict v2@(Exp2 _ qNum qType)) 
                           ,p]) =
  fsState >>>= \ state ->
  if tidIS state qnegate == tnegate && tidIS state qNum == tNum  then
    fsExp p >>>= \ p ->
    case p of
      ExpLit pos (LitInt b i)      -> unitS (ExpLit pos (LitInt b (-i)))
      ExpLit pos (LitInteger b i)  -> unitS (ExpLit pos (LitInteger b (-i)))
      ExpLit pos (LitFloat b i)    -> unitS (ExpLit pos (LitFloat b (-i)))
      ExpLit pos (LitDouble b i)   -> unitS (ExpLit pos (LitDouble b (-i)))
      ExpLit pos (LitRational b i) -> unitS (ExpLit pos (LitRational b (-i)))
      -- negate (fromInteger v) in a pattern is a special case:
      -- If the fromInteger was not elided in the recursive call
      -- (e.g. instance Num UserType) then we need to keep the dictionary
      -- for later, when we lookup the (==) method to match the pattern.
      ExpApplication _ [ExpVar _ _,ExpLit _ _] ->
          unitS (ExpApplication pos [v,d,p])
      _ -> fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])  -- Will do p once more :-(
   else
     fsExp' (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])

--
-- Transforms (sel class.type args) into (sel (class.type) args)
--
fsExp' (ExpApplication pos (v@(ExpVar _ _):ExpDict v2@(Exp2 _ _ _):es)) =
  fsExp' (ExpApplication pos (v:ExpDict (ExpApplication pos [v2]):es)) 
  -- Match (sel (class.type dicts) args)

--
-- Transforms (sel (class.type dicts) args) into ((class.type.sel dicts) args)
--
fsExp' (ExpApplication pos (ExpVar sp sel 
                           :ExpDict (ExpApplication ap (Exp2 _ cls qtyp:args))
                           :es)) =
  fsState >>>= \ state ->
  if (isMethod . dropJust . lookupIS state) sel && 
     (isData . dropJust . lookupIS state) qtyp then
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
fsExp' (ExpApplication pos (econ@(ExpCon cpos con):xs)) =
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
fsExp' (ExpApplication pos xs) =
  mapS fsExp xs >>>= \ xs ->
  fsExpAppl pos xs



fsAlt :: Alt Id -> FSMonad (Alt Id)

fsAlt (Alt pat rhs decls)  =
  fsExp pat >>>= \ pat ->
  fsDecls decls >>>= \ decls ->
  fsRhs rhs >>>= \ rhs ->
  unitS (Alt pat rhs decls)

{- End FixSyntax ------------------------------------------------------------}
