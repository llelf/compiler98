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
import Info(Info,isData,isMethod)
import FSLib(FSMonad,Tree,startfs,fsState,fsExpAppl,fsClsTypSel,fsExp2,fsId
            ,fsRealData,fsList,fsTidFun,fsTracing)
import Ratio
import Machine
import DbgId(t_R,t_ap,t_conInt,t_conInteger,t_conFloat,t_conDouble
            ,t_fromConInteger,t_fromConRational
            ,t_patFromConInteger,t_patFromConRational
            ,t_mkNTId,t_mkNTConstr,t_mkSR,t_mkNTId',t_mkNTConstr',t_mkSR')
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
fixSyntax :: Bool	-- are we dealing with trace-transformed code?
          -> Decls Id 
          -> IntState 
          -> ((TokenId,IdKind) -> Id) 
          -> ([Decl Id]  -- modified declarations
             ,IntState   -- modified internal state
             ,Tree (TokenId,Id))

fixSyntax trace topdecls state tidFun =
  startfs trace fsTopDecls topdecls state tidFun


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
  fsTracing >>>= \trace ->
  fsExp' trace exp

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
-- This is a separate function because we want a cheap runtime test
-- for trace-transformed code.  (i.e. the boolean argument)
fsExp' t (ExpApplication pos (ExpApplication _ xs:ys)) =
  fsExp' t (ExpApplication pos (xs++ys))

fsExp' True (ExpApplication p xs@[ExpVar pi i, arg@(ExpLit _ _)]) =
  fsState >>>= \state ->
  fsTidFun >>>= \tidFun ->
  let token = tidIS state i in
  if token == t_mkNTId' then 
    fsExp' True (ExpApplication p [ExpVar pi (tidFun (t_mkNTId,Var)),arg])
  else if token == t_mkNTConstr' then 
    fsExp' True (ExpApplication p [ExpVar pi (tidFun (t_mkNTConstr,Var)),arg])
  else if token == t_mkSR' then 
    fsExp' True (ExpApplication p [ExpVar pi (tidFun (t_mkSR,Var)),arg])
  else {- nothing to do -}
    mapS fsExp xs >>>= \ xs -> fsExpAppl p xs

fsExp' True exp@(ExpApplication p [ExpVar _ fci
				  , ExpDict dict@(Exp2 _ qNum qType)
				  , sr, t, l@(ExpLit p2 (LitInteger b i))]) =
    fsState >>>= \state ->
    fsTidFun >>>= \tidFun -> 
    if tidIS state fci == t_fromConInteger
    && tidIS state qNum == tNum then
        fsExp sr >>>= \sr ->
        fsExp t >>>= \t -> 
        if tidIS state qType == tInt then
         -- strace (strPos p++": literal Num expression of type Int\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInt, Var)), sr, t
	                            ,ExpLit p2 (LitInt b (fromInteger i))])
        else if tidIS state qType == tInteger then
         -- strace (strPos p++": literal Num expression of type Integer\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conInteger, Var))
                                    ,sr, t, l])
        else if tidIS state qType == tFloat then
         -- strace (strPos p++": literal Num expression of type Float\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conFloat, Var)),sr,t
	                            ,ExpLit p2 (litFloatInteger b i)])
        else if tidIS state qType == tDouble then
         -- strace (strPos p++": literal Num expression of type Double\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conDouble, Var))
                                    ,sr,t
	                            ,ExpLit p2 (LitDouble b (fromInteger i))])
  --    else if tidIS state qType == tRational then
  --     -- strace (strPos p++": literal Num expression of type Rational\n") $
  --	    unitS (ExpApplication p2
  --                 [ExpVar p2 (tidFun (t_ap 2, Var)), sr, t
  --                 ,ExpApplication p2
  --                    [ExpVar p2 (tidFun (tRatioCon, Var))
  --                    ,ExpDict (Exp2 p2 (tidFun (tIntegral,TClass))
  --                                      (tidFun (tInteger,TCon)))
  --                    ,sr, t]
  --                 ,ExpApplication p2
  --                    [ExpVar p2 (tidFun (t_conInteger, Var))
  --                    ,sr, t, ExpLit p2 (LitInteger b i)]
  --                 ,ExpApplication p2
  --                    [ExpVar p2 (tidFun (t_conInteger, Var))
  --                    ,sr, t, ExpLit p2 (LitInteger b 1)]
  --                 ])
  --Let a Rational just fall through to become (fromInteger i):
        else unitS (ExpApplication p
                      [ExpVar p2 (tidFun (t_ap 1, Var)), sr, t
                      ,ExpApplication p2
                         [ExpVar p2 (tidFun (tfromInteger, Var))
                         ,dict, sr, t]
                      ,ExpApplication p2
                         [ExpVar p2 (tidFun (t_conInteger, Var))
                         ,sr, t, l]])
  --    else error ("fsExp: strange expr(1) at "++ strPos p ++
  --                "\n  ctx is Num, rhs literal not Int/Integer/Float/Double")
    else if tidIS state fci == t_patFromConInteger
         && tidIS state qNum == tNum then
        if tidIS state qType == tInt then
         -- strace (strPos p++": literal Num pattern of type Int\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (LitInt b (fromInteger i))
                                    ,PatWildcard p])
        else if tidIS state qType == tInteger then
         -- strace (strPos p++": literal Num pattern of type Integer\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,l, PatWildcard p])
        else if tidIS state qType == tFloat then
         -- strace (strPos p++": literal Num pattern of type Float\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (litFloatInteger b i)
                                    ,PatWildcard p])
        else if tidIS state qType == tDouble then
         -- strace (strPos p++": literal Num pattern of type Double\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (LitDouble b (fromInteger i))
                                    ,PatWildcard p])
        else if tidIS state qType == tRational then
         -- strace (strPos p++": literal Num pattern of type Rational\n") $
         -- known to be wrong!
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (LitRational b (fromInteger i))
                                    ,PatWildcard p])
	else error ("fsExp: strange expr(5) at "++ strPos p ++
                    "\n  Num ctx, pattern not Int/Integer/Float/Double?")
    else error ("fsExp: strange expr(2) at " ++ strPos p ++
                "\n  ctx not Num?  neither a lhs pat nor a rhs literal?" ++
                "\n  fci=" ++ show t_fromConInteger ++
                "\n  pfci=" ++ show t_patFromConInteger ++
                "\n  ?=" ++ show(tidIS state fci))

fsExp' True exp@(ExpApplication p [ExpVar _ fci, ExpDict dict{-@(ExpVar _ _)-}
                                  , sr, t, l@(ExpLit p2 (LitInteger b i))]) =
  fsState >>>= \state ->
    if tidIS state fci == t_fromConInteger
    || tidIS state fci == t_patFromConInteger then
        fsTidFun >>>= \tidFun -> 
        fsExp sr >>>= \sr ->
        fsExp t >>>= \t -> 
        --strace ("fixSyntax: fromInteger expr/pat with dictionary ("
        --      ++showExp state dict++")") $
        --strace (strPos p++": literal Num expr/pat of unknown type\n") $
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
--where
--  showExp state (Exp2 _ a b) =
--      "Exp2 "++ show (tidIS state a)++" "++show (tidIS state b)
--  showExp state (ExpApplication _ es) =
--      "ExpAp ["++ concat (intersperse "," (map (showExp state) es)) ++ "]"
--  showExp state (ExpVar _ a) =
--      "ExpVar "++ show (strIS state a)
--  showExp state e =
--      "_"

fsExp' True exp@(ExpApplication p [ExpVar _ fcr
                                  ,ExpDict dict@(Exp2 _ qFractional qType)
                                  ,sr, t, l@(ExpLit p2 (LitRational b i))]) =
    fsState >>>= \state ->
    if tidIS state fcr == t_fromConRational
    && tidIS state qFractional == tFractional then
        fsTidFun >>>= \tidFun -> 
        fsExp sr >>>= \sr ->
        fsExp t >>>= \t -> 
        if tidIS state qType == tFloat then
         -- strace (strPos p++": literal Fract expression of type Float\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conFloat, Var)),sr,t
	                            ,ExpLit p2 (litFloatRational b i)])
        else if tidIS state qType == tDouble then
         -- strace (strPos p++": literal Fract expression of type Double\n") $
	    unitS (ExpApplication p [ExpVar p (tidFun (t_conDouble, Var)),sr,t
	                            ,ExpLit p2 (LitDouble b (fromRational i))])
        else if tidIS state qType == tRational then
         -- strace (strPos p++": literal Fract expression of type Rational\n")$
	    unitS (ExpApplication p
                     [ExpVar p (tidFun (t_ap 2, Var)), sr, t
                     ,ExpApplication p
                        [ExpVar p (tidFun (tRatioCon, Var))
                        ,ExpDict (Exp2 p (tidFun (tIntegral,TClass))
                                         (tidFun (tInteger,TCon)))
                        ,sr, t]
                     ,ExpApplication p
                        [ExpVar p (tidFun (t_conInteger, Var))
                        ,sr, t, ExpLit p2 (LitInteger b (numerator i))]
                     ,ExpApplication p
                        [ExpVar p (tidFun (t_conInteger, Var))
                        ,sr, t, ExpLit p2 (LitInteger b (denominator i))]
                     ])
	else unitS (ExpApplication p
                      [ExpVar p (tidFun (t_ap 1, Var)), sr, t
                      ,ExpApplication p
                         [ExpVar p (tidFun (tfromRational, Var))
                         ,dict, sr, t]
                      ,ExpApplication p
                         [ExpVar p (tidFun (t_ap 2, Var)), sr, t
                         ,ExpApplication p
                            [ExpVar p (tidFun (tRatioCon, Var))
                            ,ExpDict (Exp2 p (tidFun (tIntegral,TClass))
                                             (tidFun (tInteger,TCon)))
                            ,sr, t]
                         ,ExpApplication p
                            [ExpVar p (tidFun (t_conInteger, Var))
                            ,sr, t, ExpLit p2 (LitInteger b (numerator i))]
                         ,ExpApplication p
                            [ExpVar p (tidFun (t_conInteger, Var))
                            ,sr, t, ExpLit p2 (LitInteger b (denominator i))]
                         ]
                      ])
 --     else
 --       error ("fsExp: strange expr(10) at "++ strPos p ++
 --              "\n  ctx is Fractional, literal on rhs is not float/double")
    else if tidIS state fcr == t_patFromConRational
         && tidIS state qFractional == tFractional then
        fsTidFun >>>= \tidFun -> 
        if tidIS state qType == tFloat then
         -- strace (strPos p++": literal Fract pattern of type Float\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (litFloatRational b i)
                                    ,PatWildcard p])
        else if tidIS state qType == tDouble then
         -- strace (strPos p++": literal Fract pattern of type Double\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,ExpLit p2 (LitDouble b (fromRational i))
                                    ,PatWildcard p])
        else if tidIS state qType == tRational then
         -- strace (strPos p++": literal Fract pattern of type Rational\n") $
	    unitS (ExpApplication p [ExpCon p (tidFun (t_R, Con))
	                            ,l ,PatWildcard p])
	else error ("fsExp: strange expr(15) at "++ strPos p ++
                    "\n  definite ctx, pat on lhs is not Float or Double")
    else error ("fsExp: strange expr(12) at " ++ strPos p ++
                "\n  ctx not Fractional?  neither a lhs pat nor a rhs literal?"
                ++"\n  ?=" ++ show(tidIS state fcr))

fsExp' True exp@(ExpApplication p [ExpVar _ fcr, ExpDict dict{-@(ExpVar _ _)-}
                                  ,sr, t, l@(ExpLit p2 (LitRational b i))]) =
  fsState >>>= \state ->
    if tidIS state fcr == t_fromConRational
    || tidIS state fcr == t_patFromConRational then
        fsTidFun >>>= \tidFun -> 
        fsExp sr >>>= \sr ->
        fsExp t >>>= \t -> 
     -- strace (strPos p++": literal Fractional expr/pat of unknown type\n") $
	unitS (ExpApplication p
                 [ExpVar p (tidFun (t_ap 1, Var)), sr, t
                 ,ExpApplication p
                    [ExpVar p (tidFun (tfromRational, Var))
                    ,dict, sr, t]
                 ,ExpApplication p
                    [ExpVar p (tidFun (t_ap 2, Var)), sr, t
                    ,ExpApplication p
                       [ExpVar p (tidFun (tRatioCon, Var))
                       ,ExpDict (Exp2 p (tidFun (tIntegral,TClass))
                                        (tidFun (tInteger,TCon)))
                       ,sr, t]
                    ,ExpApplication p
                       [ExpVar p (tidFun (t_conInteger, Var))
                       ,sr, t, ExpLit p2 (LitInteger b (numerator i))]
                    ,ExpApplication p
                       [ExpVar p (tidFun (t_conInteger, Var))
                       ,sr, t, ExpLit p2 (LitInteger b (denominator i))]
                    ]
                 ])
    else error ("fsExp: strange expr(13) at "++ strPos p ++
                "\n  variable ctx, neither a lhs pat nor a rhs literal rational")

--- fromInteger {Int Integer Float Double} constant
fsExp' False exp@(ExpApplication pos [v@(ExpVar _ qfromInteger)
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
  	else fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- fromRational {Float Double Rational} constant
fsExp' False (ExpApplication pos [v@(ExpVar _ qfromRational) 
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
  	else fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])  -- Match (sel (class.type dicts) args)
    else fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos [v2]),l])

--- negate {Int Integer Float Double Rational} constant

fsExp' False (ExpApplication pos [v@(ExpVar pos3 qnegate) 
                                 ,(ExpDict v2@(Exp2 _ qNum qType)) 
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
      _ -> fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])  -- Will do p once more :-(
   else
     fsExp' False (ExpApplication pos [v,ExpDict (ExpApplication pos3 [v2]),p])

--
-- Transforms (sel class.type args) into (sel (class.type) args)
--
fsExp' t (ExpApplication pos (v@(ExpVar _ _):ExpDict v2@(Exp2 _ _ _):es)) =
  fsExp' t (ExpApplication pos (v:ExpDict (ExpApplication pos [v2]):es)) 
  -- Match (sel (class.type dicts) args)

--
-- Transforms (sel (class.type dicts) args) into ((class.type.sel dicts) args)
--
fsExp' t (ExpApplication pos (ExpVar sp sel 
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
fsExp' t (ExpApplication pos (econ@(ExpCon cpos con):xs)) =
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
fsExp' t (ExpApplication pos xs) =
  mapS fsExp xs >>>= \ xs ->
  fsExpAppl pos xs



fsAlt :: Alt Id -> FSMonad (Alt Id)

fsAlt (Alt pat rhs decls)  =
  fsExp pat >>>= \ pat ->
  fsDecls decls >>>= \ decls ->
  fsRhs rhs >>>= \ rhs ->
  unitS (Alt pat rhs decls)

{- End FixSyntax ------------------------------------------------------------}
