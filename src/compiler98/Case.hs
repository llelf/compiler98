{-
Translates all patterns into case expressions
-}

module Case(caseTopLevel) where

import Ratio
import Extra
import Syntax
import PackedString(PackedString,packString,unpackPS)
import SyntaxPos
import PosCode
import State
import IntState
import Tree234
import AssocTree
import IdKind
import TokenId
import NT
import Bind(identPat)
import Info
import CaseLib
import CaseHelp
import CaseOpt
import FSLib
import MergeSort(mergeSort)
import SyntaxUtil
import Foreign(ImpExp(..))

caseTopLevel modstr t2i code topdecls state tidFun =
    let
	down =
	 	(\dict ->					-- expEqualNumEq
		 ExpApplication noPos
			[ExpVar noPos (tidFun (t_equalequal,Var))
			,ExpApplication noPos [Exp2 noPos (tidFun (tNum,TClass)) (tidFun (tEq,TClass))
					      ,dict]
		        ]
                ,ExpVar noPos (tidFun (t_eqInteger,Var))	-- expEqInteger
                ,ExpVar noPos (tidFun (t_eqFloat,Var))		-- expEqFloat
                ,ExpVar noPos (tidFun (t_eqDouble,Var))		-- expEqDouble
		,ExpCon noPos (tidFun (tTrue,Con))		-- expTrue
		,(ExpCon noPos (tidFun (t_List,Con))		-- expList  expNil
		 ,ExpCon noPos (tidFun (t_Colon,Con)))		--          expCons
		,ExpVar noPos (tidFun (t_error,Var))		-- expError
		,tidFun 					-- used by stgRatioCon
		,PosVar noPos (tidFun (t_undef,Var))		-- stgUndef 
		,modstr						-- strModid
		,initAT						-- translate
		)

	up =	(state
		,t2i						-- TokenId -> Int   generated (and some tuples) to unique
		)
    in
     case caseTopDecls topdecls down up of 
      (bs,up) ->
       case mapS caseCode code down up of
        (bs',(state,t2i)) -> (bs ++ concat bs',state)


caseNoMatch :: String -> Pos -> CaseFun (CaseFun PosExp)
caseNoMatch x pos  down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
  (caseExp $
    ExpApplication pos 
       [expError
       ,ExpLit pos (LitString Boxed (strModid ++ ": " ++ x ++ "."))]
  ,up)

caseTopDecls :: [Decl Int] -> CaseFun [(Int, PosLambda)]
caseTopDecls depends = unitS (concat :: ( [[a]]->[a])) =>>> mapS caseDecl depends

caseDecls :: (Decls Int) -> CaseFun PosExp -> CaseFun PosExp
caseDecls (DeclsScc depends) exp = caseDepends depends exp

caseDepends :: [DeclsDepend Int] -> CaseFun PosExp -> CaseFun PosExp
caseDepends []               e = e
caseDepends (DeclsNoRec d:r) e = unitS (PosExpLet noPos) =>>> caseDecl d =>>> caseDepends r e
caseDepends (DeclsRec  ds:r) e = unitS (PosExpLet noPos . concat) =>>> mapS caseDecl ds =>>> caseDepends r e

caseDecl :: Decl Int -> CaseFun [(Int,PosLambda)]
caseDecl d@(DeclPrimitive pos fun arity t) =
  unitS [(fun, PosPrimitive pos fun)]
caseDecl d@(DeclForeignImp pos str fun arity cast t) =
  unitS [(fun, PosForeign pos fun str cast Imported)]
caseDecl d@(DeclForeignExp pos str fun t) =
  unitS [(fun, PosForeign pos fun str False Exported)]
caseDecl (DeclFun pos fun funs) =
  unitS ((:[]) . pair fun) =>>> matchFun pos fun funs
caseDecl (DeclPat (Alt pat gdexps decls)) =
  caseDeclPatAs pat gdexps decls

caseDeclPatAs (PatAs p v pat) gdexps decls = caseDeclPatFix (p,v) pat gdexps decls
caseDeclPatAs pat gdexps decls =
   caseUnique >>>= \ tupleId ->
   caseState >>>= \ state ->
   caseAdd (InfoName tupleId ((forceM (mrpsIS state) . visible . strPos) (getPos pat)) 0  tunknown) >>>
   caseDeclPatFix (getPos pat,tupleId) pat gdexps decls

caseDeclPatFix (pos,tupleId) pat gdexps decls =
  singleVars pat >>>= \ easy ->
  case easy of
    Just pis ->
       caseTuple (length pis) >>>= \ tupleCon ->
       caseDecls decls (onePat pos pat Nothing gdexps) >>>= \ exp ->
       mapS (oneSel (PosVar pos tupleId) tupleCon pis) (map ( \ (Just p,i) -> (p,i) ) (filter (isJust.fst) (zip pis [1..]))) >>>= \ sels ->
       unitS ((tupleId,PosLambda pos [] [] exp) : sels)

    Nothing ->
      let pis :: [(Pos,Int)]
          pis = identPat pat
      in caseTuple (length pis) >>>= \ tupleCon ->
         caseDecls decls (onePat pos pat (Just (ExpApplication pos (ExpCon pos tupleCon: map (uncurry ExpVar) pis)))  gdexps) >>>= \ exp ->
         mapS (oneSel (PosVar pos tupleId) tupleCon pis) (zip pis [1..]) >>>= \ sels ->
         unitS ((tupleId,PosLambda pos [] [] exp) : sels)


oneSel :: PosExp -> Int -> [a] -> ((Pos,Int),Int) -> CaseFun (Int,PosLambda)
oneSel t tuplecon pis ((pos,ident),index) =
  caseUniques pis >>>= \ gunki ->   -- Only want the unique integers
  unitS (ident
	,PosLambda pos [] [] (PosExpCase pos t [PosAltCon pos tuplecon (map (pair pos . snd) gunki) (PosVar pos (map snd gunki !! (index-1::Int)))]))

oneId :: PosExp -> (Pos,Int) -> CaseFun (Int,PosLambda)
oneId t (pos,ident) =
  unitS (ident
	,PosLambda pos [] [] t)

onePat :: Pos -> ExpI -> Maybe ExpI -> [(ExpI,ExpI)] -> CaseFun PosExp
onePat pos pat tuple gdexps = 
  caseTrue >>>= \ true@(ExpCon _ itrue) ->
  caseNoMatch ("All guards false for pattern at " ++ strPos pos) pos >>>= \ noguard ->
  fixGdExp itrue gdexps noguard >>>= \ exp ->
  case tuple of  -- No tuple means that exp already has the correct format
    Nothing -> unitS exp
    Just tuple ->
      caseNoMatch ("No match in pattern expression at " ++ strPos pos) pos >>>= \ nomatch ->
      match [exp] [Fun [pat] [(true,tuple)] (DeclsScc [])] nomatch


{-
onePat pos pat tuple gdexps = 
  caseTrue >>>= \ true@(ExpCon _ itrue) ->
  caseNoMatch ("All guards false for pattern at " ++ strPos pos) pos >>>= \ noguard ->
  fixGdExp itrue gdexps noguard >>>= \ exp ->
  caseNoMatch ("No match in pattern expression at " ++ strPos pos) pos >>>= \ nomatch ->
  match [exp] [Fun [pat] [(true,tuple)] (DeclsScc [])] nomatch
-}



caseExp :: ExpI -> CaseFun PosExp
caseExp (ExpCase pos exp alts) = caseExp exp >>>= \ exp ->  matchCase pos exp alts
caseExp (ExpIf pos c e1 e2) = unitS (PosExpIf pos) =>>> caseExp c =>>> caseExp e1 =>>> caseExp e2
caseExp (ExpLambda pos pats exp) =  matchLambda pos pats exp

-- Nothing to do
caseExp (ExpApplication pos es)   =
  mapS caseExp es >>>= \ es ->
  unitS (PosExpApp pos es)

caseExp (ExpDict exp)    =  caseExp exp >>>= \ exp -> unitS (PosExpDict exp)
caseExp (ExpLet pos decls exp)    =  caseDecls decls (caseExp exp)
caseExp (ExpVar pos ident) = caseIdent pos ident
caseExp (ExpCon pos ident) = unitS (PosCon pos ident)
caseExp (ExpLit pos (LitInt b v)) = unitS (PosInt pos v)
caseExp (ExpLit pos (LitChar b v)) =  unitS (PosChar pos (fromEnum v))
caseExp (ExpLit pos (LitInteger b v)) = unitS (PosInteger pos v)
caseExp (ExpLit pos (LitRational b v)) =
	caseRatioCon >>>= \ ratioCon ->  unitS (PosExpApp pos [ratioCon, PosInteger pos (numerator v), PosInteger pos (denominator v)])
caseExp (ExpLit pos (LitString b v)) = unitS (PosString pos v)
caseExp (ExpLit pos (LitDouble b v)) = unitS (PosDouble pos v)
caseExp (ExpLit pos (LitFloat b v)) = unitS (PosFloat pos v)
caseExp  ExpFail = unitS PosExpFail
caseExp (Exp2 pos   i1 i2) =   unitS (PosVar pos) =>>> fsExp2i pos i1 i2  -- re-introduced by expEqualNumEq
caseExp (PatWildcard pos) =  caseUndef
caseExp e  =  error ("caseExp " ++ strPos (getPos e))

-------- Interface to match

matchFun pos fun funs@(Fun args _ _:_) =
  caseUniques args >>>= \ iargs ->
  caseNoMatch ("Pattern match failure in function at " ++ strPos pos) pos >>>= \ nomatch ->
  let vars = map ( \ (a,i) -> (getPos a,i)) iargs
  in match (map (uncurry PosVar) vars) funs nomatch >>>=  \ exp ->
     unitS (PosLambda pos [] vars exp)

matchLambda pos pats exp = 
 if all isExpVar pats
 then
  unitS (PosExpLambda pos [] (map ( \ (ExpVar p a) -> (p,a) ) pats)) =>>> caseExp exp
 else
  caseUniques pats >>>= \ ipats ->
  caseNoMatch ("Pattern match failure in lambda at " ++ strPos pos) pos >>>= \ nomatch ->
  caseTrue >>>= \ expTrue ->
  let vars = map ( \ (p,i) -> (getPos p,i) ) ipats
  in match (map (uncurry PosVar) vars) [Fun pats [(expTrue,exp)] (DeclsScc [])] nomatch >>>= \ exp ->
     unitS (PosExpLambda pos [] vars exp)

matchCase pos cexp alts = 
 caseNoMatch ("No matching alternative in case expression at " ++ strPos pos) pos >>>= \ nomatch ->
 match [cexp] (map alt2fun alts) nomatch

--------------------  Help functions


fixFuns :: Int -> [Fun Int] -> CaseFun PosExp -> CaseFun PosExp
fixFuns true [Fun _ gdexps decls] def =
  caseDecls decls (fixGdExp true gdexps def)
fixFuns true (Fun _ gdexps decls:rest) def =
  caseDecls decls (fixGdExp true gdexps (unitS PosExpFail)) >>>= \ e1 ->
  fixFuns true rest def >>>= \ e2 ->
  optFatBar e1 e2

fixGdExp :: Int -> [(ExpI,ExpI)] -> CaseFun PosExp -> CaseFun PosExp
fixGdExp true [] def   = def
fixGdExp true ((ExpApplication pos [x],e):r) def = 
  fixGdExp true ((x,e):r) def
fixGdExp true ((ExpCon pos c,e):r) def | c == true =
  (if null r then
    unitS0
   else
    caseError ("Alternative at " ++ strPos (getPos r) ++ " is hidden by alternative at " ++ strPos pos)) >>>
  caseExp e
fixGdExp true ((g,e):r) def = unitS (PosExpIf noPos) =>>> caseExp g =>>> caseExp e =>>> fixGdExp true r def


--------  The core 

match ::  [PosExp] -> [Fun Int] -> CaseFun PosExp -> CaseFun PosExp
match []   funs def = caseTrue >>>= \ (ExpCon _ true) -> fixFuns true funs def  -- All patterns matched
match vars funs def =
  caseState >>>= \ state ->
  caseList >>>= \ list ->
  case splitPattern list state funs of
    []  -> def                  -- No more patterns to match
    [x] -> matchOne vars x def
    xs  ->
      case vars of
        (PosVar _ _:_) -> matchMany vars xs def
        (ve:ves) -> if any needLet xs then
                       caseUnique >>>= \ v ->
                       let pos = getPos ve
                       in matchMany (PosVar pos v:ves) xs def >>>= \ exp ->
                          unitS (PosExpLet pos [(v,PosLambda pos [] [] ve)] exp)
                    else
                       matchMany vars xs def

matchMany vars [] def = def
matchMany vars (x:xs) def = matchOne vars x (matchMany vars xs def) 

matchOne (ce:ces) (PatternVar x) def =
  case unzip x of
    (pats,funs) ->
      varExpT (concatMap getTrans pats) ce >>>= \ (trans,v,f,ce) ->
      caseTranslate v trans >=>
      match ces funs def >>>= \ exp ->
      unitS (f exp)

matchOne (ce:ces) (PatternCon x) def =
  varExpT (concatMap (getTrans.fst) x) ce >>>= \ (trans,v,f,ce) ->
  caseTranslate v trans >=>
  mapS (matchAltCon ces) (sortCon x) >>>= \ alts ->
  def >>>= \ e2 ->
  optFatBar (f (PosExpCase (getPos ce) ce alts)) e2
matchOne (ce:ces) (PatternInt x) def =
  varExpT (concatMap (getTrans.fst) x) ce >>>= \ (trans,v,f,ce) ->
  caseTranslate v trans >=>
  mapS (matchAltInt ces) (sortInt x) >>>= \ alts ->
  def >>>= \ e2 ->
  optFatBar (f (PosExpCase (getPos ce) ce alts)) e2

matchOne (ce:ces) (PatternNK x) def =
  varExp ce >>>= \ (v,f,ce) ->
  caseTranslate v (concatMap (getTrans.fst) x) >=>
  mapS (matchNK v ces) x >>>= \ nks ->
  def >>>= \ e2 ->
  optFatBar (f (foldr ($) PosExpFail nks)) e2

matchOne (ce:ces) (PatternIf x) def =
  varExp ce >>>= \ (v,f,ce) ->
  caseTranslate v (concatMap (getTrans.fst) x) >=>
  mapS (matchAltIf v ces) x >>>= \ ifs ->  -- varExp always returns a valid v!
  def >>>= \ e2 ->
  optFatBar (f (foldr1 (PosExpFatBar True) ifs)) e2
--  optFatBar (f (foldr ($) PosExpFail ifs)) e2
matchOne (ce:ces) (PatternIrr (pat,fun)) def =
  varExp ce >>>= \ (v,f,ce) ->
  caseTranslate v (getTrans pat) >=>
  case dropPatAs pat of
    (PatIrrefutable pos pat) ->
      let pis = identPat pat
	  pos = getPos pat
      in caseTuple (length pis) >>>= \ tupleCon ->
	 caseUnique >>>= \ tupleId ->
	 caseState >>>= \ state ->
	 caseAdd (InfoName tupleId ((forceM (mrpsIS state) . visible . strPos . getPos) pat) 0 tunknown) >>>
	 mapS (oneSel (PosVar pos tupleId) tupleCon pis) (zip pis [1..]) >>>= \ sels ->
	 caseTrue >>>= \ true ->
	 matchCase pos ce
	    [Alt pat [(true,ExpApplication noPos (ExpCon noPos tupleCon: map (uncurry ExpVar) pis))] (DeclsScc [])] >>>= \ exp ->
         unitS (PosExpLet pos ((tupleId,PosLambda pos [] [] (f exp)) :sels)) =>>> match ces [fun] def

-- Ugly hack ought to at least try to group same constant if it is safe
-- (which it is for Integer,Double and Float, but not for unknown types)

matchAltIf :: Int -> [PosExp] -> (ExpI,Fun Int) -> CaseFun PosExp
matchAltIf v ces (PatAs _ _ pat,fun) = matchAltIf v ces (pat,fun)
matchAltIf v ces (pat@(ExpApplication pos [fromInteger,dict,lit]),fun) =
  caseEqualNumEq >>>= \ equalNumEq ->
  unitS (PosExpIf pos) =>>>
	caseExp (ExpApplication pos [equalNumEq dict,ExpVar pos v,pat]) =>>>
	match ces [fun] (unitS PosExpFail) =>>>
          (unitS PosExpFail)
matchAltIf v ces (pat@(ExpLit pos (LitInteger _ a)),fun) =
  caseEqInteger >>>= \ equal ->
  mkIfLit v ces pos pat fun equal
matchAltIf v ces (pat@(ExpLit pos (LitFloat _ a)),fun) =
  caseEqFloat >>>= \ equal ->
  mkIfLit v ces pos pat fun equal
matchAltIf v ces (pat@(ExpLit pos (LitDouble _ a)),fun) =
  caseEqDouble >>>= \ equal ->
  mkIfLit v ces pos pat fun equal
matchAltIf v ces pat = error ("What? matchAltIf at " ++ strPos (getPos pat) ++ "\n")

mkIfLit :: Int -> [PosExp] -> Pos -> ExpI -> Fun Int -> ExpI -> CaseFun PosExp
mkIfLit v ces pos pat fun equal =  
 unitS (PosExpIf pos) =>>>
       caseExp (ExpApplication pos [equal,ExpVar pos v,pat]) =>>>
       match ces [fun] (unitS PosExpFail) =>>>
       (unitS PosExpFail)


matchAltCon :: [PosExp] -> (Int,[([Pos],Fun Int)]) -> CaseFun PosAlt
matchAltCon ces (con,(poss_funs)) = 
  caseUniques (fst (head poss_funs)) >>>= \ vs ->
  let ces' = map (uncurry PosVar) vs
  in match (ces' ++ ces) (map snd poss_funs) (unitS PosExpFail) >>>= \ exp ->
     unitS (PosAltCon noPos con vs exp)

matchAltInt ces (i,funs) = 
  match ces funs (unitS PosExpFail) >>>= \ exp ->
  unitS (PosAltInt noPos i  exp)

matchNK :: Int -> [PosExp] -> (ExpI,Fun Int) -> CaseFun (PosExp->PosExp)
matchNK v ces (PatNplusK pos n n' k kle ksub, fun) =
  caseTrue >>>= \ true ->
  match ces [fun] (unitS PosExpFail) >>>= \ exp ->
  caseDecl (DeclFun pos n' [Fun [] [(true, ExpVar pos v)] (DeclsScc [])]) >>>= \ local ->
  caseExp kle >>>= \ cond ->
  caseDecl (DeclFun pos n [Fun [] [(true, ksub)] (DeclsScc [])]) >>>= \ binding ->
  unitS (\f-> PosExpLet pos local (PosExpIf pos cond (PosExpLet pos binding exp) f))

------------------

caseCode (CodeClass pos cls) =
  caseState >>>= \ state ->
  let clsInfo =  dropJust (lookupIS state cls)
  in mapS (fsExp2i pos cls) (superclassesI clsInfo) >>>= \ sc ->
     caseUniques (sc ++ forceOrder state (map fst (methodsI clsInfo))) >>>= \ msi ->
     caseTuple (length msi) >>>= \ tupleCon ->
     mapS (select tupleCon (map snd msi)) msi
 where
  select tupleCon tvars (c,i) =
    caseUnique >>>= \ v ->
      unitS (c,PosLambda pos [] [(pos,v)] (PosExpCase noPos (PosVar noPos v) [PosAltCon noPos tupleCon (map (pair noPos) tvars) (PosVar noPos i)]))
 
caseCode d@(CodeInstance pos cls typ args exps methods) =
  caseState >>>= \ state ->
  fsExp2i pos cls typ >>>= \ u ->
  caseTuple (length exps + length methods) >>>= \ tupleCon ->
  let eargs = map (ExpVar pos) args 
  in caseExp (ExpApplication noPos (ExpCon noPos tupleCon:exps
                                  ++ map (ExpApplication noPos . (:eargs) . ExpVar noPos)
                                         (forceOrder state methods))) >>>= \ tuple ->
     unitS [(u,PosLambda pos [] (map (pair pos) args) tuple)]


forceOrder state is =
  map snd (mergeSort (map ( \ i -> (tidIS state i,i)) is))


----------------------

