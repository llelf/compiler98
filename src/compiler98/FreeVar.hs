module FreeVar(freeVar) where

import State
import PosCode
import SyntaxPos
import Extra(singletonSet,emptySet,unionSet,removeSet,strPos,noPos,Pos(..),pair,dropJust)
import IntState
import Kind
--------- ===========

data FreeDown =
  FreeDown
    Bool			-- Strict
    Bool			-- keep Case

data FreeThread =
  FreeThread
    [[Int]]			-- envs
    IntState

freeVar keepcase code state = 
  case (mapS freeBindingTop code) (FreeDown True keepcase) (FreeThread [] state) of
    (f_code,FreeThread envs state) -> (map snd f_code,state)

-------

freeString pos string =
  unitS (emptySet,PosExpLambda pos [] [] (PosExpThunk pos [PosPrim pos STRING,PosString pos string]))

freeLambda pos _ args exp =
  freeStrict True $
     freePushEnv (map snd args) >>>
     freeExp exp >>>= \ (expF,exp) ->
     freePopEnv >>>
     let envs = removeSet expF (map snd args)
     in unitS (envs,PosExpLambda pos (map (pair pos) envs) args exp)

freeBindingTop (fun,body) =
  freeBindingLow True fun body

freeBinding (fun,body@(PosLambda pos envs [] exp)) =
  freeBindingLow False fun body
freeBinding (fun,body@(PosLambda pos envs (_:_) exp)) =
  freeBindingLow True fun body
freeBinding (fun,body@(PosPrimitive pos fn)) =
  freeBindingLow True fun body
freeBinding (fun,body@(PosForeign pos fn t c ie)) =
  freeBindingLow True fun body

freeBindingLow strict fun (PosLambda pos envs args exp) =
  freeStrict strict $
  freePushEnv (map snd args) >>>
  freeExp exp >>>= \ (expF,exp) ->
  freePopEnv >>>
  let envs  = removeSet expF (map snd args)
      envsL = removeSet envs (singletonSet fun)
  in unitS (envsL,(fun,PosLambda pos (map (pair pos) envs) args exp))
freeBindingLow strict fun (PosPrimitive pos fn) =
  unitS (emptySet,(fun,PosPrimitive pos fn))
freeBindingLow strict fun (PosForeign pos fn t c ie) =
  unitS (emptySet,(fun,PosForeign pos fn t c ie))

freeExp (PosExpLambda pos envs args exp)  = freeLambda pos envs args exp
freeExp (PosExpDict exp)    = freeExp exp -- not needed any more
freeExp (PosExpLet pos bindings exp)    =
  freePushEnv (map fst bindings) >>>
  mapS freeBinding bindings >>>= \ f_bindings ->
  case unzip f_bindings of 
    (bindingFs,bindings) ->
      freeExp exp >>>= \ (expF,exp) ->
        freePopEnv >>>
        let free = removeSet (foldr unionSet expF bindingFs) (map fst bindings)
	in unitS (free,PosExpLet pos bindings exp)
freeExp e@(PosExpCase pos exp alts) =
  freeQStrict >>>= \ strict ->
  if strict
  then
    mapS freeAlt alts >>>= \ f_alts ->
    case unzip f_alts of
      (altFs,alts) ->
	 freeExp exp >>>= \ (expF,exp) ->
         let free = foldr unionSet expF altFs
         in unitS (free,PosExpCase pos exp alts)
  else freeLambda pos [] [] e
freeExp e@(PosExpFatBar b e1 e2) =
  freeQStrict >>>= \ strict ->
  if strict
  then
    freeExp e1 >>>= \ (e1F,e1) ->
    freeExp e2 >>>= \ (e2F,e2) ->
    unitS (unionSet e1F e2F, PosExpFatBar b e1 e2)
  else freeLambda noPos [] [] e
freeExp (PosExpFail) = unitS (emptySet,PosExpFail)
freeExp e@(PosExpIf pos c e1 e2)       =
  freeQStrict >>>= \ strict ->
  if strict
  then
    freeExp c >>>= \ (cF,c) ->
    freeExp e1 >>>= \ (e1F,e1) ->
    freeExp e2 >>>= \ (e2F,e2) ->
    unitS (unionSet cF (unionSet e1F e2F), PosExpIf pos c e1 e2)
  else freeLambda pos [] [] e
freeExp (PosExpApp pos (econ@(PosCon cpos con):args)) =
  freeArity con >>>= \ arity ->
  let available = length args
  in if available < arity
     then freeNewArgs (arity-available) >>>= \ nargs ->
	  freeLambda pos [] (map (pair pos) nargs) (PosExpThunk pos (econ:args ++ map (PosVar pos) nargs))
     else if available == arity -- Yes I'm paranoid !!!
	  then freeExp (PosExpThunk pos (econ:args))
	  else error ("Too many arguments to constructor " ++ " wants " ++ show arity
			 ++ " but got " ++ show available)
freeExp (PosExpApp pos (f:es)) =
  freeExp f >>>= \ (fF,f) ->
  freeStrict False (mapS freeExp es) >>>= \ f_es ->
  case unzip f_es of
    (eFs,es) ->
      unitS (foldr unionSet fF eFs, posExpApp pos (f:es))

freeExp e@(PosExpThunk pos (c@(PosCon _ con):es)) = -- A con with correct number of arguments
  freeQStrict >>>= \ strict ->
  freeConStrict con >>>= \ conStrict ->
  if strict || not (or conStrict)
  then
    freeStrict False (mapS freeExp es) >>>= \ f_es ->
    case unzip f_es of
      (eFs,es) ->
        unitS (foldr unionSet emptySet eFs, PosExpThunk pos (c:es))
  else freeLambda pos [] [] e
freeExp (PosExpThunk pos (f:es)) = -- A primitive/con with correct number of arguments
  freeStrict False (mapS freeExp es) >>>= \ f_es ->
  case unzip f_es of
    (eFs,es) ->
      unitS (foldr unionSet emptySet eFs, PosExpThunk pos (f:es))
freeExp con@(PosCon pos _) = freeExp (PosExpApp pos [con])
freeExp exp@(PosVar pos i) = freeIdent exp i
freeExp (PosString pos string) = freeString pos string
freeExp exp = unitS (emptySet,exp)

freeAlt (PosAltCon pos con args exp) =
  freePushEnv (map snd args) >>>
  freeExp exp >>>= \ (expF,exp) ->
  freePopEnv >>>
  unitS (removeSet expF (map snd args),PosAltCon pos con args exp)
freeAlt (PosAltInt pos int      exp) =
  freeExp exp >>>= \ (expF,exp) ->
  unitS (expF,PosAltInt pos int  exp)

freeIdent exp ident down up@(FreeThread envs state) =
  if any (ident `elem`) envs 
  then ((singletonSet ident,exp),up)
  else ((emptySet,exp),up)

-------------

freePushEnv envs' down up@(FreeThread envs state) =
  FreeThread (envs':envs) state

freePopEnv down up@(FreeThread (_:envs) state) =
  FreeThread envs state

freeArity con down up@(FreeThread envs state) =
  case  lookupIS state con of
    Just conInfo -> (arityVI conInfo,up)

freeQStrict down@(FreeDown strict keepcase) up =
  (strict || keepcase,up)

freeStrict strict free down@(FreeDown _ keepcase) up =
  free (FreeDown strict keepcase) up

freeNewArgs n down  up@(FreeThread envs state) =
 case mkArgs [] state n of
   (args,state) -> (args,(FreeThread envs state))
 where
  mkArgs a state 0 = (a,state)
  mkArgs a state n = case uniqueIS state of (u,state) -> mkArgs (u:a) state (n-1)

freeConStrict con  down up@(FreeThread envs state) =
  ((strictI . dropJust . lookupIS state) con, up)
