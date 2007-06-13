{-
Performs lambda-lifting of the program
-}

module Lift (liftCode) where

import List
import State
import PosCode
import SyntaxPos
import Extra (emptySet,unionSet,removeSet,noPos,dropJust,strace,pair)
import IntState
import TokenId
import IdKind
--------- ===========

data LiftDown =
  LiftDown
    Bool			-- strict
    ((TokenId,IdKind)->Int)	-- tidFun
    TokenId		        -- current function

data LiftThread =
  LiftThread
    [(Int,[Int])]  -- translation from lifted identifier to new free variables
    [PosBinding]   -- new top-level definitions
    IntState


liftCode code state tidFun = 
  case (mapS liftTopBinding code)
             (LiftDown True tidFun tunknown)
             (LiftThread [] [] state) of
    (code,LiftThread _ _ state) -> (concat code,state)

liftTopBinding d =
  liftSetTid (fst d) >=>
  liftBinding d >>>= \ d ->
  liftTop    >>>= \ sc ->
  unitS (d:sc)

liftScc pos bindingsIn down@(LiftDown strict tidFun ptid)
                       up@(LiftThread transIn scIn stateIn) =
  let
      (declsInLift,declsInStay) = partition liftIt bindingsIn

      definedLift = map fst declsInLift

      envLift = foldr unionSet emptySet 
		(map (expandEnv transIn)
                     (removeSet (foldr (unionSet . map snd . getEnvs)
                                       emptySet
                                       declsInLift)
                                definedLift))

      transNew = map (`pair` envLift) definedLift ++ transIn

      (declsOutLift,LiftThread _ scInLift state1) =
		 mapS liftBinding declsInLift (LiftDown True tidFun ptid)
                                              (LiftThread transNew [] stateIn)

      (declsOutStay,LiftThread _ scInStay state2) =
		 mapS liftBinding declsInStay (LiftDown False tidFun ptid)
                                              (LiftThread transNew [] state1)

      scHere = map (addArgs envLift) declsOutLift

      newBindings = map (addEnvs transNew) declsOutStay 

      newSC = scHere ++ scInLift++scInStay++scIn
      newState = foldr (updateInfo ptid) state2 scHere
  in (newBindings, LiftThread transNew newSC newState)

updateInfo ptid (fun, PosLambda pos envs args exp) state =
 let arity = length args in
 updateIS state fun
          (\info-> let tid = tidI info in
                   (seq tid (InfoName fun tid arity (tidPos ptid pos) True)))
								 --PHtprof

addArgs newargs (fun, PosLambda pos envs args exp) =
  (fun, PosLambda pos [] (map (pair pos) newargs++args) exp) 

addEnvs trans (fun, PosLambda pos envs args exp) =
  (fun, PosLambda pos (map (pair pos)
                           (foldr unionSet
                                  emptySet
                                  (map (expandEnv trans . snd) envs)))
                      args exp)

liftIt (fun, PosLambda pos envs args exp) = not (null args)

getEnvs (fun, PosLambda pos envs args exp) = envs

expandEnv trans f =
  case lookup f trans of
    Nothing -> [f]
    Just set -> set


liftLambda pos envs args exp down@(LiftDown strict tidFun ptid)
                             up@(LiftThread transIn scIn stateIn) =
  let
      newEnvs = map (pair pos)
                    (foldr unionSet
                           emptySet
                           (map (expandEnv transIn . snd) envs))

      scHere =  (fun, PosLambda pos [] (newEnvs++args) exp)
      arity = length newEnvs + length args
      (fun,state2) = uniqueIS stateIn
      tid = (visible (reverse ("LAMBDA" ++ show fun))) -- Not exported
      newSC = scHere:scIn
      newState = seq tid $ addIS fun (InfoName fun tid arity
                                               (tidPos ptid pos) True) --PHtprof
                                 state2

  in (PosExpApp pos (PosVar pos fun:map (uncurry PosVar) newEnvs)
     ,LiftThread transIn newSC newState
     )

liftBinding (fun,PosLambda pos envs args exp) =
  liftExp exp >>>= \ exp ->
  unitS (fun,PosLambda pos envs args exp)
liftBinding (fun,PosPrimitive pos fn) =
  unitS (fun,PosPrimitive pos fn)
liftBinding (fun,PosForeign pos fn t c ie) =
  unitS (fun,PosForeign pos fn t c ie)

liftExp (PosExpLambda pos envs args exp) =
  liftStrict True (liftExp exp) >>>=
  liftLambda pos envs args
liftExp (PosExpLet pos bindings exp) =
  liftScc pos bindings >>>= \ bindings ->
  liftExp exp >>>= \ exp -> 
  unitS (PosExpLet pos bindings exp)
liftExp e@(PosExpCase pos exp alts) =
  liftGetStrict >>>= \ strict ->
  if strict
  then unitS (PosExpCase pos) =>>> liftExp exp =>>> mapS liftAlt alts
  else strace "liftExp PosExpCase lazy!" $
       liftExp (PosExpLambda pos [] [] e)
liftExp e@(PosExpFatBar b e1 e2) =
  liftGetStrict >>>= \ strict ->
  if strict
  then unitS (PosExpFatBar b) =>>> liftExp e1 =>>> liftExp e2
  else strace "liftExp PosExpFatBar lazy!" $
       liftExp (PosExpLambda noPos [] [] e)
liftExp (PosExpFail) = unitS PosExpFail
liftExp e@(PosExpIf pos c e1 e2)       =
  liftGetStrict >>>= \ strict ->
  if strict
  then unitS (PosExpIf pos) =>>> liftExp c =>>> liftExp e1 =>>> liftExp e2
  else strace "liftExp PosExpIf lazy!" $
       liftExp (PosExpLambda pos [] [] e)
liftExp (PosExpApp pos es) =  -- hd es is not always strict !!!
  liftGetStrict >>>= \ strict ->
  if strict
  then --OLD: unitS (posExpApp pos) =>>> liftStrict False (mapS liftExp es)
       liftExp (head es) >>>= \ head_es ->
       liftStrict False (mapS liftExp (tail es)) >>>= \ tail_es ->
       unitS (posExpApp pos (head_es:tail_es))
  else liftApply es >>>= liftExp
liftExp (PosExpThunk pos (e:es)) =
  -- A primitive/con/apply with correct number of arguments
  liftExp e >>>= \ e ->
  liftStrict False (mapS liftExp es) >>>= \ es ->
  unitS (PosExpThunk pos (e:es))
liftExp (PosVar pos i) = liftIdent pos i
liftExp a = unitS a

liftAlt (PosAltCon pos con args exp) =
  unitS (PosAltCon pos con args) =>>> liftExp exp
liftAlt (PosAltInt pos int      exp) =
  unitS (PosAltInt pos int)      =>>> liftExp exp

--------------------
liftApply (e1:[]) = unitS e1
liftApply es@(e1:e2:[]) =
  liftTidFun (t_apply1,Var) >>>= \ f ->
  unitS (PosExpThunk (getPos e1) (f:es))
liftApply es@(e1:e2:e3:[]) =
  liftTidFun (t_apply2,Var) >>>= \ f ->
  unitS (PosExpThunk (getPos e1) (f:es))
liftApply es@(e1:e2:e3:e4:[]) =
  liftTidFun (t_apply3,Var) >>>= \ f ->
  unitS (PosExpThunk (getPos e1) (f:es))
liftApply    (e1:e2:e3:e4:e5:es) =
  liftTidFun (t_apply4,Var) >>>= \ f ->
  liftApply (PosExpThunk (getPos e1) (f:e1:e2:e3:e4:e5:[]):es)


liftIdent pos ident down@(LiftDown strict tidFun ptid)
                    up@(LiftThread trans sc state) =
  case lookup ident trans of
    Nothing ->  (PosVar pos ident,up)
    Just env ->  (PosExpApp pos (PosVar pos ident:map (PosVar pos) env),up)

liftSetTid fun down@(LiftDown strict tidFun ptid)
               up@(LiftThread trans sc state) =  
  (LiftDown strict tidFun ((profI . dropJust . lookupIS state) fun),up)

liftTop  down up@(LiftThread trans sc state) =
  (sc,LiftThread [] [] state)

liftStrict strict lift down@(LiftDown _ tidFun ptid) up =
  lift (LiftDown strict tidFun ptid) up

liftGetStrict down@(LiftDown strict tidFun ptid) up =
  (strict,up)

liftTidFun tid down@(LiftDown strict tidFun ptid) up =
  (PosVar noPos (tidFun tid) ,up)
