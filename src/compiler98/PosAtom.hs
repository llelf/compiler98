module PosAtom(posAtom) where

import IntState
import PosCode -- hiding (isPosAtom)
import State
import TokenId(tidPos,tunknown)
import Extra(dropJust,unionSet,removeSet,pair)
import TokenId(visible)
import SyntaxPos

-- isPosAtom x = True -- A (not so) small lie

data AtomDown =
  AtomDown
    TokenId 		        -- current function
    [Int]			-- current env

data AtomState =
  AtomState
    IntState
    PosCode


posAtom :: IntState -> [(Int,PosLambda)] -> ([(Int,PosLambda)],IntState)

posAtom state code = 
  case (mapS atomTopBinding code) (AtomDown tunknown []) (AtomState state []) of
    (code,AtomState state _) -> (concat code,state)

atomTopBinding (fun,PosLambda pos [] args exp) =
  atomTid fun >=>
  atomEnv (map snd args) >=>
  atomExp exp >>>= \ exp ->
  atomTop >>>= \ bs ->
  unitS ((fun,PosLambda pos [] args exp):bs)
atomTopBinding (fun,PosPrimitive pos fn) =
  unitS [(fun,PosPrimitive pos fn)]
atomTopBinding (fun,PosForeign pos fn t c ie) =
  unitS [(fun,PosForeign pos fn t c ie)]


atomBinding (fun,PosLambda pos envs [] exp) =  -- no functions left
  atomExp exp >>>= \ exp -> 
  flattenExp fun pos envs exp
atomBinding (fun,PosPrimitive pos fn) =
  unitS [(fun,PosPrimitive pos fn)]
atomBinding (fun,PosForeign pos fn t c ie) =
  unitS [(fun,PosForeign pos fn t c ie)]

-- Forgot why I do this! The isPosAtom used was forced to be true due to local definition?
flattenExp fun pos envs exp =
  unitS [(fun,PosLambda pos envs [] exp)] -- !!! envs might be too large !!!

{-
--flattenExp fun pos envs (PosExpLet _ bindings' exp) =
--  flattenExp fun pos envs exp >>>= \ bindings ->
--  unitS (bindings' ++ bindings)
flattenExp fun pos envs exp@(PosExpThunk _ (f:es)) | isntPrim f =
  unitS [(fun,PosLambda pos envs [] exp)] -- !!! envs might be too large !!!
flattenExp fun pos envs exp =
 if isPosAtom exp then
  unitS [(fun,PosLambda pos envs [] exp)] -- !!! envs might be too large !!!
 else
  atomAddG pos exp >>>= \ exp ->
  unitS [(fun,exp)]
-}

atomExp (PosExpLet pos bindings exp)    =
  atomEnv (map fst bindings) >=>
  unitS (PosExpLet pos . concat) =>>> mapS atomBinding bindings =>>> atomExp exp
atomExp (PosExpCase pos exp alts) =
  unitS (PosExpCase pos) =>>> atomExp exp =>>> mapS atomAlt alts
atomExp (PosExpFatBar b e1 e2) =
  unitS (PosExpFatBar b) =>>> atomExp e1 =>>> atomExp e2
atomExp (PosExpFail) =
  unitS PosExpFail
atomExp (PosExpIf pos c e1 e2)       =
  unitS (PosExpIf pos) =>>> atomExp c =>>> atomExp e1 =>>> atomExp e2
atomExp (PosExpApp pos (f:es)) = -- First doesn't need to be an atom in byte-code back-end
  atomExp f >>>=  \ f ->
  mapS ensureAtom es >>>= \ a_b ->
  case unzip a_b of
    (as,bs) ->
      mapS atomBinding (concat bs) >>>= \ bs ->
      unitS (posExpLet pos (concat bs) (posExpApp pos (f:as)))
atomExp (PosExpThunk pos (f@(PosCon _ con):es)) =
  mapS atomExp es >>>= \ es ->
  atomStrict con >>>= \ s ->
  if and s then
    mapS ensureAtom es >>>= \ a_b ->
    case unzip a_b of
      (as,bs) ->
        mapS atomBinding (concat bs) >>>= \ bs ->
        unitS (posExpLet pos (concat bs) (foldr posStrict (PosExpThunk pos (f:as)) (zip s as) ))
  else
    unitS (PosExpThunk pos (f:es))

atomExp (PosExpThunk pos (f@(PosPrim _ _):es)) =
  mapS atomExp es >>>= \ es ->           -- Byte code back-end can handle arbitrary arguments to byte code instructions
  unitS (PosExpThunk pos (f:es))
atomExp (PosExpThunk pos (f:es)) = -- First is a function
  mapS atomExp es >>>= \ es ->           -- Byte code back-end can handle arbitrary arguments in thunks
  unitS (PosExpThunk pos (f:es))
{-
  mapS ensureAtom es >>>= \ a_b ->
  case unzip a_b of
    (as,bs) ->
      mapS atomBinding (concat bs) >>>= \ bs ->
      unitS (posExpLet pos (concat bs) (PosExpThunk pos (f:as)))
-}
atomExp e = unitS e

atomAlt (PosAltCon pos con args exp) =
  unitS (PosAltCon pos con args) =>>>  (atomEnv (map snd args) >=> atomExp exp)
atomAlt (PosAltInt pos int      exp) =
  unitS (PosAltInt pos int) =>>>   atomExp exp

posStrict (True,PosExpThunk  _ [atom]) e = posStrict (True,atom) e
posStrict (True,a@(PosVar pos _)) e = PosExpThunk pos [PosPrim pos SEQ, a, e]
posStrict a  e = e

ensureAtom exp =
  if isPosAtom exp then
    unitS (exp,[])
  else
    atomAdd (getPos exp) exp 

atomTop down up@(AtomState state bs) = (bs,AtomState state [])

atomAdd pos exp down@(AtomDown tid env) up@(AtomState state bs) =
  case uniqueIS state of
    (u,state) ->
      ((PosVar pos u,[(u,PosLambda pos (map (pair pos) (filter (`elem` env) (freePosExp exp))) [] exp)])
      ,AtomState (addIS u (InfoName u (visible (reverse ("ATOM" ++ show u))) 0 (tidPos tid pos) True) state) bs --PHtprof
      )

{-
atomAddG pos exp down@(AtomDown tid env) up@(AtomState state bs) =
  case uniqueIS state of
    (u,state) ->
      let newenv = filter (`elem` env) (freePosExp exp) 
          pnewenv = map (pair pos) newenv
      in (PosLambda pos pnewenv [] (PosExpThunk pos (PosVar pos u: map (uncurry PosVar) pnewenv))
         ,AtomState (addIS u (InfoName u (visible (reverse ("ATOM" ++ show u))) (length pnewenv) (tidPos tid pos) True) state) --PHtprof
		 ((u,PosLambda pos [] pnewenv exp): bs)
         )
-}

atomStrict con  down@(AtomDown tid env) up@(AtomState state bs) =
  ((strictI . dropJust . lookupIS state) con, up)

atomTid fun down@(AtomDown tid env) up@(AtomState state bs) =
  let tid = (profI . dropJust . lookupIS state) fun
  in seq tid (AtomDown tid env,up)

atomEnv newenv down@(AtomDown tid env) up@(AtomState state bs) =
  (AtomDown tid (newenv ++ env),up)



----------------------------------

freePosLambda (PosLambda pos env arg exp) = map snd env -- Don't need to go deeper
freePosLambda (PosPrimitive pos p) = []
freePosLambda (PosForeign pos p t c ie) = []

freePosExp (PosExpLet  pos bindings exp) = foldr unionSet (freePosExp exp) (map (freePosLambda . snd) bindings) `removeSet` map fst bindings
freePosExp (PosExpCase pos cExp alts) = foldr unionSet (freePosExp cExp)  (map freePosAlt alts)
freePosExp (PosExpApp  pos es) = foldr unionSet [] (map freePosExp es)
freePosExp (PosExpThunk pos es) = foldr unionSet [] (map freePosExp es)
freePosExp (PosExpFatBar b exp1 exp2) = freePosExp exp1 `unionSet` freePosExp exp2
freePosExp (PosExpFail) = []
freePosExp (PosExpIf   pos cExp tExp eExp) = (freePosExp cExp `unionSet` freePosExp tExp `unionSet` freePosExp eExp)
freePosExp (PosVar pos v) = [v]
freePosExp (PosCon pos c) = []
freePosExp (PosInt pos v) = []
freePosExp (PosChar pos v) = []
freePosExp (PosFloat   pos v) = [] 
freePosExp (PosDouble  pos v) = [] 
freePosExp (PosInteger pos v) = []
freePosExp (PosString  pos v) = []
freePosExp (PosPrim    pos prim) = []

freePosAlt (PosAltCon pos con args exp) = freePosExp exp `removeSet` (map snd args)
freePosAlt (PosAltInt pos int      exp) = freePosExp exp

