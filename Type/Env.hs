module Type.Env( envDecls, envPat, envPats, initEnv, lookupEnv, tvarsInEnv) where

import Syntax
import State
import NT
import Id(Id)
import Type.Data
import Type.Util
import IntState
import Bind(identPat)
import Util.Extra(mapSnd)

-- environment represented as a list of (Id,NT) pairs
initEnv = []
lookupEnv a b = lookup a b
tvarsInEnv env = concatMap (freeNT . snd) env


envDecls decls  _ (TypeState state phi ctxs ectxsi) =
  case mapS0 envDecl' decls () ([],[],state) of
    (env,ctxs',state) ->  (env,TypeState state phi (ctxs'++ctxs) ectxsi)
  

envDecl' (DeclPat (Alt pat gdexps decls)) = envPat' pat
envDecl' (DeclFun pos fun funs)           = addEnv' (pos,fun)
envDecl' _				  = unitS0


envPats pats _ (TypeState state phi ctxs ectxsi ) =
  case mapS0 envPat' pats () ([],[],state) of
    (env,ctxs',state) ->  (env,TypeState state phi (ctxs'++ctxs) ectxsi)

envPat pat _ (TypeState state phi ctxs ectxsi) =
  case envPat' pat () ([],[],state) of
    (env,ctxs',state) ->  (env,TypeState state phi (ctxs'++ctxs) ectxsi)

addEnv' (pos,ident) _ (env,ctxs,state) =
  case ntIS state ident of
    (NoType,state) ->
      case uniqueIS state of
	(unique,state) -> ((ident,NTany unique):env,ctxs,state)
    ((NewType free' exist' ctxs' [nt']),state) -> -- no constructors here!
      case uniqueISs state (map (mapSnd ( \ v -> if v `elem` exist' then mkNTexist v else mkNTvar v)) ctxs') of
	(ctxsi',state) ->
	  ((ident,nt'):env,map snd (cvi2typedict pos exist' ctxsi') ++ctxs,state)

envPat' pat = mapS0 addEnv' (identPat pat)

