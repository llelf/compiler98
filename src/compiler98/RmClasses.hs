module RmClasses(rmClasses) where

import Kind
import IntState
import Syntax
import Extra(dropJust)
import State
import Bind(identDecl)
import TokenId(tTrue,qualify)
import PackedString(unpackPS)

rmClasses tidFun state (DeclsParse topdecls) =
  case mapS rmClass topdecls (unpackPS (mrpsIS state)
                             ,\pos -> ExpCon pos (tidFun (tTrue,Con))) state of
    (codedecls,state) ->
      case unzip codedecls of
	(code,decls) -> (concat code,DeclsParse (concat decls),state)

rmClass (DeclClass pos ctx cls arg (DeclsParse decls)) =
  mapS fixArity decls >>>= \ decls ->
  unitS ([CodeClass pos cls] ,decls)
rmClass (DeclInstance pos ctx cls (TypeCons _ typ _) (DeclsParse decls)) =
  mapS fixArity decls >>>= \ decls ->
  unitS ([CodeInstance pos cls typ [] [] (concatMap identDecl decls)],decls)
rmClass decl                              = unitS ([],[decl])


fixArity decl@(DeclFun pos fun funs@(Fun args gdexps decls:_)) (rstr,true) state = 
  let wantArity = (arityVI . dropJust . lookupIS state) fun -- don't count ctx
      hasArity = length args
  in if wantArity == hasArity then
    (decl,state)
  else if wantArity < hasArity then
    --OLD: (DeclFun pos fun (map (toMany pos wantArity) funs),state)
    case uniqueISs state [0 .. hasArity] of
      ((_,newfuni):newargsi,state) ->
        let allArgs = map (ExpVar pos.snd) newargsi
        in case splitAt wantArity allArgs of
         (wantArgs,extraArgs) ->
           (DeclFun pos fun [Fun wantArgs 
                           [(true pos
                            ,ExpLambda pos extraArgs
                                       (ExpApplication pos
                                                       (ExpVar pos newfuni
                                                        :allArgs)))]
                           (DeclsParse [DeclFun pos newfuni funs])]
           ,updVarArity pos newfuni hasArity
            (addNewLetBound newfuni
                            (qualify rstr (reverse (strIS state fun ++'\'':show newfuni))) () state))

  else  -- want more
    case mapS (toFew pos (wantArity-hasArity)) funs () state of
      (funs,state) -> 
        (DeclFun pos fun funs,state)
fixArity decl _ state = (decl,state)


--toMany pos keep (Fun args gdexps decls) =
--  case splitAt keep args of 
--    (fargs,largs) -> Fun fargs (map ( \ (g,e) -> (g,ExpLambda pos largs e)) gdexps) decls

toFew pos add (Fun args gdexps decls) _ state =
    case uniqueISs state [1 .. add] of
      (newi,state) ->
        let eargs = map (ExpVar pos . snd) newi
	in  (Fun (args++eargs) (map ( \ (g,e) -> (g,ExpApplication pos (e:eargs))) gdexps) decls,state)
  
