{- ---------------------------------------------------------------------------
Remove class and instance declarations from declarations of a module,
that is, replace them by their type and method declarations.
Create separate list for information about class and instance declarations.
Also changes arity of method definitions according to the arity in the symbol
table (why?; arity in the symbol table is number of function arrows in type)
-}
module RmClasses(rmClasses) where

import IdKind
import IntState
import Syntax
import Extra(dropJust)
import State
import Bind(identDecl)
import TokenId(tTrue,qualify)
import PackedString(unpackPS)
import Id(Id)


type RmClassMonad a = (String,Id -> Exp Id) -> IntState -> (a,IntState)
type RmClassMonad2 a b = a -> IntState -> (b,IntState)


{- main function of the module -}
rmClasses :: ((TokenId,IdKind) -> Id) -> IntState -> Decls Id 
          -> ([ClassCode a Id] -- separate list about class and instance decls
             ,Decls Id         -- modified decls, without class and inst. decls
             ,IntState)

rmClasses tidFun state (DeclsParse topdecls) =
  case mapS rmClass topdecls (unpackPS (mrpsIS state)
                             ,\pos -> ExpCon pos (tidFun (tTrue,Con))) state of
    (codedecls,state) ->
      case unzip codedecls of
	(code,decls) -> (concat code,DeclsParse (concat decls),state)


rmClass :: Decl Id -> RmClassMonad ([ClassCode a Id],[Decl Id])

rmClass (DeclClass pos ctx cls arg (DeclsParse decls)) =
  mapS fixArity decls >>>= \ decls ->
  unitS ([CodeClass pos cls] ,decls)
rmClass (DeclInstance pos ctx cls (TypeCons _ typ _) (DeclsParse decls)) =
  mapS fixArity decls >>>= \ decls ->
  unitS ([CodeInstance pos cls typ [] [] (concatMap identDecl decls)],decls)
rmClass decl = unitS ([],[decl])


fixArity :: Decl Id -> RmClassMonad (Decl Id)

fixArity decl@(DeclFun pos fun funs@(Fun args gdexps decls:_)) 
  (rstr,true) state = 
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

toFew :: Pos -> Int -> Fun Id -> RmClassMonad2 a (Fun Id)

toFew pos add (Fun args gdexps decls) _ state =
    case uniqueISs state [1 .. add] of
      (newi,state) ->
        let eargs = map (ExpVar pos . snd) newi
	in  (Fun (args++eargs) (map ( \ (g,e) -> (g,ExpApplication pos (e:eargs))) gdexps) decls,state)
  
{- End RmClasses ------------------------------------------------------------}