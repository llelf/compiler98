{- ---------------------------------------------------------------------------

Tests if illegal instance for a type synonym is made.

Provides function to transform type from syntax tree into interal type.
-}
module Extract(extract,type2NT) where

import Syntax(Type(..),Decls(..),Decl(..),Fun(..),Exp(..),Stmt(..),Alt(..))
import IntState(IntState,lookupIS,depthI,strIS,addError,superclassesI
               ,instancesI,updVarArity,updVarNT)
import NT(NT(..),NewType(..))
import Reduce(Reduce,mapR,unitR,(>>>))
import Rename(ctxs2NT)
import IExtract(freeType)
import Extra(snub,strPos,mixSpace,isJust,dropJust,mixCommaAnd,isNothing)
import Bind(identPat)
import SyntaxPos(HasPos(getPos))
import AssocTree(Tree,lookupAT)


{- transform type from syntax tree into internal type -}

type2NT :: Type Int -> NT 

type2NT (TypeApp t1 t2) = NTapp (type2NT t1) (type2NT t2)
type2NT (TypeCons _ ci ts) = NTcons ci (map type2NT ts)
type2NT (TypeStrict _ t) = NTstrict (type2NT t)
type2NT (TypeVar _ v) = NTvar v

{-

-}
extract :: Decls Int -> IntState -> IntState
extract = extractDecls 


extractDecls :: Decls Int -> IntState -> IntState
extractDecls (DeclsParse decls)   =
    mapR extractDecl decls


extractDecl :: Decl Int -> Reduce IntState IntState

extractDecl (DeclInstance pos ctxs cls instanceType@(TypeCons poscon con _) 
  instmethods) =
  (\ state ->
    if (isJust . depthI . dropJust . lookupIS state) con then
      addError state ("Instance declaration of type synonym is illegal (" 
                      ++ strIS state con ++ " at " ++ strPos poscon ++ ")")
    else case (filter (isNothing.snd) 
              . map ( \ cls -> (cls,(flip lookupAT con . instancesI . dropJust
                                     . lookupIS state) cls) ) 
              . superclassesI 
              . dropJust 
              . lookupIS state) cls of
           [] -> state
           clss -> addError state 
                     ("Instance declaration for the class " 
                      ++ strIS state cls ++ " at " ++ strPos pos 
                      ++ " needs instance(s) of "
 		      ++ mixCommaAnd (map (strIS state . fst) clss) ++ ".")
  ) >>>
  extractDecls instmethods    -- error if we find any type signatures
extractDecl (DeclClass pos tctxs tClass tTVar (DeclsParse decls)) = 
  mapR extractDecl' decls
extractDecl (DeclPrimitive pos ident arity typ) =
  let nt = NewType (snub (freeType typ)) [] [] [type2NT typ]
  in updVarNT pos ident nt >>> updVarArity pos ident arity
extractDecl (DeclForeignImp pos _ ident arity cast typ) =
  let nt = NewType (snub (freeType typ)) [] [] [type2NT typ]
  in updVarNT pos ident nt >>> updVarArity pos ident arity
extractDecl (DeclForeignExp pos _ ident typ) =
  let nt = NewType (snub (freeType typ)) [] (ctxs2NT []) [type2NT typ]
  in updVarNT pos ident nt
extractDecl (DeclVarsType posidents ctxs typ) =
  let nt = NewType (snub (freeType typ)) [] (ctxs2NT ctxs) [type2NT typ]
  in mapR ( \ (pos,i) -> updVarNT pos i nt) posidents
extractDecl (DeclPat alt) =  extractDeclAlt alt
extractDecl (DeclFun pos fun funs) = 
  updFunArity pos fun funs >>> mapR extractFun funs
extractDecl d = unitR

{-
extractDecl' is used in class declarations 
as we don't want to use top level signatures there
-}
extractDecl' :: Decl Int -> IntState -> IntState

extractDecl' (DeclPat alt) =   extractDeclAlt alt
extractDecl' (DeclFun pos fun funs) =  
  updFunArity pos fun funs >>> mapR extractFun funs
extractDecl' d = unitR


updFunArity :: a -> Int -> [Fun b] -> IntState -> IntState

updFunArity pos fun funs =
  case map fA funs of
    (a:xs) ->
      if all (a==) xs
        then updVarArity pos fun a
        else \ state -> 
           addError state ("Multiple arities for " ++ strIS state fun ++ ": "
 			   ++ mixSpace (map (\ (pos,a) -> "arity " 
                           ++ show a ++ " at " ++ strPos pos) (map fPA funs)))
  where
  fA (Fun args gdexps decls) = (length args)
  fPA (Fun args gdexps decls) = (getPos args,length args)


extractFun :: Fun Int -> Reduce IntState IntState
extractFun (Fun  pats guardedExps decls) =
    mapR extractGuardedExp guardedExps >>>
    extractDecls decls

extractGuardedExp (guard,exp) =
    extractExp guard >>> extractExp exp

extractDeclAlt (Alt  pat guardedExps decls) =
        mapR ( \ (pos,ident) -> updVarArity pos ident 0) (identPat pat) >>>
	mapR extractGuardedExp guardedExps >>>
	extractDecls decls

extractAlt (Alt  pat guardedExps decls) =
	mapR extractGuardedExp guardedExps >>>
	extractDecls decls


extractExp :: Exp Int -> IntState -> IntState

extractExp (ExpScc            str exp)            = extractExp exp
extractExp (ExpLambda         pos pats exp)       = extractExp exp
extractExp (ExpLet            pos decls exp)      = 
  extractExp exp  >>> extractDecls decls
extractExp (ExpDo             pos stmts)          = mapR extractStmt stmts
extractExp (ExpCase           pos exp alts)       = 
  extractExp exp  >>> mapR extractAlt alts
extractExp (ExpIf             pos expC expT expE) = 
  extractExp expC >>> extractExp expT >>> extractExp expE
extractExp (ExpType           pos exp ctxs typ)   = extractExp exp
--- Above only in expressions
extractExp (ExpApplication   pos exps)  = mapR extractExp exps
extractExp (ExpList          pos exps)  = mapR extractExp exps
extractExp e                            = unitR


extractStmt :: Stmt Int -> Reduce IntState IntState

extractStmt (StmtExp  exp) = extractExp exp
extractStmt (StmtBind pat exp) = 
        mapR ( \ (pos,ident) -> updVarArity pos ident 0) (identPat pat) >>>
	extractExp exp
extractStmt (StmtLet decls) = extractDecls decls
