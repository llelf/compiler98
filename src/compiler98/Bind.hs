module Bind(bindDecls,bindPat,identDecl,identPat) where

import State
import Kind
import Extra(strPos,noPos)
import TokenId(TokenId)
import Syntax
import SyntaxPos
import RenameLib
import Flags
------------------------

bindDecls (DeclsParse decls) =  mapS0 bindDecl decls

bindDecl  (DeclType (Simple pos tid tvs) typ) =
    bindTid pos TSyn tid
bindDecl  (DeclDataPrim pos tid size) = 
    bindTid pos TCon tid
bindDecl  (DeclData d ctxs (Simple pos tid tvs) constrs posidents) =
    bindTid pos TCon tid >>> mapS0 bindConstr constrs
bindDecl  (DeclClass pos ctxs tid tvarIdent methods) =
    bindTid pos TClass tid >>> bindMethods methods
bindDecl  (DeclInstance pos ctx classIdent instanceType methods) =
    unitS0
bindDecl  (DeclDefault types) =
    unitS0
bindDecl  (DeclPrimitive pos tid arity typ) =
    bindTid pos Var tid
bindDecl  (DeclVarsType posidents ctxs typ) =
    unitS0
bindDecl  (DeclPat (Alt pat guaredExps decls)) =
    bindPat Var pat
bindDecl  (DeclFun pos tid funs) =
    bindTid pos Var tid
bindDecl  (DeclFixity fix) =
    unitS0

--     Used for unimplemented things
bindDecl  (DeclIgnore str)     = unitS0
bindDecl  (DeclError str)      = unitS0
bindDecl  (DeclAnnot decl str) = error "DeclAnnot"

---- =====

bindMethods (DeclsParse decls) =  mapS0 bindMethod decls

bindMethod (DeclVarsType posidents ctxs typ) =  mapS0 ( \ (pos,tid) -> bindTid pos Method tid) posidents
bindMethod _ = unitS0

-- bindAlt kind 

-----------------

bindConstr (Constr                pos tid fieldtypes) = bindTid pos  Con tid >>> mapS0 (bindFieldVar . fst) fieldtypes
bindConstr (ConstrCtx forall ctxs pos tid fieldtypes) = bindTid pos  Con tid >>> mapS0 (bindFieldVar . fst) fieldtypes

bindFieldVar Nothing = unitS0
bindFieldVar (Just posidents) = mapS0 ( \ (p,v) -> checkTid p Field v >>>= \ exist -> 
						   if exist then unitS0 else bindTid p Field v >>> bindTid p Var v)
						    posidents

-- bindPat does not bind Constr!
----------------

bindPat kind (ExpScc            str exp)                     = error "ExpScc in bindPat!"
bindPat kind (ExpLambda         pos pats exp)                = error "ExpLambda in bindPat!"
bindPat kind (ExpLet            pos decls exp)	       = error "ExpLet in bindPat!"
bindPat kind (ExpCase           pos exp alts) 	       = error "ExpCase in bindPat!"
bindPat kind (ExpIf             pos expCond expThen expElse) = error "ExpIf in bindPat!"
bindPat kind (ExpType           pos exp ctxs typ) 	       = error "ExpType in bindPat!"
--- Above only in expressions
bindPat kind (ExpRecord		pos fields) = mapS0 bindField fields
bindPat kind (ExpApplication    pos pats) = mapS0 (bindPat kind) pats
bindPat kind (ExpInfixList      pos (ExpVarOp _ _:pats)) = mapS0 (bindPat kind) pats -- prefix operator, only - is legal
bindPat kind (ExpInfixList      pos pats) = mapS0 (bindPat kind) pats
bindPat kind (ExpVar            pos tid) = bindTid pos kind tid
bindPat kind (ExpCon            pos tid) = unitS0
bindPat kind (ExpVarOp          pos tid) = bindTid pos kind tid
bindPat kind (ExpConOp          pos tid) = unitS0
bindPat kind (ExpLit            pos lit) = unitS0
-- bindPat kind (ExpTuple          pos pats) = mapS0 (bindPat kind) pats
bindPat kind (ExpList           pos pats) = mapS0 (bindPat kind) pats
--- Below only in patterns
bindPat kind (PatAs             pos tid pat) = bindTid pos kind tid >>> bindPat kind pat
bindPat kind (PatWildcard       pos) = unitS0
bindPat kind (PatIrrefutable    pos pat) = bindPat kind pat
bindPat kind (PatNplusK         pos tid tid' int _ _) = bindTid pos kind tid

bindField (FieldExp pos tid pat) = {- bindTid pos Var tid >>> -} bindPat Var pat
--bindField (FieldPun pos tid) = bindTid pos Var tid	-- H98 removes


--------------------------------------

identDecl (DeclPat (Alt pat gdexps decls)) = map snd (identPat pat)
identDecl (DeclFun pos fun (Fun args gdexps decls:_)) = [fun]
identDecl (DeclPrimitive pos fun arity t) = [fun]
identDecl _ = []

addIdent pos ident _ env = (pos,ident):env

identPat pat = identPat' pat () []

identPats pats = mapS0 identPat' pats () []

identPat' (ExpScc            str exp)                     = error ("ExpScc in identPat'" ++ show (getPos exp))
identPat' (ExpLambda         pos pats exp)                = error ("ExpLambda in identPat'" ++ show pos)
identPat' (ExpLet            pos decls exp)	       = error ("ExpLet in identPat'" ++ show pos)
identPat' (ExpDo             pos stmts)  	       = error ("ExpDo in identPat'" ++ show pos)
identPat' (ExpCase           pos exp alts) 	       = error ("ExpCase in identPat'" ++ show pos)
identPat' (ExpIf             pos expCond expThen expElse) = error ("ExpIf in identPat'" ++ show pos)
identPat' (ExpType           pos exp ctxs typ) 	       = error ("ExpType in identPat'" ++ show pos)
--- Above only in expressions
identPat' (ExpRecord         pos fields) = mapS0 identField fields
identPat' (ExpApplication    pos (ExpVar _ _:pats)) = mapS0 identPat' pats -- ignore negate!!
identPat' (ExpApplication    pos pats) = mapS0 identPat' pats
identPat' (ExpInfixList      pos (ExpVarOp _ _:pats)) = mapS0 identPat' pats -- ignore negate!!
identPat' (ExpInfixList      pos pats) = mapS0 identPat' pats
identPat' (ExpVar            pos ident) = addIdent pos ident
identPat' (ExpCon            pos ident) = unitS0
identPat' (ExpVarOp          pos ident) = addIdent pos ident
identPat' (ExpConOp          pos ident) = unitS0
identPat' (ExpLit            pos lit) = unitS0
--identPat' (ExpTuple          pos pats) = mapS0 identPat' pats
identPat' (ExpList           pos pats) = mapS0 identPat' pats
identPat' (Exp2 pos tid1 tid2) = unitS0 -- addIdent noPos tid1 >>> addIdent noPos tid2
--- Below only in patterns
identPat' (PatAs             pos ident pat) = addIdent pos ident >>> identPat' pat
identPat' (PatWildcard       pos) = unitS0
identPat' (PatIrrefutable    pos pat) = identPat' pat
identPat' (PatNplusK         pos n n' k _ _) = addIdent pos n >>> addIdent pos n'
identPat' (ExpDict               pat) = identPat' pat
identPat' p = error ("No match in identPat' at " ++ strPos (getPos p))

identField (FieldExp pos ident pat) = identPat' pat
--identField (FieldPun  pos ident) = addIdent pos ident	-- H98 removes
