module Need(needProg) where

import Reduce
import NeedLib
import Syntax
import IdKind
import PreImport
import TokenId
import DbgId
import TokenInt
import Flags
import SyntaxPos
import Extra
import SyntaxUtil(infixFun)
import Tree234

needProg flags n@(Module pos modidl exports impdecls fixdecls topdecls) inf =
  let qualFun = qualRename impdecls
  in case needit (needModule (sDbgTrans flags || sDbgPrelude flags) n) qualFun (initNeed (modidl == tMain)) of
       (need,overlap) -> (need,qualFun,overlap,preImport flags modidl (treeMap fst need) exports impdecls)

needModule debugging (Module pos modid exports imports fixdecls topdecls) =
      pushNeed >>>
      bindDataDecls topdecls >>>
      bindDecls topdecls >>>
      pushNeed >>>
      bindTid Modid modid >>>
      mapR bindImport imports >>>
      mapR needExport exports >>>
      popNeed >>>
      mapR needImport imports >>>
      mapR needFixDecl fixdecls >>>
      needDecls topdecls >>>
      (if debugging then needTids pos tokenDbg else unitR) >>>
      popNeed


--------------------------------

needExport  (ExportEntity  pos entity) =
    needEntity entity
needExport  (ExportModid   pos hs) =
    needTid pos Modid hs

needEntity (EntityVar  pos hs) =             		-- varid
    needTid pos Var hs
needEntity (EntityTyConCls  pos hs) =             	-- TyCon(..) | TyCls(..)
    needTid pos TC hs
needEntity (EntityTyCon  pos hs posidents) =   	-- TyCon | TyCon(conid,..,conid)
       needTid pos TCon hs
    >>> needPosIdents Con posidents
needEntity (EntityTyCls  pos hs posidents) =   	-- TyCls(varid,..,varid) 
       needTid pos TClass hs
    >>> needPosIdents Method posidents

needPosIdents kind posidents = 
    mapR ( \ (pos,tid) -> needTid pos kind tid) posidents

-----------------------------------

--needImport (Import (pos,tid) impspec) =
--    {- needTid pos Modid tid >>> -} needImpSpec impspec
--needImport (ImportQ (pos,tid)) =
--    unitR -- needTid pos Modid tid
--needImport (ImportQas (pos,tid) (pos2,tid2)) =
--    unitR -- needTid pos Modid tid
needImport (Import (pos,tid) impspec) = needImpSpec impspec
needImport (ImportQ (pos,tid) impspec) = needImpSpec impspec
needImport (ImportQas (pos,tid) (pos2,tid2) impspec) = needImpSpec impspec
needImport (Importas (pos,tid) (pos2,tid2) impspec) = needImpSpec impspec

needImpSpec (NoHiding entities) = mapR needEntity entities
needImpSpec (Hiding entities)   = unitR

-----------------------------------

needFixDecl (InfixPre tid,level,posidents) =
  needTid (getPos (head posidents)) Var tid >>> mapR needFixId posidents
needFixDecl (typeClass,level,posidents) = 
  mapR needFixId posidents

needFixId (FixCon pos tid) = needTid pos Con tid
needFixId (FixVar pos tid) = needTid pos Var tid

-----------------------------------

needDecls (DeclsParse decls)   = mapR needDecl decls

--        type   simple  = type
needDecl (DeclType simple typ) =
     pushNeed
  >>> needSimple TSyn simple
  >>> needType typ
  >>> popNeed

--        data primitive type = size
needDecl (DeclDataPrim pos tid size) = 
  unitR

--        data context => simple = constrs deriving (tycls)
needDecl (DeclData b ctxs simple constrs posidents) =
     mapR needCtx ctxs
  >>> mapR needConstr constrs
  >>> mapR needDeriving posidents
  >>> unitR		-- needTids (getPos simple) tokenEval


--        class context => class where { csign; valdef }
needDecl (DeclClass pos tctxs tClass tTVar (DeclsParse decls)) =
     pushNeed
  >>> bindTid TVar tTVar
  >>> mapR needCtx tctxs
  >>> mapR needClassInst decls
  >>> popNeed

--        instance context => tycls inst where { valdef }
needDecl (DeclInstance pos ctxs tClass inst (DeclsParse decls)) =
     mapR needCtx ctxs
  >>> needType inst
  >>> mapR needClassInst decls
  >>> needTid pos TClass tClass

--        default (type,..)
needDecl (DeclDefault types) =
     mapR needType types

--      vars :: context => type
needDecl (DeclVarsType posidents ctxs typ) =
     mapR (\ (pos,tid) -> needTid pos Var tid) posidents
  >>> mapR needCtx ctxs
  >>> needType typ

needDecl (DeclPat (Alt pat@(ExpInfixList pos pats) gdexps decls)) =
      pushNeed
  >>> bindPat pat   -- Also generate need for constructors
  >>> needExp pat
  >>> bindDecls decls
  >>> mapR needGdExp gdexps
  >>> needDecls decls
  >>> popNeed

needDecl (DeclPat (Alt  pat gdexps decls)) =
     needExp pat
  >>> bindDecls decls
  >>> mapR needGdExp gdexps
  >>> needDecls decls

needDecl (DeclFun pos hs funs) =
      mapR needFun funs
needDecl (DeclPrimitive pos hs arity t) =
      needType t
needDecl (DeclForeignImp pos _ hs arity cast t) =
      needType t
needDecl (DeclForeignExp pos _ hs typ) =
      needTid pos Var hs
  >>> needType typ
   -- error ("\nAt "++ strPos pos ++ ", foreign export not supported.")
needDecl (DeclFixity f) =
      needFixDecl f

--     Used for unimplemented things
needDecl d@(DeclIgnore str) = unitR
needDecl d@(DeclError str) = unitR
needDecl (DeclAnnot decl annots) = unitR


needDeriving (pos,tid)
	| (ensureM rpsPrelude tid) == tBounded = needTid pos TClass tid >>> needTids pos tokenBounded
	| (ensureM rpsPrelude tid) == tEnum    = needTid pos TClass tid >>> needTids pos tokenEnum
	| (ensureM rpsPrelude tid) == tEq      = needTid pos TClass tid >>> needTids pos tokenEq
	| (ensureM rpsPrelude tid) == tIx      = needTid pos TClass tid >>> needTids pos tokenIx
	| (ensureM rpsPrelude tid) == tOrd     = needTid pos TClass tid >>> needTids pos tokenOrd
	| (ensureM rpsPrelude tid) == tRead    = needTid pos TClass tid >>> needTids pos tokenRead
	| (ensureM rpsPrelude tid) == tShow    = needTid pos TClass tid >>> needTids pos tokenShow
	| (ensureM rpsBinary tid)  == tBinary  = needTid pos TClass tid >>> needTids pos tokenBinary		--MALCOLM
  	| True = strace ("Warning: Don't know what is needed to derive "
				 ++ show tid ++ " at " ++ strPos pos)
		 (needTid pos TClass tid)

needClassInst (DeclVarsType posidents ctxs typ) =
     mapR needCtx ctxs
  >>> needType typ
needClassInst (DeclPat (Alt (ExpVar pos fun) gdexps decls)) =
      needTid pos Method fun
  >>> needFun (Fun [] gdexps decls)
needClassInst (DeclPat (Alt (ExpInfixList pos es) gdexps decls)) =
  case infixFun es of
    Just (pat1,pos',fun',pat2) ->
	 needTid pos Method fun'
      >>> pushNeed
      >>> bindPat pat1 >>> bindPat pat2
      >>> bindDecls decls  
      >>> needExp pat1 >>> needExp pat2
      >>> mapR needGdExp gdexps
      >>> needDecls decls
      >>> popNeed
    Nothing ->
      error ("Sorry (infix) lhs-patterns doesn't work in instances " ++ strPos pos)
needClassInst (DeclPat (Alt pat gdexps decls)) =
  error ("Sorry lhs-patterns doesn't work in instances " ++ strPos (getPos pat))
needClassInst (DeclFun pos fun funs) =
     needTid pos Method fun
  >>> mapR needFun funs
needClassInst (DeclAnnot decl annots) =
     needClassInst decl

needFun (Fun  pats guardedExps decls) =
     pushNeed
  >>> mapR bindPat pats  -- Also generate need for constructors
  >>> bindDecls decls
  >>> mapR needGdExp guardedExps
  >>> needDecls decls
  >>> popNeed


needGdExp (guard,exp) = needExp guard >>> needExp exp


needAlt (Alt  pat guardedExps decls) =
     pushNeed
  >>> bindPat pat  -- Also generate need for constructors
  >>> bindDecls decls
  >>> needExp pat
  >>> mapR needGdExp guardedExps
  >>> needDecls decls
  >>> popNeed

needType (TypeApp t1 t2) = needType t1 >>> needType t2
needType (TypeCons  pos hs types) = needTid pos TCon hs >>> mapR needType types
needType (TypeVar   pos hs)       = unitR
needType (TypeStrict pos typ)     = needType typ

needSig (Sig posidents typ) = needType typ

needSimple kind (Simple pos hs posidents) = needTid pos kind hs -- posidents are typevariables!

needCtx (Context pos hs _) = needTid pos TClass hs

needConstr (Constr                pos hs types) = mapR needFieldType types
needConstr (ConstrCtx forall ctxs pos hs types) = mapR needCtx ctxs >>> mapR needFieldType types

needFieldType (_,typ) = needType typ

needStmts [] = unitR
needStmts (StmtExp exp:[]) = needExp exp
needStmts (StmtExp exp:r) = needTid (getPos exp) Var t_gtgt >>>  needExp exp >>> needStmts r
needStmts (StmtBind pat exp:r) = needTid (getPos pat) Var t_gtgteq >>> needExp exp >>> pushNeed >>> bindPat pat >>> needStmts r >>> popNeed
needStmts (StmtLet decls :r) =  pushNeed  >>> bindDecls decls  >>> needDecls decls >>> needStmts r >>> popNeed

needField (FieldExp pos var exp) = needTid pos Field var >>> needExp exp
needField (FieldPun pos var) = needTid pos Field var >>> needTid pos Var var
--needField (FieldPun pos var) = error ("\nAt "++ strPos pos ++ ", token: "++
--      show var ++
--      "\nPunning of named fields has been removed from the Haskell language."++
--      "\nUse "++show var++"="++show var++" instead.")

needExp (ExpScc            str exp) =  needExp exp
needExp (ExpLambda         pos pats exp) =
     pushNeed  >>> mapR bindPat pats  >>> needExp exp  >>> popNeed
needExp (ExpDo            pos stmts) = needTids pos tokenMonad >>> needStmts stmts
needExp (ExpLet            pos decls exp) =
     pushNeed  >>> bindDecls decls  >>> needDecls decls >>> needExp exp  >>> popNeed
needExp (ExpCase           pos exp alts) =  needExp exp  >>> mapR needAlt alts
needExp (ExpIf             pos expCond expThen expElse) =
     needExp expCond >>> needExp  expThen >>> needExp expElse
needExp (ExpRecord exp fields) = needExp exp >>> mapR needField fields
needExp (ExpType           pos exp ctxs typ) =
    needExp exp >>> mapR needCtx ctxs >>> needType typ
--- Above only in expressions
needExp (ExpApplication   pos exps) = mapR needExp exps
needExp (ExpInfixList     pos exps) = mapR needExp exps
needExp (ExpVar           pos tid)  = needTid pos Var tid
needExp (ExpCon           pos tid)  = needTid pos Con tid
needExp (ExpVarOp         pos tid)  = needTid pos Var tid
needExp (ExpConOp         pos tid)  = needTid pos Con tid
needExp e@(ExpLit         pos (LitInteger  _ _)) = needTids pos tokenInteger
needExp e@(ExpLit         pos (LitRational _ _)) = needTids pos tokenRational
needExp e@(ExpLit         pos lit)  = unitR
needExp (ExpList          pos exps) = mapR needExp exps
--- Below only in patterns
needExp (PatAs            pos hs pat) = needTid pos Var hs >>> needExp pat
needExp (PatWildcard      pos)        = unitR
needExp (PatIrrefutable    pos pat)   = needExp pat
needExp (PatNplusK        pos tid _ _ _ _) = needTid pos Var tid >>>
                                             needTids pos tokenNplusK


----------- ========================

bindImport (Import (pos,tid) impspec) =
    bindTid Modid tid
bindImport (ImportQ (pos,tid) impspec) =
    bindTid Modid tid
bindImport (ImportQas (pos,tid) (pos2,tid2) impspec) =
    bindTid Modid tid >>> bindTid Modid tid2
bindImport (Importas (pos,tid) (pos2,tid2) impspec) =
    bindTid Modid tid >>> bindTid Modid tid2

-- Hack to enforce that constructors are bound before need is checked
bindDataDecls (DeclsParse decls)   = mapR bindDataDecl decls

bindDataDecl (DeclType (Simple pos tid posidents) typ) =  bindTid TSyn tid
bindDataDecl (DeclDataPrim pos tid size) = bindTid TCon tid
bindDataDecl (DeclData b ctxs (Simple pos tid posidents) constrs _) = bindTid TCon tid >>> mapR bindConstr constrs
bindDataDecl _ = unitR


bindDecls (DeclsParse decls)   = mapR bindDecl decls

bindDecl (DeclType (Simple pos tid posidents) typ) =  unitR -- bindTid TSyn tid
bindDecl (DeclDataPrim pos tid size) = unitR -- bindTid TCon tid
bindDecl (DeclData b ctxs (Simple pos tid posidents) constrs _) = unitR -- bindTid TCon tid >>> mapR bindConstr constrs
bindDecl (DeclClass pos tctxs tClass tTVar (DeclsParse decls)) = bindTid TClass tClass >>> mapR bindClass decls
bindDecl (DeclInstance pos ctxs tClass inst (DeclsParse decls)) = unitR
bindDecl (DeclDefault types) = unitR
bindDecl (DeclVarsType posidents ctxs typ) = unitR
bindDecl (DeclPat (Alt pat@(ExpInfixList pos pats) _ _)) =
    case filter isVarOp pats of
        [ExpVarOp pos tid] -> bindTid Var tid
        [] -> bindPat pat
        _ -> error (show pos ++ ": (n+k) patterns are not supported\n")
bindDecl (DeclPat (Alt pat gdexps decls)) = bindPat pat  -- Also generate need for constructors
bindDecl (DeclPrimitive pos tid arity t) = bindTid Var tid
bindDecl (DeclForeignImp pos _ tid arity cast t) = bindTid Var tid
bindDecl (DeclForeignExp pos _ tid t) = unitR
bindDecl (DeclFun pos tid funs) = bindTid Var tid
bindDecl d@(DeclIgnore str) = unitR
bindDecl d@(DeclError str) = unitR
bindDecl (DeclAnnot decl annots) = unitR
bindDecl (DeclFixity f) = unitR

bindConstr (Constr                pos hs ftypes) = bindTid Con hs >>> mapR bindFieldType ftypes
bindConstr (ConstrCtx forall ctxs pos hs ftypes) = bindTid Con hs >>> mapR bindFieldType ftypes

bindFieldType (Nothing,_) = unitR
bindFieldType (Just posidents,_) = mapR ( \ (p,v) -> bindTid Var v >>> bindTid Field v) posidents

bindClass (DeclVarsType posidents ctxs typ) = mapR (bindTid Method . snd) posidents
bindClass _ = unitR

bindField (FieldExp pos var pat) = needTid pos Field var >>> bindTid Var var >>> bindPat pat
bindField (FieldPun pos var) = needTid pos Field var >>> bindTid Var var
--bindField (FieldPun pos var) = error ("\nAt "++ strPos pos ++ ", token: "++
--      show var ++
--      "\nPunning of named fields has been removed from the Haskell language."++
--      "\nUse "++show var++"="++show var++" instead.")

--- Above only in expressions
bindPat (ExpApplication   pos exps) = mapR bindPat exps
bindPat (ExpInfixList     pos (ExpVarOp _ _:pats)) = mapR bindPat pats -- must be prefix -
bindPat (ExpInfixList     pos exps) = mapR bindPat exps
bindPat (ExpVar           pos tid)  = bindTid Var tid
bindPat (ExpCon           pos tid)  = needTid pos Con tid
bindPat (ExpVarOp         pos tid)  = bindTid Var tid
bindPat (ExpConOp         pos tid)  = needTid pos Con tid

bindPat e@(ExpLit         pos (LitInteger  _ _)) = needTid pos Var t_equalequal >>> needTids pos tokenInteger
bindPat e@(ExpLit         pos (LitRational _ _)) = needTid pos Var t_equalequal >>> needTids pos tokenRational
bindPat e@(ExpLit         pos lit)  = unitR

bindPat (ExpList          pos exps) = mapR bindPat exps
bindPat (ExpRecord pat fields) = bindPat pat >>> mapR bindField fields   -- pat is alwasy ExpCon
--- Below only in patterns
bindPat (PatAs            pos hs pat) = bindTid Var hs >>> bindPat pat
bindPat (PatWildcard      pos)        = unitR
bindPat (PatIrrefutable   pos pat)    = bindPat pat
bindPat (PatNplusK        pos tid _ _ _ _) = bindTid Var tid >>>
                                             needTids pos tokenNplusK


------

needTids pos kindtids = mapR (uncurry (needTid pos)) kindtids

isVarOp (ExpVarOp _ _) = True
isVarOp _ = False

