module SyntaxPos(Pos,HasPos(..)) where

import Extra(noPos)
import Syntax

class HasPos a where
        getPos :: a -> Pos

instance HasPos (Decls a) where
     getPos (DeclsParse decls) = getPosList decls
     getPos (DeclsScc decls) = getPosList decls

instance HasPos (DeclsDepend a) where
     getPos (DeclsNoRec decl) = getPos decl
     getPos (DeclsRec   decls) = getPosList decls

instance HasPos (Decl a) where
    getPos (DeclType simple _)          = getPosSimple simple
    getPos (DeclDataPrim pos _ _)       = pos
    getPos (DeclData _ _ simple _ _)    = getPosSimple simple
    getPos (DeclConstrs pos _ _)        = pos
    getPos (DeclClass pos _ _ _ _ _)    = pos
    getPos (DeclInstance pos _ _ _ _)   = pos
    getPos (DeclDefault [])             = noPos
    getPos (DeclDefault (t:_))          = getPosType t
    getPos (DeclVarsType ((pos,_):_) _ _) = pos
    getPos (DeclFun pos fun funs)       = pos
    getPos (DeclPrimitive pos fun a t)  = pos
    getPos (DeclForeignImp pos _ s fun a c t _) = pos
    getPos (DeclForeignExp pos _ s fun t) = pos
    getPos (DeclPat alt)                = getPosAlt alt
    getPos (DeclIgnore str)             = noPos
    getPos (DeclError str)              = noPos

instance HasPos (Entity a) where
    getPos (EntityVar        pos _)   = pos
    getPos (EntityConClsAll  pos _)   = pos
    getPos (EntityConClsSome pos _ _) = pos

instance HasPos (Alt a) where
    getPos e = getPosAlt e

instance HasPos (Fun a) where
    getPos e = getPosFun e

instance HasPos (Rhs a) where
    getPos r = getPosRhs r

instance HasPos (Exp a) where
    getPos e = getPosExp e

instance HasPos a => HasPos [a] where
    getPos l = getPosList l

instance HasPos b => HasPos (a,b) where  -- used on GdExp
    getPos (a,b) = getPos b

instance HasPos (Simple a) where
    getPos s = getPosSimple s

instance HasPos (Type a) where
    getPos t = getPosType t

instance HasPos (Context a) where
    getPos (Context pos _ _) = pos

instance HasPos (FixId a) where
    getPos (FixCon pos a) = pos
    getPos (FixVar pos a) = pos

instance HasPos (Field a) where
    getPos (FieldExp pos _ _) = pos
    getPos (FieldPun pos _) = pos

instance HasPos (Constr a) where
    getPos (Constr pos _ _) = pos
    getPos (ConstrCtx _ _ pos _ _) = pos

-----------------------

getPosList [] = noPos
getPosList (x:xs) = getPos x

getPosSimple (Simple pos _ _) = pos

getPosAlt (Alt pat _ _) = getPosExp pat

getPosFun (Fun [] rhs _) = getPosRhs rhs
getPosFun (Fun (a:args) _ _)   = getPosExp a

getPosRhs (Unguarded e) = getPosExp e
getPosRhs (Guarded ((g,e):_)) = getPosExp g

getPosType (TypeApp  t1 t2) = getPosType t1
getPosType (TypeCons  pos _ _) = pos
getPosType (TypeVar   pos _)   = pos
getPosType (TypeStrict  pos _)   = pos

getPosExp (ExpDict              exp)       = getPosExp exp
getPosExp (ExpScc               str exp)   = getPosExp exp
getPosExp (ExpLambda            pos _ _)   = pos
getPosExp (ExpLet               pos _ _)   = pos
getPosExp (ExpDo 		pos _)	   = pos
getPosExp (ExpCase              pos _ _)   = pos
getPosExp (ExpFail)			   = error "No position for ExpFail"
getPosExp (ExpIf                pos _ _ _) = pos
getPosExp (ExpType              pos _ _ _) = pos
getPosExp (ExpRecord            exp fdefs) = getPosExp exp
getPosExp (ExpApplication       pos _ )    = pos
getPosExp (ExpInfixList         pos _)     = pos
getPosExp (ExpVar               pos _)     = pos
getPosExp (ExpCon               pos _)     = pos
getPosExp (ExpVarOp             pos _)     = pos
getPosExp (ExpConOp             pos _)     = pos
getPosExp (ExpLit               pos _)     = pos
-- getPosExp (ExpTuple             pos _)     = pos
getPosExp (ExpList              pos _)     = pos
getPosExp (Exp2                 pos i1 i2) = pos
getPosExp (PatAs                pos _ _)   = pos
getPosExp (PatWildcard          pos)       = pos
getPosExp (PatIrrefutable       pos _)     = pos
getPosExp (PatNplusK            pos _ _ _ _ _) = pos

