{- ---------------------------------------------------------------------------
Functions to convert an identifier (either TokenId or Unique) 
or a simple syntactic construct into a printable string.
-}
module StrSyntax where

import Lex
import Syntax
import Extra(mix,mixComma,mixSpace,strChr,strStr)
import TokenId(TokenId)
import Id(Id)
import IntState(IntState,strIS)
import PackedString (unpackPS)

class StrId a where
  pStd :: IntState -> a -> String
  pFix :: IntState -> a -> String
  pType :: Bool -> IntState -> Pos -> a -> [Type a] -> String

instance StrId TokenId where  -- state is error here!
  pStd state tid = show tid
  pFix state tid = show tid
  pType b state pos tid [] = show tid
  pType b state pos tid ts = "(" ++ show tid ++ " " ++ mixSpace (map (strType b state) ts) ++ ")"

instance StrId Int {-Id-} where
  pStd state i = strIS state i
  pFix state i = strIS state i
  pType b state pos i [] = strIS state i
  pType b state pos i ts = "(" ++ strIS state i ++ " " ++ mixSpace (map (strType b state) ts) ++ ")"

strAnnots d p annots = mixSpace (map (strAnnot d p) annots)

strAnnot d p (AnnotArity (pos,ident) int) =
        "{-# ARITY " ++ (pStd p) ident ++ " = " ++ show int ++ "#-}"
strAnnot d p (AnnotPrimitive (pos,ident) prim) = 
        "{-# PRIMITIVE " ++ (pStd p) ident ++ " = " ++ unpackPS prim ++ "#-}"
strAnnot d p (AnnotNeed posidents) =
        "{-# NEED " ++ mixSpace (map strNeed posidents) ++ "#-}"
   where strNeed [x] = (pStd p) x
         strNeed xs  = "{" ++  mixSpace (map (pStd p) xs) ++ "}"
strAnnot d p (AnnotUnknown) =
        ("{-# ??? #-}")


strExports :: StrId a => b -> IntState -> [Export a] -> String

strExports d p [] =
        ""
strExports d p exports =
        "(" ++ mixComma (map (strExport d p) exports) ++ ")"


strExport :: StrId a => b -> IntState -> Export a -> [Char]

strExport d p (ExportEntity pos entity) =
        strEntity d p entity
strExport d p (ExportModid pos id) =
        (pStd p) id ++ ".."


strEntity d p (EntityVar  pos id) =
        (pStd p) id
strEntity d p (EntityTyConCls  pos id) =
        (pStd p) id ++ "(..)"
strEntity d p (EntityTyCon  pos id []) =
        (pStd p) id 
strEntity d p (EntityTyCon  pos id ids) =
        (pStd p) id ++ "(" ++ mixComma (map ((pStd p).snd) ids) ++ ")"
strEntity d p (EntityTyCls  pos id ids) =
        (pStd p) id ++ "(" ++ mixComma (map ((pStd p).snd) ids) ++ ")"

strImpSpec d p (NoHiding []) =
        "()"
strImpSpec d p (NoHiding entitys) = 
        "(" ++ mixComma (map (strEntity d p) entitys) ++ ")"
strImpSpec d p (Hiding []) =
        ""
strImpSpec d p (Hiding entitys) = 
        "hiding (" ++ mixComma (map (strEntity d p) entitys) ++ ")"

strFixDecl d p (c,i,ids) =
        show c ++ show i ++ " " ++ mixComma (map ((pFix p).stripFixId) ids)

strDerivings d p []  = ""
strDerivings d p [(_,der)] = "deriving " ++ (pStd p) der
strDerivings d p ds  = "deriving(" ++ mixComma (map ((pStd p).snd) ds) ++ ")"

strConstr d p (Constr pos c cs) =
        (pStd p) c ++ " " ++ mixSpace (map (strFieldType d p) cs)
strConstr d p (ConstrCtx [] ctxs pos c cs) =
        strContexts d p ctxs ++ (pStd p) c ++ " " ++ mixSpace (map (strFieldType d p) cs)
strConstr d p (ConstrCtx forall ctxs pos c cs) =
        "forall " ++ mixSpace (map (show . snd) forall) ++ "." ++ strContexts d p ctxs ++ (pStd p) c ++ " " ++ mixSpace (map (strFieldType d p) cs)

strFieldType d p (Nothing,typ) = strType d p typ
strFieldType d p (Just posidents,typ) = "{" ++ mixComma (map ((pStd p).snd) posidents) ++ " :: " ++ strType d p typ  ++ "}"

strType d p (TypeCons pos t ts) =
        pType d p pos t ts
strType d p (TypeApp t1 t2) =
        "(" ++ strType d p t1 ++ " " ++ strType d p t2 ++ ")"
strType d p (TypeVar pos id) =
        't':show id
strType d p (TypeStrict pos typ) = 
        "!" ++  strType d p typ

strSig d p (Sig ids t) = mixComma (map ((pStd p).snd) ids) ++ " :: " ++ strType d p t

strContexts d p []  = ""
strContexts d p [cx]  = ' ':strContext d p cx ++ " => "
strContexts d p cxs  = " (" ++ mixComma (map (strContext d p) cxs) ++ ") => "

strContext d p (Context pos c (_,v)) =
        (pStd p) c ++ " " ++  't':show v

strSimple d p (Simple pos id ids) = 
  pStd p id ++ " " ++ mixSpace (map ((pStd p).snd) ids)
strTypeSimple d p (Simple pos id ids) = 
  pStd p id ++ " " ++ mixSpace (map (('t':).show.snd) ids)

strInst d p t = strType d p t


strVarsType d p (DeclVarsType ids cxs t) =
  mixComma (map ((pStd p).snd) ids) ++ " :: " ++ strContexts d p cxs 
    ++ strType d p t

{- End Module StrSyntax -----------------------------------------------------}
