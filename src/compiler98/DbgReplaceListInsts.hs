{- ---------------------------------------------------------------------------
This file is not used at all!!!
-}
module DbgReplaceListInsts(dbgReplaceListInsts) where

import Syntax
import TokenId
import DbgId

dbgReplaceListInsts (Module pos id exports impdecs fixdecs (DeclsParse decls)) = 
    Module pos id exports impdecs fixdecs (DeclsParse (map replaceListInsts decls))

replaceListInsts (DeclInstance pos ctxs id inst@(TypeCons p tid ts) decls) =
    DeclInstance pos ctxs id inst' decls
  where 
    inst' = if tid == t_List then TypeCons p tList ts else inst 
replaceListInsts decl = decl
    
