module DeriveLib where

import Syntax
import IntState
import NT

syntaxCtxs pos ctxs = map ( \ (c,v) -> Context pos c [(pos,v)]) ctxs
syntaxType pos typ tvs = TypeCons pos typ (map (TypeVar pos) tvs)

noArgs constrInfo =
  case ntI constrInfo of
    (NewType _ _ _ [nt]) -> True
    _ -> False

deriveError str = \ down state ->
  (DeclIgnore str,addError state str)
