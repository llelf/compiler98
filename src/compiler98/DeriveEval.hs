module DeriveEval(deriveEval) where

import Syntax
import IntState
import IdKind
import NT
import State
import DeriveLib
import TokenId(TokenId)

deriveEval tidFun cls typ tvs ctxs pos =
    unitS $
      DeclInstance pos (syntaxCtxs pos ctxs) cls [syntaxType pos typ tvs] $
	DeclsParse []
