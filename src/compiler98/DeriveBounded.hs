module DeriveBounded(deriveBounded) where

import Syntax
import IntState
import IdKind
import NT
import State
import DeriveLib
import TokenId(TokenId,tminBound,tmaxBound,tBounded,tTrue)
import Extra(strPos)

deriveBounded tidFun cls typ tvs ctxs pos =
 getInfo typ >>>= \ typInfo -> 
 let expTrue = ExpCon pos (tidFun (tTrue,Con))
     constrs = constrsI typInfo
     tidTyp = tidI typInfo
     nt = NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]
 in
  getInfo (head constrs) >>>= \ minInfo ->
  getInfo (last constrs) >>>= \ maxInfo ->
  addInstMethod tBounded tidTyp tminBound nt (tidFun (tminBound,Method)) >>>= \ methodMinBound ->
  addInstMethod tBounded tidTyp tmaxBound nt (tidFun (tmaxBound,Method)) >>>= \ methodMaxBound ->
  unitS $
    DeclInstance pos (syntaxCtxs pos ctxs) cls [syntaxType pos typ tvs] $
      DeclsParse 
        [mkBound expTrue pos minInfo methodMinBound (tidFun (tminBound,Var))
	,mkBound expTrue pos maxInfo methodMaxBound (tidFun (tmaxBound,Var))
	]


mkBound expTrue pos constrInfo methodBound funBound =
 let con = ExpCon pos (uniqueI constrInfo)
 in case ntI constrInfo of
     NewType _ _ _ [nt] -> -- This constructor has no arguments
       DeclFun pos methodBound
	 [Fun [] (Unguarded (ExpCon pos (uniqueI constrInfo))) (DeclsParse [])]

     NewType _ _ _ (_:nts) ->  -- We only want a list with one element for each argument, the elements themselves are never used
      let args = (map fst . zip (repeat expBound)) nts
          expBound = ExpVar pos funBound
      in  
        DeclFun pos methodBound
	  [Fun []
	    (Unguarded 
              (ExpApplication pos (ExpCon pos (uniqueI constrInfo):args))) 
            (DeclsParse [])]

