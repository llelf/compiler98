module CaseLib where

import Extra(Pos(..),noPos,strPos,pair,sndOf,dropJust)
import Syntax
import SysDeps(PackedString,packString,unpackPS)
import SyntaxPos
import PosCode
import State
import IntState
import AssocTree
import IdKind
import TokenId
import NT
import Bind(identPat)
import Info

type ExpI = Exp Int

type Down = (ExpI -> ExpI
	    ,ExpI
	    ,ExpI
	    ,ExpI
	    ,ExpI
	    ,(ExpI,ExpI)
	    ,ExpI
	    ,(TokenId,IdKind) -> Int
	    ,PosExp
	    ,[Char]
	    , AssocTree Int Int
	    )
type Thread = (IntState, AssocTree TokenId Int)

type CaseFun a = Down -> Thread -> (a,Thread)

----- Low level stuff

addRatioCon :: ((TokenId,IdKind) -> Int) -> IntState -> (Int,IntState)
addRatioCon tidFun state =
 case uniqueIS state of
  (u,state) ->
   let ratio = tidFun (tRatio,TCon)
       tvar = NTvar 1
   in
    case lookupIS state ratio of
     Just info ->
      case constrsI info of
       [ratioCon] -> (ratioCon,state)
       [] -> (u,addIS u (InfoConstr  u tRatioCon IEnone (InfixL,7)
				    (NewType [1] [] [{- !!! Integral 1 -}] [tvar,tvar,NTcons ratio [tvar]])
			            [Nothing,Nothing] ratio)
			(updateIS state ratio (\_ -> updConstrsI info [u])))

caseTidFun down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,tidFun,stgUndef,strModid,translate) up = (tidFun,up)

caseList :: CaseFun (ExpI,ExpI)
caseList down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up = (expList,up)

caseEqInteger :: CaseFun ExpI
caseEqInteger down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
 (expEqInteger, up)

caseEqFloat :: CaseFun ExpI
caseEqFloat   down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
 (expEqFloat, up)

caseEqDouble :: CaseFun ExpI
caseEqDouble  down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
 (expEqDouble, up)

caseTrue :: CaseFun ExpI
caseTrue down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up = (expTrue,up)

caseRatioCon :: CaseFun PosExp
caseRatioCon down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,tidFun,stgUndef,strModid,translate) up@(state,t2s) =
 case addRatioCon tidFun state of
   (ratioCon,state) -> (PosCon noPos ratioCon,(state,t2s))

caseUndef :: CaseFun PosExp
caseUndef down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up = (stgUndef,up)

caseEqualNumEq :: CaseFun (ExpI -> ExpI)
caseEqualNumEq down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up = (expEqualNumEq,up)

caseIdent :: Pos -> Int -> CaseFun PosExp
caseIdent pos ident down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
  case lookupAT translate ident of
    Just v -> (PosVar pos v,up)
    Nothing -> (PosVar pos ident,up)

caseTranslate :: Int -> [Int] -> CaseFun Down
caseTranslate v us down@(expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,translate) up =
  ((expEqualNumEq,expEqInteger,expEqFloat,expEqDouble,expTrue,expList,expError,stgRatioCon,stgUndef,strModid,foldr ( \ u t -> addAT t sndOf u v ) translate us),up)

caseTuple :: Int -> CaseFun Int
caseTuple s down  up@(state,t2i) = 
  let tid = TupleId s
  in case lookupAT t2i tid of
    Just i -> (i,up)
    Nothing ->
      case uniqueIS state of
  	(u,state) ->
          let free = [1 .. s]
	      tvars = map NTvar free
              info = InfoName u tid s tid False --PHtprof
          in (u,(addIS u info state,addAT t2i sndOf tid u))

caseAdd :: Info -> Down -> Thread -> Thread
caseAdd info d up@(state,t2i) =
  (addIS (uniqueI info) info state,t2i)

caseError :: String -> Down -> Thread -> Thread
caseError error down (state,t2i) = (addError state error,t2i)

caseUnique :: CaseFun Int
caseUnique down (state,t2i) =
  case uniqueIS state of
    (i,state) -> (i,(state,t2i))

caseUniques :: [a] -> CaseFun [(a,Int)]
caseUniques l down (state,t2i) = 
 case uniqueISs state l of
   (il,state) -> (il,(state,t2i))

caseState :: CaseFun IntState
caseState down up@(state,t2i) = (state,up)

caseArity :: Int -> CaseFun Int
caseArity con down up@(state,t2i) =
  case lookupIS state con of
    Just info -> (arityVI info,up)


