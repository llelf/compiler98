module Derive (derive) where

import MergeSort(unique)
import TokenId
import Extra(pair,triple,noPos,snub,mixCommaAnd,strPos,Pos(..),isNothing,dropJust,mapSnd)
import NT
import Syntax
import IntState
import MergeSort(mergeSort)
import AssocTree
import Memo
import Rename(fixInstance)
import TypeCtx
import State
import Kind
import TypeData

--import DeriveEval		-- Removed in Haskell 98
import DeriveEq
import DeriveOrd
import DeriveShow
import DeriveRead
import DeriveEnum
import DeriveIx
import DeriveBounded
import DeriveBinary		-- MALCOLM


import DeriveLib

derive :: ((TokenId,Kind) -> Int) ->
	  IntState 		->
	  [(Int,[(Pos,Int)])] 	->
	  Decls Int 		-> Either [[Char]] (IntState,(Decls Int))
derive tidFun state derived (DeclsParse topdecls) =
       case doPreWork derived of
	 preWork ->
           case allThere preWork of
             errors@(_:_) -> Left errors
             [] -> case mapS (\ w -> deriveOne state tidFun w >>>= \decl ->  fixInstance (tidFun (tTrue,Con)) decl) (work preWork) () state of
		       (instdecls,state) -> Right (state,DeclsParse ( instdecls ++ topdecls))
 where 
   doPreWork d = (concatMap ( \  (con,pos_clss) -> map (pair con) pos_clss))  d

   allThere preWork = foldr (checkSC state (foldr ( \ (con,(pos,cls)) t -> addM t (cls,con)) initM preWork)) [] preWork

   work preWork = solve state (map (startDeriving tidFun state) preWork)

copyInst tcon realtcon (pos,cls) state =
  case lookupAT ((instancesI . dropJust . lookupIS state) cls) realtcon of
    Just (free,ctxs) -> addInstance cls tcon free ctxs () state
    Nothing -> addError state ("Deriving of " ++ strIS state cls ++ " at " ++ strPos pos ++ " for newtype " ++ strIS state tcon
			       ++ " is not possible as no instance exist for the isomorphic type" ++ strIS state realtcon)


checkSC state willDerive (con,(pos,cls)) errors =
  case lookupIS state cls of
    Just info ->
      case (map ( \ sc -> case lookupIS state sc of Just info -> (sc,instancesI info))  . superclassesI) info of
        scinsts ->
	  case filter ( \ (sc,insts) -> isNothing (lookupM willDerive (sc,con)) && isNothing (lookupAT insts con)) scinsts of
	    [] -> errors
	    scinsts -> ("Need " ++ mixCommaAnd (map ( \ (sc,_) -> strIS state sc ++ ' ':strIS state con ) scinsts)
		     ++ " to derive " ++ strIS state cls ++ ' ':strIS state con ++ " at " ++ strPos pos):errors

checkClass state cls tid =
   case lookupIS state cls of
     Nothing -> False
     Just info -> tidI info == tid

--deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tEval =
--  deriveEval tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tEq =
  deriveEq tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tOrd =
  deriveOrd tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tShow =
  deriveShow tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tRead =
  deriveRead tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tEnum =
  deriveEnum tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tIx =
  deriveIx tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tBounded =
  deriveBounded tidFun cls typ tvs ctxs pos
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) | checkClass state cls tBinary =
  deriveBinary tidFun cls typ tvs ctxs pos            --MALCOLM
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) =
  getInfo cls >>>= \ clsInfo ->
  deriveError ("Don't know how to derive " ++ show (tidI clsInfo) ++ " at " ++ strPos pos)


--- ============================   Derive needed contexts


    -- Eval doesn't care about constructors at all
    -- Bounded only cares about arguments to the the first and the last constructor
    -- All other classes need type of all constructor arguments

--startDeriving tidFun state (con,(pos,cls)) | checkClass state cls tEval =
--  case lookupIS state con of
--    Just conInfo -> 
--      let (NewType free exist ctx nt) = ntI conInfo
--      in (((cls,con),(free,[])),(pos,[]))
startDeriving tidFun state (con,(pos,cls)) | checkClass state cls tBounded =
  case lookupIS state con of
    Just conInfo -> 
      let (NewType free [] ctx nt) = ntI conInfo
          fstAndlst [] = []
          fstAndlst [x] = [x]
          fstAndlst xs = [head xs,last xs]
          types = (snub . concatMap ( ( \ (NewType free [] ctxs nts) -> init nts) . ( \ (Just info) -> ntI info) .  lookupIS state) . fstAndlst . constrsI) conInfo
      in (((cls,con),(free,map (pair cls) free ++ ctx)),(pos,types))

startDeriving tidFun state (con,(pos,cls)) =
  case lookupIS state con of
    Just conInfo -> 
      let (NewType free [] ctx nt) = ntI conInfo
          types = (snub . concatMap ( ( \ (NewType free [] ctxs nts) -> init nts) . ( \ (Just info) -> ntI info) .  lookupIS state) . constrsI) conInfo
      in (((cls,con),(free,map (pair cls) free ++ ctx)),(pos,types))


oneStep ::  IntState
	-> [ ((Int,Int),([Int],[(Int,Int)])) ]
        -> ( ((Int,Int),([Int],[(Int,Int)])) , (Pos,[NT]) )
        -> ( ((Int,Int),([Int],[(Int,Int)])) , (Pos,[NT]) )
oneStep state given ((cls_con@(cls,con),(free,ctxs)),pos_types@(pos,types)) =
 let (NewType _ _ data_ctxs _) = ntI (dropJust (lookupIS state con))
 in
  case (mergeSort . ctxsReduce state . (map (mapSnd NTvar) data_ctxs ++) . concatMap (ctxsSimplify state given) . map (\ nt -> TypeDict cls nt [(0,pos)])) types of
     ctxs -> ((cls_con,(free,map (mapSnd stripNT) ctxs)),pos_types)

solve intState work =
  if map (snd.fst) work' ==  map (snd.fst) work
  then work
  else solve intState work'
 where
  given' = map fst work
  work' = map (oneStep intState given') work



