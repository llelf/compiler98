{- ---------------------------------------------------------------------------
Derive class instances that are demanded in derived clause of data type 
definitions.
-}
module Derive (derive) where

import List(sort)
import TokenId
import Extra(pair,snub,mixCommaAnd,strPos
            ,isNothing,dropJust,mapSnd)
import NT
import Syntax
import IntState
import AssocTree
import Memo
import Rename(fixInstance)
import TypeCtx
import State
import IdKind
import TypeData
import Id(Id)

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

derive :: ((TokenId,IdKind) -> Id) ->
	  IntState 		->
	  [(Id,[(Pos,Id)])] 	-> -- instances that have to be derived:
		                   -- class, (type constructor with position)
	  Decls Id 		-> Either [String] (IntState,(Decls Id))

derive tidFun state derived (DeclsParse topdecls) =
  let preWork = doPreWork derived in
  case allThere preWork of
      errors@(_:_) -> Left errors
      [] -> case mapS (\w-> deriveOne state tidFun w >>>= \decl ->  
                            fixInstance (tidFun (tTrue,Con)) decl)
                      (work preWork) () state of
              (instdecls,state) -> 
                Right (state, DeclsParse (instdecls++topdecls))
  where 
  -- just reorder
  doPreWork :: [(Id,[(Pos,Id)])] -> [(Id,(Pos,Id))]
  doPreWork d = concatMap (\(con,pos_clss)-> map (pair con) pos_clss)  d

  allThere :: [(Id,(Pos,Id))] -> [String]
  allThere preWork = 
    foldr (checkSC state (foldr (\(con,(pos,cls)) t -> addM t (cls,con))
                                initM preWork))
          [] preWork

  work :: [(Id,(Pos,Id))] -> [(((Int,Int),([Int],[(Int,Int)])),(Pos,[NT]))]
  work preWork = solve state (map (startDeriving tidFun state) preWork)

{-
copyInst :: Id -> Id -> (Pos,Id) -> IntState -> IntState
copyInst tcon realtcon (pos,cls) state =
  case lookupAT ((instancesI . dropJust . lookupIS state) cls) realtcon of
    Just (free,ctxs) -> addInstance cls tcon free ctxs () state
    Nothing -> 
      addError state 
        ("Deriving of " ++ strIS state cls ++ " at " ++ strPos pos ++ 
         " for newtype " ++ strIS state tcon ++ 
         " is not possible as no instance exist for the isomorphic type" ++ 
         strIS state realtcon)
-}

checkSC :: IntState -> Memo (Id,Id) -> (Id,(Pos,Id)) -> [String] -> [String]
checkSC state willDerive (con,(pos,cls)) errors =
  case lookupIS state cls of
    Just info ->
      case (map (\ sc -> 
                 case lookupIS state sc of 
                   Just info -> (sc,instancesI info))  
             . superclassesI) info of
        scinsts ->
	  case filter (\ (sc,insts) -> 
                       isNothing (lookupM willDerive (sc,con)) 
                         && isNothing (lookupAT insts con)) 
                 scinsts of
	    [] -> errors
	    scinsts -> ("Need " ++ 
                        mixCommaAnd 
                          (map (\ (sc,_) -> 
                                strIS state sc ++ ' ':strIS state con ) 
                          scinsts)
		        ++ " to derive " ++ strIS state cls ++ 
                        ' ':strIS state con ++ " at " ++ strPos pos) : errors


checkClass :: IntState -> Id -> TokenId -> Bool
checkClass state cls tid =
   case lookupIS state cls of
     Nothing -> False
     Just info -> tidI info == tid


deriveOne :: IntState 
          -> ((TokenId,IdKind) -> Int) 
          -> (((Int,Int),([Int],[(Int,Int)])),(Pos,a)) 
          -> b 
          -> IntState 
          -> (Decl Int,IntState)

{- gone in Haskell 98
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) 
  | checkClass state cls tEval = deriveEval tidFun cls typ tvs ctxs pos
-}
deriveOne state tidFun (((cls,typ),(tvs,ctxs)),(pos,types)) 
  | checkClass state cls tEq = deriveEq tidFun cls typ tvs ctxs pos
  | checkClass state cls tOrd = deriveOrd tidFun cls typ tvs ctxs pos
  | checkClass state cls tShow = deriveShow tidFun cls typ tvs ctxs pos
  | checkClass state cls tRead = deriveRead tidFun cls typ tvs ctxs pos
  | checkClass state cls tEnum = deriveEnum tidFun cls typ tvs ctxs pos
  | checkClass state cls tIx = deriveIx tidFun cls typ tvs ctxs pos
  | checkClass state cls tBounded = deriveBounded tidFun cls typ tvs ctxs pos
  | checkClass state cls tBinary = deriveBinary tidFun cls typ tvs ctxs pos  
  | otherwise =
      getInfo cls >>>= \ clsInfo ->
      deriveError ("Don't know how to derive " ++ show (tidI clsInfo) 
                   ++ " at " ++ strPos pos)


--- ============================   Derive needed contexts


    -- Eval doesn't care about constructors at all
    -- Bounded only cares about arguments to the the first and 
    --   the last constructor
    -- All other classes need type of all constructor arguments

--startDeriving tidFun state (con,(pos,cls)) | checkClass state cls tEval =
--  case lookupIS state con of
--    Just conInfo -> 
--      let (NewType free exist ctx nt) = ntI conInfo
--      in (((cls,con),(free,[])),(pos,[]))


startDeriving tidFun state (con,(pos,cls)) | checkClass state cls tBounded =
  case lookupIS state con of
    Just conInfo -> 
      let (NewType free [] ctx _) = ntI conInfo
          fstAndlst [] = []
          fstAndlst [x] = [x]
          fstAndlst xs = [head xs,last xs]
          types = (snub . concatMap ( ( \ (NewType free [] ctxs nts) -> init nts) . ( \ (Just info) -> ntI info) .  lookupIS state) . fstAndlst . constrsI) conInfo
      in (((cls,con),(free,map (pair cls) free ++ ctx)),(pos,types))

startDeriving tidFun state (con,(pos,cls)) =
  case lookupIS state con of
    Just conInfo -> 
      let (NewType free [] ctx _) = ntI conInfo
          types = (snub . concatMap ( ( \ (NewType free [] ctxs nts) -> init nts) . ( \ (Just info) -> ntI info) .  lookupIS state) . constrsI) conInfo
      in (((cls,con),(free,map (pair cls) free ++ ctx)),(pos,types))


oneStep ::  IntState
	-> [ ((Int,Int),([Int],[(Int,Int)])) ]
        -> ( ((Int,Int),([Int],[(Int,Int)])) , (Pos,[NT]) )
        -> ( ((Int,Int),([Int],[(Int,Int)])) , (Pos,[NT]) )
oneStep state given ((cls_con@(cls,con),(free,ctxs)),pos_types@(pos,types)) =
 let (NewType _ _ data_ctxs _) = ntI (dropJust (lookupIS state con))
 in
  case ( sort
       . ctxsReduce state
       . (map (mapSnd mkNTvar) data_ctxs ++)
       . concatMap (ctxsSimplify [] state given)
       . map (\ nt -> TypeDict cls nt [(0,pos)])) types of
     ctxs -> ((cls_con,(free,map (mapSnd stripNT) ctxs)),pos_types)

solve intState work =
  if map (snd.fst) work' ==  map (snd.fst) work
  then work
  else solve intState work'
 where
  given' = map fst work
  work' = map (oneStep intState given') work



