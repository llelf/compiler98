{- ---------------------------------------------------------------------------
The FSMonad and some helper functions for FixSyntax
-}
module FSLib(module FSLib, AssocTree(..), Tree, TokenId) where

import Syntax
import IdKind
import Info
import State
import AssocTree
import Extra(Pos(..),noPos,sndOf,dropJust)
import TokenId(mkQual3,mkQual2,TokenId(..),t_Colon,t_List,tRatio,tRatioCon
              ,t_id)
import IntState(IntState,lookupIS,addIS,uniqueIS,tidIS,updateIS)
import NT(NewType(..),NT)
import Id(Id)

type Inherited = ((Exp Id,Exp Id)  -- expList (nil, cons)
                  ,Exp Id          -- expId
                  ,(TokenId,IdKind) -> Id) --tidFun

type Threaded = (IntState,Tree (TokenId,Id))

type FSMonad a = State Inherited Threaded a Threaded


startfs :: (Decls Id -> FSMonad a)
        -> Decls Id 
        -> IntState
        -> ((TokenId,IdKind) -> Id) 
        -> (a,IntState,Tree (TokenId,Id))

startfs fs x state tidFun =
      let down = ((ExpCon noPos (tidFun (t_List,Con))	 
		  ,ExpCon noPos (tidFun (t_Colon,Con))	 
		  )
		 ,ExpVar noPos (tidFun (t_id,Var))  
		 ,tidFun
		 )

	  up =	(state
		    ,initAT)
      in
	case fs x down up of
	 (x,(state,t2i)) -> (x,state,t2i)


fsList :: FSMonad (Exp Id, Exp Id)
fsList down@(expList,expId,tidFun) up = (expList,up)

fsId :: FSMonad (Exp Id)
fsId down@(expList,expId,tidFun) up = (expId,up)

fsState :: FSMonad IntState
fsState down up@(state,t2i) = (state,up)

fsTidFun :: FSMonad ((TokenId,IdKind) -> Id)
fsTidFun down@(expList,expId,tidFun) up =
  (tidFun,up)


{- 
Returns True iff given data constructor is defined by data definition,
not newtype definition.
-}
fsRealData :: Id -> FSMonad Bool

fsRealData con down up@(state,t2i) =
  ((isRealData . dropJust . lookupIS state . belongstoI 
    . dropJust . lookupIS state) con,up)


fsExpAppl :: Pos -> [Exp Id] -> FSMonad (Exp Id)
 
fsExpAppl pos [x] = unitS x
fsExpAppl pos xs = unitS (ExpApplication pos xs)


fsClsTypSel :: Pos -> Id -> Id -> Id -> FSMonad (Exp Id)

fsClsTypSel pos cls typ sel down  up@(state,t2i) = 
  case lookupIS state cls of
   Just clsInfo ->
     case lookupIS state typ of
       Just typInfo ->
	 let tid = mkQual3  (tidI clsInfo) (tidI typInfo) (tidIS state sel)
	 in case lookupAT t2i tid of
	   Just i -> (ExpVar pos i,up)
	   Nothing ->
	     case uniqueIS state of
	       (u,state) ->
		 let   -- !!! Arity of selector doesn't look right !!!
		    arity = (arityIM . dropJust . lookupIS state) sel + (length . snd . dropJust . lookupAT (instancesI clsInfo)) typ
		    info = InfoName  u tid arity tid
--                    info = InfoMethod  u tid (InfixDef,9) NoType (Just arity) cls
		 in (ExpVar pos u,(addIS u info state,addAT t2i sndOf tid u))


fsExp2 :: Pos -> Id -> Id -> a 
       -> (IntState,Tree (TokenId,Int)) 
       -> (Exp Int,(IntState,Tree (TokenId,Int)))

fsExp2 pos cls i = 
  unitS (ExpVar pos) =>>> fsExp2i pos cls i


fsExp2i :: Pos -> Id -> Id -> a 
        -> (IntState,Tree (TokenId,Id)) 
        -> (Id,(IntState,Tree (TokenId,Id)))


fsExp2i pos cls i down  up@(state,t2i) = 
  case lookupIS state cls of
   Just clsInfo ->
     case lookupIS state i of
       Just clsdatInfo ->
	 let tid = mkQual2  (tidI clsInfo)  (tidI clsdatInfo)
     	 in case lookupAT t2i tid of           
	   Just i ->  (i,up)
	   Nothing ->
	     case uniqueIS state of
	       (u,state) ->
		 if isClass clsdatInfo
		 then    -- Exp2 is either superclass (Ord.Eq) taking one argument ...
		    (u,(addIS u (InfoMethod  u tid (InfixDef,9) NoType (Just 1) cls) state,addAT t2i sndOf tid u))
		 else -- ... or instance (Eq.Int) argument depends on type
		    let arity = (length . snd . dropJust . lookupAT (instancesI clsInfo)) i   -- snd instead of fst !!!
		    in seq arity (u,(addIS u (InfoVar  u tid (InfixDef,9) IEall NoType (Just arity)) state,addAT t2i sndOf tid u))

{- End Module FSLib ---------------------------------------------------------}
