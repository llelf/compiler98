module TraceId
  ( TraceId		-- abstract type
			-- constructors:
  , mkLambdaBound      	-- :: TokenId -> TraceId
  , plus		-- :: TokenId -> AuxiliaryInfo -> TraceId
			-- selectors:
  , tokenId		-- :: TraceId -> TokenId
  , arity		-- :: TraceId -> Maybe Int
  , isLambdaBound	-- :: TraceId -> Bool
  , fixPriority		-- :: TraceId -> Int
  , tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
  ,tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenMinus -- :: TraceId
  ) where

import TokenId (TokenId,t_Colon,t_List,t_gtgt,t_gtgteq,tfail
               ,t_andand,t_equalequal,t_greaterequal,tminus)
import AuxFile (AuxiliaryInfo(..),Fixity(..),emptyAux)

{-
data TraceId = Keep	{ tokenId :: TokenId }
             | New	{ tokenId :: TokenId
			, arity   :: Maybe Int
			, isLambdaBound :: Bool
			}
-}

type TraceId = (TokenId, Maybe AuxiliaryInfo)

-- construction functions

mkLambdaBound :: TokenId -> TraceId
mkLambdaBound t = 
  (t, Just (Has{ args=(-1), fixity=Def, priority=9, letBound=False }))

plus :: TokenId -> AuxiliaryInfo -> TraceId
t `plus` aux = (t, Just aux)

-- selection functions

tokenId :: TraceId -> TokenId
tokenId (t,_) = t

arity :: TraceId -> Maybe Int
arity (_,Nothing)  = Nothing
arity (_,Just aux) = let a = args aux in if a==(-1) then Nothing else Just a

isLambdaBound :: TraceId -> Bool
isLambdaBound (_,Nothing)  = error "TraceId.isLambdaBound: no aux information"
isLambdaBound (_,Just aux) = not (letBound aux) || args aux == 0
 -- The reason for including a test for arity==0 here is that a CAF defn
 -- is technically a pattern-binding rather than a function binding.
 -- It makes the transformation easier if we treat a CAF as lambda-bound.

fixPriority :: TraceId -> Int
fixPriority (_,Nothing) = 3	-- default fixity and priority
fixPriority (_,Just info) = encode (fixity info) (priority info)
  where
    encode Def     _ = 3
    encode L       n = 2 + (n*4)
    encode R       n = 1 + (n*4)
    encode None    n = 0 + (n*4)
    encode (Pre _) n = 0 + (n*4)


-- TraceId versions of some hardcoded tokens 

tTokenCons :: TraceId
tTokenCons = t_Colon `plus` emptyAux{args=2}

tTokenNil :: TraceId
tTokenNil = t_List `plus` emptyAux{args=0}

tTokenGtGt :: TraceId
tTokenGtGt = t_gtgt `plus` emptyAux

tTokenGtGtEq :: TraceId
tTokenGtGtEq = t_gtgteq `plus` emptyAux

tTokenFail :: TraceId
tTokenFail = tfail `plus` emptyAux{args=1}

tTokenAndAnd :: TraceId
tTokenAndAnd = t_andand `plus` emptyAux{args=2}

tTokenEqualEqual :: TraceId
tTokenEqualEqual = t_equalequal `plus` emptyAux

tTokenGreaterEqual :: TraceId
tTokenGreaterEqual = t_greaterequal `plus` emptyAux

tTokenMinus :: TraceId
tTokenMinus = tminus `plus` emptyAux

