module TraceId
  ( TraceId		-- abstract type
			-- constructors:
  , mkLambdaBound      	-- :: TokenId -> TraceId
  , plus		-- :: TokenId -> AuxiliaryInfo -> TraceId
                        -- modifiers
  , dropModule          -- :: TraceId -> TraceId
  , modLambdaBound      -- :: TraceId -> TraceId
  , modLetBound         -- :: TraceId -> TraceId
			-- selectors:
  , tokenId		-- :: TraceId -> TokenId
  , arity		-- :: TraceId -> Maybe Int
  , isLambdaBound	-- :: TraceId -> Bool
  , fixPriority		-- :: TraceId -> Int
  , getUnqualified      -- :: TraceId -> String
  , hasInfo             -- :: TraceId -> Bool
  , tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
  , tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenMinus
  , tTokenTrue,tTokenFalse -- :: TraceId
  ) where

import TokenId (TokenId,extractV,dropM,t_Colon,t_List,t_gtgt,t_gtgteq,tfail
               ,t_andand,t_equalequal,t_greaterequal,tminus,tTrue,tFalse)
import AuxTypes (AuxiliaryInfo(..),Fixity(..),emptyAux)
import Maybe (isJust)
import PackedString (unpackPS)


data TraceId = TI TokenId (Maybe AuxiliaryInfo)

instance Eq TraceId where
  TI t1 _ == TI t2 _ = t1 == t2


-- construction functions

mkLambdaBound :: TokenId -> TraceId
mkLambdaBound t = 
  TI t (Just (Has{ args=(-1), fixity=Def, priority=9, letBound=False }))

plus :: TokenId -> AuxiliaryInfo -> TraceId
t `plus` aux = TI t (Just aux)

-- modification functions

-- drop qualifier
dropModule :: TraceId -> TraceId
dropModule (TI tokenId aux) = TI (dropM tokenId) aux

modLambdaBound :: TraceId -> TraceId
modLambdaBound (TI token (Just aux)) = TI token (Just aux{letBound=False})
modLambdaBound (TI token Nothing) = mkLambdaBound token

modLetBound :: TraceId -> TraceId
modLetBound (TI token (Just aux)) = TI token (Just aux{letBound=True,args=0})
modLetBound (TI token Nothing) = error "modLetBound"

-- selection functions

tokenId :: TraceId -> TokenId
tokenId (TI t _) = t

arity :: TraceId -> Maybe Int
arity (TI _ Nothing)  = Nothing
arity (TI _ (Just aux)) = 
  let a = args aux in if a==(-1) then Nothing else Just a

isLambdaBound :: TraceId -> Bool
isLambdaBound (TI _ Nothing) = 
  error "TraceId.isLambdaBound: no aux information"
isLambdaBound (TI _ (Just aux)) = not (letBound aux)

fixPriority :: TraceId -> Int
fixPriority (TI _ Nothing) = 3	-- default fixity and priority
fixPriority (TI _ (Just info)) = encode (fixity info) (priority info)
  where
    encode Def     _ = 3
    encode L       n = 2 + (n*4)
    encode R       n = 1 + (n*4)
    encode None    n = 0 + (n*4)
    encode (Pre _) n = 0 + (n*4)

getUnqualified :: TraceId -> String
getUnqualified = reverse . unpackPS . extractV . tokenId

hasInfo :: TraceId -> Bool
hasInfo (TI _ aux) = isJust aux

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

tTokenTrue :: TraceId
tTokenTrue = tTrue `plus` emptyAux{args=0}

tTokenFalse :: TraceId
tTokenFalse = tFalse `plus` emptyAux{args=0}
