module TraceId
  ( TraceId		-- abstract type
  , Fixity(L,R,Pre,Def,None)
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
  , tFixity             -- :: TraceId -> Fixity
  , tPriority           -- :: TraceId -> Int {0-9}
  , getUnqualified      -- :: TraceId -> String
  , hasInfo             -- :: TraceId -> Bool
  , tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
  , tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenGreater,tTokenMinus
  , tTokenTrue,tTokenFalse,tTokenEQ,tTokenCompare
  ,tTokenLocalFromEnum,tTokenInt,tTokenMinBound,tTokenMaxBound
  ,tTokenFromEnum,tTokenToEnum,tTokenEnumFrom,tTokenEnumFromThen
  ,tTokenEnumFromTo,tTokenEnumFromThenTo,tTokenError
  ,tTokenCompose,tTokenShowsPrec,tTokenShowParen,tTokenShowChar
  ,tTokenShowString,tTokenReadsPrec,tTokenReadParen,tTokenYield
  ,tTokenAlt,tTokenThenAp,tTokenThenLex,tTokenRange,tTokenIndex,tTokenInRange
  ,tTokenMap,tTokenLocalToEnum,tTokenTuple2,tTokenFun
  ,tTokenRangeSize,tTokenReturn,tTokenPlus,tTokenTimes -- :: TraceId
  ) where

import TokenId 
  (TokenId,mkQualifiedTokenId,extractV,dropM,t_Colon,t_List,t_gtgt,t_gtgteq
  ,tfail,t_andand,t_equalequal,t_greater,t_greaterequal,tminus,tTrue
  ,tFalse,tEQ,tcompare,visImport,tInt,tminBound,tmaxBound
  ,tfromEnum,ttoEnum,tenumFrom,tenumFromThen,tenumFromTo
  ,tenumFromThenTo,t_error,t_dot,tshowsPrec,tshowParen,tshowChar
  ,tshowString,treadsPrec,treadParen,trange,tindex,tinRange,t_Tuple,t_Arrow)
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

tFixity :: TraceId -> Fixity
tFixity (TI _ Nothing) = Def
tFixity (TI _ (Just info)) = fixity info

tPriority :: TraceId -> Int {- 0-9 -}
tPriority (TI _ Nothing) = 9
tPriority (TI _ (Just info)) = priority info

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

tTokenGreater :: TraceId
tTokenGreater = t_greater `plus` emptyAux

tTokenGreaterEqual :: TraceId
tTokenGreaterEqual = t_greaterequal `plus` emptyAux

tTokenMinus :: TraceId
tTokenMinus = tminus `plus` emptyAux

tTokenTrue :: TraceId
tTokenTrue = tTrue `plus` emptyAux{args=0}

tTokenFalse :: TraceId
tTokenFalse = tFalse `plus` emptyAux{args=0}

tTokenEQ :: TraceId
tTokenEQ = tEQ `plus` emptyAux{args=0}

tTokenCompare :: TraceId
tTokenCompare = tcompare `plus` emptyAux

tTokenLocalFromEnum :: TraceId
tTokenLocalFromEnum = visImport "localFromEnum" `plus` emptyAux{args=1}

tTokenInt :: TraceId
tTokenInt = tInt `plus` emptyAux

tTokenMinBound :: TraceId
tTokenMinBound = tminBound `plus` emptyAux

tTokenMaxBound :: TraceId
tTokenMaxBound = tmaxBound `plus` emptyAux

tTokenFromEnum :: TraceId
tTokenFromEnum = tfromEnum `plus` emptyAux

tTokenToEnum :: TraceId
tTokenToEnum = ttoEnum `plus` emptyAux

tTokenEnumFrom :: TraceId
tTokenEnumFrom = tenumFrom `plus` emptyAux

tTokenEnumFromTo :: TraceId
tTokenEnumFromTo = tenumFromTo `plus` emptyAux

tTokenEnumFromThen :: TraceId
tTokenEnumFromThen = tenumFromThen `plus` emptyAux

tTokenEnumFromThenTo :: TraceId
tTokenEnumFromThenTo = tenumFromThenTo `plus` emptyAux

tTokenError :: TraceId
tTokenError = t_error `plus` emptyAux{args=1}

tTokenCompose :: TraceId
tTokenCompose = t_dot `plus` emptyAux{args=2}

tTokenShowsPrec :: TraceId
tTokenShowsPrec = tshowsPrec `plus` emptyAux

tTokenShowParen :: TraceId
tTokenShowParen = tshowParen `plus` emptyAux{args=2}

tTokenShowString :: TraceId
tTokenShowString = tshowString `plus` emptyAux{args=0}

tTokenShowChar :: TraceId
tTokenShowChar = tshowChar `plus` emptyAux{args=0}

tTokenReadsPrec :: TraceId
tTokenReadsPrec = treadsPrec `plus` emptyAux

tTokenReadParen :: TraceId
tTokenReadParen = treadParen `plus` emptyAux{args=2}

tTokenYield :: TraceId
tTokenYield = mkQualifiedTokenId "PreludeBasic" "yield" `plus` emptyAux{args=2}

tTokenAlt :: TraceId
tTokenAlt = mkQualifiedTokenId "PreludeBasic" "alt" `plus` emptyAux{args=3}

tTokenThenLex :: TraceId
tTokenThenLex = mkQualifiedTokenId "PreludeBasic" "thenLex" `plus` emptyAux{args=2}

tTokenThenAp :: TraceId
tTokenThenAp = mkQualifiedTokenId "PreludeBasic" "thenAp" `plus` emptyAux{args=0}

tTokenRange :: TraceId
tTokenRange = trange `plus` emptyAux

tTokenIndex :: TraceId
tTokenIndex = tindex `plus` emptyAux

tTokenInRange :: TraceId
tTokenInRange = tinRange `plus` emptyAux

tTokenMap :: TraceId
tTokenMap = mkQualifiedTokenId "PreludeBasic" "map" `plus` emptyAux{args=2}

tTokenLocalToEnum :: TraceId
tTokenLocalToEnum = visImport "localToEnum" `plus` emptyAux{args=1}

tTokenTuple2 :: TraceId
tTokenTuple2 = t_Tuple 2 `plus` emptyAux{args=2}

tTokenFun :: TraceId
tTokenFun = t_Arrow `plus` emptyAux

tTokenRangeSize :: TraceId
tTokenRangeSize = mkQualifiedTokenId "Ix" "rangeSize" `plus` emptyAux

tTokenReturn :: TraceId
tTokenReturn = mkQualifiedTokenId "PreludeBasic" "return" `plus` emptyAux

tTokenPlus :: TraceId
tTokenPlus = mkQualifiedTokenId "PreludeBasic" "+" `plus` emptyAux

tTokenTimes :: TraceId
tTokenTimes = mkQualifiedTokenId "PreludeBasic" "*" `plus` emptyAux
