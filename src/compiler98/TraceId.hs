module TraceId
  ( TraceId		-- abstract type
			-- constructors:
  , just		-- :: TokenId -> TraceId
  , plus		-- :: TokenId -> AuxiliaryInfo -> TraceId
			-- selectors:
  , tokenId		-- :: TraceId -> TokenId
  , arity		-- :: TraceId -> Maybe Int
  , isLambdaBound	-- :: TraceId -> Bool
  , fixPriority		-- :: TraceId -> Int
  ) where

import TokenId (TokenId)
import AuxFile (AuxiliaryInfo(..))

{-
data TraceId = Keep	{ tokenId :: TokenId }
             | New	{ tokenId :: TokenId
			, arity   :: Maybe Int
			, isLambdaBound :: Bool
			}
-}

type TraceId = (TokenId, Maybe AuxiliaryInfo)

-- construction functions

just :: TokenId -> TraceId
just t = (t, Nothing)

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
isLambdaBound (_,Just aux) = not (letBound aux)

fixPriority :: TraceId -> Int
fixPriority (_,Nothing) = 3	-- default fixity and priority
fixPriority (_,Just info) = encode (fixity info) (priority info)
  where
    encode Def     _ = 3
    encode L       n = 2 + (n*4)
    encode R       n = 1 + (n*4)
    encode None    n = 0 + (n*4)
    encode (Pre _) n = 0 + (n*4)
