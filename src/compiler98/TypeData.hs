{- ---------------------------------------------------------------------------
Monad and dictionary type for type checking.
Also used in Remove1_3 for removing fields.
-}
module TypeData(TypeMonad,TypeDown(TypeDown),TypeState(TypeState)
               ,TypeDict(TypeDict)) where

import IntState(IntState)
import TokenId(TokenId)
import NT(NT)
import IdKind(IdKind)
import Extra(Pos(..))
import Syntax (Exp)
import AssocTree
import Id(Id)

data TypeDown =
      TypeDown
	[(Int,NT)]		-- env
	((TokenId,IdKind)->Int)	-- tidFun
	[Int]			-- defaults
	[(Int,Exp Int)]		-- ctxDict
	[(Int,[Exp Int])]	-- envDict

data TypeState =
      TypeState
	IntState		-- state
	(AssocTree Int NT)	-- phi
	[TypeDict]		-- ctxs
	[((Int,NT),Int)]	-- ctxs introduced due to pattern matching 
                                -- on existential NT is either NTvar or NTexist

type TypeMonad a = TypeDown -> TypeState -> (a,TypeState)

data TypeDict =
      TypeDict
	Int NT
	[(Int,Pos)]
	deriving (Show)

instance Eq TypeDict where
  (TypeDict c nt ip) == (TypeDict c' nt' ip') = c == c' && nt == nt'

instance Ord TypeDict where
  (TypeDict c nt ip) <= (TypeDict c' nt' ip') = 
    c < c' || (c == c' && nt <= nt')
  compare (TypeDict c nt ip) (TypeDict c' nt' ip') =
    case compare c c' of
      LT -> LT
      EQ -> compare nt nt'
      GT -> GT

{- TypeData -----------------------------------------------------------------}
