module TypeData where

import IntState(IntState)
import TokenId(TokenId)
import NT(NT)
import IdKind(IdKind)
import Extra(Pos(..))
import Syntax (Exp)
import AssocTree
import Tree234

data TypeDown =
      TypeDown
	[(Int,NT)]		-- env
	((TokenId,IdKind)->Int)	-- tidFun
	[Int]			-- defaults
	[(Int,Exp Int)]		-- ctxDict
	[(Int,[Exp Int])]	-- envDict
        Bool			-- Debug translating?

data TypeState =
      TypeState
	IntState		-- state
	(AssocTree Int NT)	-- phi
	[TypeDict]		-- ctxs
	[((Int,NT),Int)]	-- ctxs introduced due to pattern matching on existential NT is either NTvar or NTexist

data TypeDict =
      TypeDict
	Int NT
	[(Int,Pos)]
	deriving (Show)

instance Eq TypeDict where
  (TypeDict c nt ip) == (TypeDict c' nt' ip') = c == c' && nt == nt'

instance Ord TypeDict where
  (TypeDict c nt ip) <= (TypeDict c' nt' ip') = c < c' || (c == c' && nt <= nt')
  compare (TypeDict c nt ip) (TypeDict c' nt' ip') =
    case compare c c' of
      LT -> LT
      EQ -> compare nt nt'
      GT -> GT

