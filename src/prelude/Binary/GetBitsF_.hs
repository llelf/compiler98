module Binary
  ( getBitsF
  ) where

import GreenCard
import BinPtr    ({-type-}BinPtr(..))
import BinHandle ({-type-}BinHandle(..))


getBitsF :: BinHandle -> Int -> BinPtr -> (Int, BinPtr)
getBitsF bh w p = (getBitsFAux bh w p, toEnum (w + fromEnum p))

gr_getBitsFAux primitive 3 :: ForeignObj -> Int -> Int -> Int

getBitsFAux :: BinHandle -> Int -> BinPtr -> Int
getBitsFAux (BH bh) width (BP p) =
  let value = gr_getBitsFAux bh width p
  in value


