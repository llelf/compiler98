module Binary
  ( equalsBin
  ) where

import GreenCard
import BinPtr
import BinHandle

 
gr_equalsBin primitive 5 :: Int -> ForeignObj -> Int -> ForeignObj -> Int -> Bool

equalsBin :: Int -> BinHandle -> BinPtr -> BinHandle -> BinPtr -> Bool
equalsBin size (BH bhx) (BP x) (BH bhy) (BP y) =
  let retval = gr_equalsBin size bhx x bhy y
  in retval


