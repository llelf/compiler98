module Binary
  ( compareBin
  ) where

import GreenCard
import BinPtr
import BinHandle

    -- %-#include <stdio.h>
 


gr_compareBin primitive 6 :: Int -> Int -> ForeignObj -> Int -> ForeignObj -> Int -> Int

compareBin :: Int -> Int -> BinHandle -> BinPtr -> BinHandle -> BinPtr -> Ordering
compareBin sizex sizey (BH bhx) (BP x) (BH bhy) (BP y) =
  let retval = gr_compareBin sizex sizey bhx x bhy y
  in (toEnum retval)


