module Binary
  ( stdmem
  ) where

import GreenCard
import BinHandle ({-type-}BinHandle(..))


gr_stdmem primitive 0 :: ForeignObj

stdmem :: BinHandle
stdmem =
  let bh = gr_stdmem
  in (BH bh)



