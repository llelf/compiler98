module PackedString (primComparePS) where

import PackedString (PackedString)

foreign import primComparePSC :: PackedString -> PackedString -> Int

primComparePS :: PackedString -> PackedString -> Ordering
primComparePS ps1 ps2 = compare (primComparePSC ps1 ps2) (0::Int)
