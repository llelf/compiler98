module PackedString (primComparePS) where

import PackedString (PackedString)

foreign import primComparePSC :: PackedString -> PackedString -> Int

primComparePS :: PackedString -> PackedString -> Ordering
primComparePS ps1 ps2 = case primComparePSC ps1 ps2 of
				(-1) -> LT
				0    -> EQ
				1    -> GT
