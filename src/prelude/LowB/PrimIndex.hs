module PackedString where

import DPackedString(PackedString)
import Prelude hiding(Ix(range,index,inRange))

index primitive 2 :: PackedString -> Int -> Char
