module PackedString where

import Prelude hiding(map)
import DPackedString
import PackString
import UnpackPS

map :: (Char -> Char) -> PackedString -> PackedString
map f = packString . Prelude.map f . unpackPS
