module PackedString where

import FFIBuiltin(PackedString)

unpackPS primitive 1 :: PackedString -> [Char]

