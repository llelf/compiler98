module PackedString(unpackPS) where

import PreludeBuiltin(PackedString)

_tprim_unpackPS primitive 2 :: Trace -> R PackedString -> R String

unpackPS s = _prim _tprim_unpackPS s

