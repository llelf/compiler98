module PackedString(unpackPS) where

import PreludeBuiltin(PackedString)

#if 0
foreign import ccall "unpackPSC" unpackPS_C :: E PackedString -> String
unpackPS p = unpackPS_C (E p)
#endif

_tprim_unpackPS primitive 2 :: Trace -> R PackedString -> R String
unpackPS s = _prim _tprim_unpackPS s
