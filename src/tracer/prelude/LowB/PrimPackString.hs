module PackedString where

import PreludeBuiltin(PackedString)

_tprim_packString primitive 2 :: Trace -> R String -> R PackedString

_prim_packString s = _prim _tprim_packString s

packString :: String -> PackedString
packString str = 
   seq (forceList str) (_prim_packString str)
  where
   forceList [] = ()
   forceList (x:xs) = seq x (forceList xs) 
