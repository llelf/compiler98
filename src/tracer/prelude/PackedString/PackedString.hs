module PackedString(PackedString, packString{-, unpackPS-}) where

--data PackedString = PS String

import DPackedString
import PackString

{-
packString :: String -> PackedString
packString = PS

unpackPS :: PackedString -> String
unpackPS (PS s) = s

instance Show PackedString where
    showsPrec _ (PS s) = showString s

instance Eq PackedString where
    PS a == PS b = a == b

instance Ord PackedString where
    compare (PS a) (PS b) = compare a b
    PS a <  PS b = a <  b
    PS a <= PS b = a <= b
-}
