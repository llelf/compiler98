module PackedString where

import Prelude hiding(unwords)
import DPackedString
import PackString
import UnpackPS

unwords :: [PackedString] -> PackedString
unwords = packString . Prelude.unwords . map unpackPS
