module PackedString where

import Prelude hiding(unlines)
import DPackedString
import PackString
import UnpackPS

unlines :: [PackedString] -> PackedString
unlines = packString . Prelude.unlines . map unpackPS
