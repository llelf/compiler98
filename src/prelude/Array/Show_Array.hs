module Array(Show(..)) where

import DArray
import Assocs
import Bounds
import Elems

instance  (Ix a, Show a, Show b) => Show (Array a b)  where
    showsPrec p a = showParen (p > 9) (
                    showString "array " .
                    shows (bounds a) . showChar ' ' .
                    shows (assocs a)                  )

    showsType a = showString " (Array " .  (showsType . fst . bounds) a
                  . showChar ' ' . (showsType . head . elems) a . showChar ')'
