module Array where

import DArray
import Bounds

indices               :: (Ix a) => Array a b -> [a]
indices               =  range . bounds
