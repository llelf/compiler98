module Array where

import DArray
import Indices
import Index

elems                 :: (Ix a) => Array a b -> [b]
elems a               =  [a!i | i <- indices a]
