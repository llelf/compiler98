module Array(Read(..)) where

import DArray
import ArrayFun

instance  (Ix a, Read a, Read b) => Read (Array a b)  where
    readsPrec p = readParen (p > 9)
           (\r -> [(array b as, u) | ("array",s) <- lex r,
                                     (b,t)       <- reads s,
                                     (as,u)      <- reads t   ])
