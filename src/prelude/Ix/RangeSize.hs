module Ix where

rangeSize               :: (Ix a) => (a,a) -> Int
rangeSize b@(l,u)
    | null (range b) =  0
    | otherwise      =  index b u + 1
