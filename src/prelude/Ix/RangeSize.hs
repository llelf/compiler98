module Ix where

rangeSize               :: (Ix a) => (a,a) -> Int
rangeSize b@(l,u)
    | l>u         =  0
    | otherwise   =  index b u + 1
