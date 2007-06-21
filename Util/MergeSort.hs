{-|
    Sort like functions, should be moved and removed
-}
module Util.MergeSort(group,unique) where

import List(sort)

group l = groupSorted (sort l)

groupSorted [] = []
groupSorted (x:xs) = groupSorted' x [] xs
        where
            groupSorted' x a [] = [x:a]
            groupSorted' x a (y:ys) =
                if x == y
                then groupSorted' x (y:a) ys
                else (x:a) : groupSorted' y [] ys

unique xs = map head (group xs)
