module MergeSort(mergeSort,mergeList
                ,mergeSortCmp,mergeListCmp
                ,group,groupSorted,unique
                ,cmpFst
                ) where

mergeSort ls = mergeList (split ls)
    where
        split [] = []
        split (x:xs) = split' x [] xs
            where
                split' x acc (y:xs) =
                    if x<=y then split' y (x:acc) xs
                    else reverse (x:acc) : split' y [] xs
                split' x acc [] = [reverse (x:acc)]

mergeList [] = []
mergeList [x] = x
mergeList xs = mergeList (merge xs)
    where
        merge      [] = []
        merge     [x] = [x]
        merge (x:b:c) = merge2 x b : merge c
            where
                merge2 [] b = b
                merge2 x [] = x
                merge2 xxs@(x:xs) bbs@(b:bs) =
                        if x <= b then x : merge2  xs bbs
                        else          b : merge2 xxs  bs


mergeSortCmp comb cmp ls =
        mergeListCmp comb cmp (split ls)
    where
        split = map (:[])

mergeListCmp comb cmp [] = []
mergeListCmp comb cmp [x] = x
mergeListCmp comb cmp xs = mergeListCmp comb cmp (merge xs)
    where
        merge      [] = []
        merge     [x] = [x]
        merge (x:b:c) = merge2 x b : merge c
            where
                merge2 [] b = b
                merge2 x [] = x
                merge2 xxs@(x:xs) bbs@(b:bs) =
                        cmp x b (x : merge2  xs bbs)
                                (merge2 (b `comb` x:xs) bs)
                                (b : merge2  xxs bs)

cmpFst (x,d1) (b,d2) c1 c2 c3 =
       if x <  b then c1
  else if x == b then c2
  else                c3

 
---------------

group l = groupSorted (mergeSort l)

groupSorted [] = []
groupSorted (x:xs) = groupSorted' x [] xs
        where
            groupSorted' x a [] = [x:a]
            groupSorted' x a (y:ys) =
                if x == y
                then groupSorted' x (y:a) ys
                else (x:a) : groupSorted' y [] ys

unique xs = map head (group xs)
