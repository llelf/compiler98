module Scc (sccDepend,Depend(..),isRec) where

import List
import MergeSort(mergeSort)
--import Extra(Set(..),emptySet,unionSet,singletonSet,listSet)
import Extra(sndOf)
import Memo
import AssocTree
import Tree234

type Set a = Memo a

emptySet = initM
listSet xs = foldr (flip addM) initM xs
addSet as a = if elemM as a then as else addM as a 
member a as = elemM as a
set2list as = treeMapList (:) as

{-
type Set a = [a]
emptySet = []
listSet xs = nub xs
addSet as a = if a `elem` as then as else a:as
member a as = a `elem` as
set2list as = as
-}

-- scc :: (Eq a)  => (a->[a]) -> (a->[a]) -> [a] -> [Set a]
scc :: (Ord a) => (a->[a]) -> (a->[a]) -> [a] -> [Set a]
scc ins outs = span . depth
    where depth = snd . depthSearch outs (emptySet,[])
          span  = snd . spanSearch  ins  (emptySet,[])


--depthSearch :: (Eq a)  => (a->[a]) -> (Set a,[a]) -> [a] -> (Set a,[a])
depthSearch :: (Ord a) => (a->[a]) -> (Set a,[a]) -> [a] -> (Set a,[a])
depthSearch = foldl . dsearch
    where dsearch rel (visited,seq) v | v `member` visited = (visited,seq)
                                      | otherwise        = (visited',v:seq')
                                     where (visited',seq') = depthSearch rel 
                                                                         (addSet visited v,seq)
                                                                         (rel v)


--spanSearch :: (Eq a)  => (a->[a]) -> (Set a,[Set a]) -> [a] -> (Set a,[Set a])
spanSearch :: (Ord a) => (a->[a]) -> (Set a,[Set a]) -> [a] -> (Set a,[Set a])
spanSearch = foldl . search
     where search rel (visited,setseq) v | v `member` visited = (visited,setseq)
                                         | otherwise        = (visited',listSet (v:seq):setseq)
                                     where (visited',seq) = depthSearch rel 
                                                                        (addSet visited v,[])
                                                                        (rel v)


sccAssoc at d =
  case lookupAT at d of
    Nothing -> [d]
    Just ds -> ds

mkout ds = sccAssoc (foldr ( \ (k,vs) at -> addAT at sndOf k vs ) initAT ds)

mkin ds = sccAssoc (foldr ( \ (k,vs) at ->
				let ks = [k]
				in foldr ( \ v at -> addAT at comb v ks) at vs) initAT ds)
  where comb [v] vs = v:vs

data Depend a = NoRec a
              | Rec   [a]

isRec (NoRec _) = False
isRec (Rec _) = True

instance (Show a) => Show (Depend a) where
  showsPrec d (NoRec x)    = ("NoRec "++).showsPrec d x
  showsPrec d (Rec  xs)    = ("Rec "++).showsPrec d xs

sccDepend :: (Ord a) => [(a, [a])] -> [Depend a]
sccDepend dep = fix' (map set2list (scc (mkin dep) out (map fst dep)))
        where
	  out = mkout dep

          fix' [] = []
          fix' ([x]:r) = (if x `elem` out x then Rec [x] else NoRec x) : fix' r
          fix' (xs:r)  = Rec xs : fix' r

