module AssocTree
  ( AssocTree(..)
  , Tree
  , initAT	-- :: AssocTree k v
  , listAT	-- :: AssocTree k v -> [(k,v)]
  , reorderAT	-- :: (AssocTree k v -> (j,u) -> AssocTree k v)
		--			 -> AssocTree j u -> AssocTree k v
  , addAT	-- :: Ord k =>
		--	 AssocTree k v -> (v->v->v) -> k -> v -> AssocTree k v
  , lookupAT	-- :: Ord a =>
		--	 AssocTree k v -> k -> Maybe v
  , updateAT 	-- :: Ord k =>
		--	 AssocTree k v -> k -> (v->v) -> AssocTree k v
  ) where

import Tree234

type AssocTree k v = Tree (k,v)

initAT :: AssocTree k v
initAT = initTree

listAT :: AssocTree k v -> [(k,v)]
listAT t = treeMapList (:) t

reorderAT :: (AssocTree k v -> (j,u) -> AssocTree k v)
	 -> AssocTree j u -> AssocTree k v
reorderAT translate t = 
   foldl translate initAT (listAT t) 

cmp1 key (key',value) = compare key key'

cmp2 key _ (key',value) = compare key key'

addAT :: Ord k => AssocTree k v -> (v->v->v) -> k -> v -> AssocTree k v
addAT t comb key value = 
  treeAdd combine (cmp2 key) (key,value) t
 where
   combine (k1,v1) (k2,v2) = (k2,comb v1 v2)

lookupAT :: Ord k => AssocTree k v -> k -> Maybe v
lookupAT t key = 
   treeSearch Nothing ok (cmp1 key) t
  where
   ok (key,value) = Just value

updateAT :: Ord k => AssocTree k v -> k -> (v->v) -> AssocTree k v
updateAT t key upd = 
  treeUpdate update (cmp1 key) t
 where
   update (k1,v1) = (k1,upd v1)

