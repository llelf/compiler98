module AssocTree(AssocTree(..),initAT,reorderAT,addAT,lookupAT,Tree,updateAT,listAT) where

import Tree234

type AssocTree k v = Tree (k,v)

initAT = initTree

listAT t = treeMapList (:) t

reorderAT translate t = 
   foldl translate initAT (listAT t) 

cmp1 key (key',value) = compare key key'

cmp2 key _ (key',value) = compare key key'

addAT t comb key value = 
  treeAdd combine (cmp2 key) (key,value) t
 where
   combine (k1,v1) (k2,v2) = (k2,comb v1 v2)

lookupAT t key = 
   treeSearch Nothing ok (cmp1 key) t
  where
   ok (key,value) = Just value

updateAT t key upd = 
  treeUpdate update (cmp1 key) t
 where
   update (k1,v1) = (k1,upd v1)

