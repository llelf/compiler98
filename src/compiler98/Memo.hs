module Memo(Memo(..),initM,elemM,lookupM,addM,useM,Tree) where

import Tree234

type Memo a = Tree a

initM = initTree

elemM tree value = 
  treeSearch False (\_ -> True) (compare value) tree

lookupM tree value =
  treeSearch Nothing Just (compare value) tree

addM tree value =
  treeAdd comb compare value tree
 where
  comb a b = b


useM tree value =
    case lookupM tree value of
	Nothing -> (addM tree value,value)
	Just v  -> (tree,v)
