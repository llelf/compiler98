module Tree234(Tree,initTree,treeAdd,treeAddList,treeSearch,treeUpdate
	            ,treeMapList,treeFromList,treeRebuild,treeCombine,treeMap) where

data Tree a = Leaf
            | Leaf2 a
            | Leaf3 a a
            | Leaf4 a a a
            | Node2 a     (Tree a) (Tree a)
            | Node3 a a   (Tree a) (Tree a) (Tree a)
            | Node4 a a a (Tree a) (Tree a) (Tree a) (Tree a)
	    deriving (Show)

initTree = Leaf

treeAddFail = error "treeAddFail"

treeAdd comb cmp a t = treeAdd' comb cmp a id Node2 t

treeSearch fail cont p Leaf = fail
treeSearch fail cont p (Leaf2 a1) =
  case p a1 of
    LT -> fail
    EQ -> cont a1
    GT -> fail
treeSearch fail cont p (Leaf3 a1 a2) =
  case p a1 of
    LT -> fail
    EQ -> cont a1
    GT -> case p a2 of
	    LT -> fail
	    EQ -> cont a2
	    GT -> fail
treeSearch fail cont p (Leaf4 a1 a2 a3) =
  case p a2 of
    LT -> case p a1 of
	    LT -> fail
            EQ -> cont a1
	    GT -> fail
    EQ -> cont a2
    GT -> case p a3 of
            LT -> fail
            EQ -> cont a3
            GT -> fail
treeSearch fail cont p (Node2 a1       t1 t2) =
  case p a1 of
    LT -> treeSearch fail cont p t1
    EQ -> cont a1
    GT -> treeSearch fail cont p t2
treeSearch fail cont p (Node3 a1 a2    t1 t2 t3) =
  case p a1 of
    LT -> treeSearch fail cont p t1
    EQ -> cont a1
    GT -> case p a2 of
	    LT -> treeSearch fail cont p t2
            EQ -> cont a2
            GT -> treeSearch fail cont p t3
treeSearch fail cont p (Node4 a1 a2 a3 t1 t2 t3 t4) =
  case p a2 of
    LT -> case p a1 of
	    LT -> treeSearch fail cont p t1
	    EQ -> cont a1
	    GT -> treeSearch fail cont p t2
    EQ -> cont a2
    GT -> case p a3 of
	    LT -> treeSearch fail cont p t3
	    EQ -> cont a3
	    GT -> treeSearch fail cont p t4

treeUpdateFailed t = t -- error "treeUpdate couldn't find the node"

treeUpdate update p t = treeUpdate' update p t

treeUpdate' update p t@Leaf = (treeUpdateFailed t)
treeUpdate' update p t@(Leaf2 a1) =
  case p a1 of
    LT -> treeUpdateFailed t
    EQ -> Leaf2 (update a1)
    GT -> treeUpdateFailed t
treeUpdate' update p t@(Leaf3 a1 a2) =
  case p a1 of
    LT -> treeUpdateFailed t
    EQ -> Leaf3 (update a1) a2
    GT -> case p a2 of
	    LT -> treeUpdateFailed t
	    EQ -> Leaf3 a1 (update a2)
            GT -> treeUpdateFailed t
treeUpdate' update p t@(Leaf4 a1 a2 a3) =
  case p a2 of
    LT -> case p a1 of
            LT -> treeUpdateFailed t
            EQ -> Leaf4 (update a1) a2 a3
            GT -> treeUpdateFailed t
    EQ -> Leaf4 a1 (update a2) a3
    GT -> case p a3 of
            LT -> treeUpdateFailed t
            EQ -> Leaf4 a1 a2 (update a3)
            GT -> treeUpdateFailed t
treeUpdate' update p (Node2 a1       t1 t2) =
  case p a1 of
    LT -> Node2 a1 (treeUpdate' update p t1) t2
    EQ -> Node2 (update a1) t1 t2
    GT -> Node2 a1 t1 (treeUpdate' update p t2)
treeUpdate' update p (Node3 a1 a2    t1 t2 t3) =
  case p a1 of
    LT -> Node3 a1 a2          (treeUpdate' update p t1) t2 t3
    EQ -> Node3 (update a1) a2 t1 t2 t3
    GT -> case p a2 of
	    LT -> Node3 a1 a2          t1 (treeUpdate' update p t2) t3
            EQ -> Node3 a1 (update a2) t1 t2 t3
            GT -> Node3 a1 a2          t1 t2 (treeUpdate' update p t3)
treeUpdate' update p (Node4 a1 a2 a3 t1 t2 t3 t4) =
  case p a2 of
    LT -> case p a1 of
           LT -> Node4 a1 a2 a3 (treeUpdate' update p t1) t2 t3 t4
           EQ -> Node4 (update a1) a2 a3 t1 t2 t3 t4
           GT -> Node4 a1 a2 a3 t1 (treeUpdate' update p t2) t3 t4
    EQ -> Node4 a1 (update a2) a3 t1 t2 t3 t4
    GT -> case p a3 of
           LT -> Node4 a1 a2 a3 t1 t2 (treeUpdate' update p t3) t4
           EQ -> Node4 a1 a2 (update a3) t1 t2 t3 t4
           GT -> Node4 a1 a2 a3 t1 t2 t3 (treeUpdate' update p t4)

treeMapList f t = treeMapList' f t []
 where
  treeMapList' f Leaf r = r
  treeMapList' f (Leaf2 a1) r =
        f a1 r
  treeMapList' f (Leaf3 a1 a2) r =
        f a1 (f a2 r)
  treeMapList' f (Leaf4 a1 a2 a3) r =
        f a1 (f a2 (f a3 r))
  treeMapList' f (Node2 a1       t1 t2) r =
        treeMapList' f t1 (f a1 (treeMapList' f t2 r))
  treeMapList' f (Node3 a1 a2    t1 t2 t3) r =
        treeMapList' f t1 (f a1 (treeMapList' f t2
		       (f a2 (treeMapList' f t3 r))))
  treeMapList' f (Node4 a1 a2 a3 t1 t2 t3 t4) r =
        treeMapList' f t1 (f a1 (treeMapList' f t2
		       (f a2 (treeMapList' f t3 
		       (f a3 (treeMapList' f t4 r))))))

treeMap f Leaf = Leaf
treeMap f (Leaf2 a1) =
        Leaf2 (f a1)
treeMap f (Leaf3 a1 a2) =
        Leaf3 (f a1) (f a2)
treeMap f (Leaf4 a1 a2 a3) =
        Leaf4 (f a1) (f a2) (f a3)
treeMap f (Node2 a1       t1 t2) =
        Node2 (f a1) (treeMap f t1) (treeMap f t2)
treeMap f (Node3 a1 a2    t1 t2 t3) =
        Node3 (f a1) (f a2) (treeMap f t1) (treeMap f t2)
                             (treeMap f t3)
treeMap f (Node4 a1 a2 a3 t1 t2 t3 t4) =
        Node4 (f a1) (f a2) (f a3) (treeMap f t1) (treeMap f t2)
                             (treeMap f t3)
                             (treeMap f t4)

treeFromList comb cmp l  = treeAddList comb cmp l Leaf

treeAddList comb cmp []     t = t
treeAddList comb cmp (x:xs) t = treeAddList comb cmp xs (treeAdd comb cmp x t)   

treeRebuild comb cmp t = treeFromList comb cmp (treeMapList (:) t)

treeCombine comb cmp t1 t2 = treeAddList comb cmp (treeMapList (:) t2) t1

treeAdd' comb cmp a keep split (Leaf) =
    keep (
       Leaf2 a
    )
treeAdd' comb cmp a keep split (Leaf2 a1) =
    keep (
       case cmp a a1 of
	 LT -> Leaf3 a a1
         EQ -> Leaf2 (a `comb` a1)
         GT -> Leaf3 a1 a
    )
treeAdd' comb cmp a keep split (Leaf3 a1 a2) =
    keep (
       case cmp a a1 of
	 LT -> Leaf4 a a1 a2
         EQ -> Leaf3 (a `comb` a1) a2
         GT -> case cmp a a2 of
                 LT -> Leaf4 a1 a a2
                 EQ -> Leaf3 a1 (a `comb` a2)
                 GT -> Leaf4 a1 a2 a
    )
treeAdd' comb cmp a keep split (Leaf4 a1 a2 a3) =
  case cmp a a2 of
    LT -> case cmp a a1 of
            LT -> split a2 (Leaf3 a a1) (Leaf2 a3)
            EQ -> keep (Leaf4 (a `comb` a1) a2 a3)
            GT -> split a2 (Leaf3 a1 a) (Leaf2 a3)
    EQ -> keep (Leaf4 a1 (a `comb` a2) a3)
    GT -> case cmp a a3 of
            LT -> split a2 (Leaf2 a1)               (Leaf3 a a3)
            EQ -> keep (Leaf4 a1 a2 (a `comb` a3))
            GT -> split a2 (Leaf2 a1)               (Leaf3 a3 a)
treeAdd' comb cmp a keep split (Node2 a1       t1 t2) =
   keep (
     case cmp a a1 of
       LT -> treeAdd' comb cmp a  (\t1' -> Node2 a1       t1' t2)
                                (\a0' t0' t1' -> Node3 a0' a1   t0' t1' t2) t1
       EQ -> Node2 (a `comb` a1) t1 t2
       GT -> treeAdd' comb cmp a  (\t2' -> Node2 a1       t1  t2')
                                (\a2' t2' t3' -> Node3 a1  a2'  t1 t2' t3') t2
   )
treeAdd' comb cmp a keep split (Node3 a1 a2    t1 t2 t3) =
    keep (
      case cmp a a1 of
        LT -> treeAdd' comb cmp a  (\t1' -> Node3 a1 a2            t1' t2  t3)
                                (\a0' t0' t1' -> Node4 a0' a1 a2        t0' t1' t2 t3) t1
        EQ -> Node3 (a `comb` a1) a2 t1  t2  t3
        GT -> case cmp a a2 of
                LT -> treeAdd' comb cmp a (\t2' -> Node3 a1 a2            t1 t2' t3)
                                       (\a1_5' t1_5' t2' -> Node4 a1 a1_5' a2    t1 t1_5' t2' t3) t2
                EQ -> Node3 a1 (a `comb` a2) t1 t2 t3
                GT -> treeAdd' comb cmp a (\t3' -> Node3 a1 a2            t1 t2 t3')
                                       (\a3' t3' t4' -> Node4 a1 a2  a3'   t1 t2 t3' t4') t3
     )
treeAdd' comb cmp a keep split (Node4 a1 a2 a3 t1 t2 t3 t4) =
  case cmp a a2 of
    LT -> case cmp a a1 of
            LT -> split a2 (treeAdd' comb cmp a (\t1'->Node2 a1 t1' t2)
                                                 (\a0' t0' t1' -> Node3 a0' a1 t0' t1' t2) t1)
                                  (Node2 a3 t3 t4)
            EQ -> keep (Node4 (a `comb` a1) a2 a3 t1 t2 t3 t4)
            GT -> split a2 (treeAdd' comb cmp a (\t2' -> Node2 a1 t1 t2')
                                                 (\a2' t2' t3' -> Node3 a1 a2' t1 t2' t3') t2)
                                  (Node2 a3 t3 t4)
                
    EQ -> keep (Node4 a1 (a `comb` a2) a3 t1 t2 t3 t4)
    GT -> case cmp a a3 of
            LT -> split a2 (Node2 a1 t1 t2)
                                  (treeAdd' comb cmp a (\t3'->Node2 a3 t3' t4)
                                                 (\a2' t2' t3'-> Node3 a2' a3 t2' t3' t4) t3)
            EQ -> keep (Node4 a1 a2 (a `comb` a3) t1 t2 t3 t4)
            GT -> split a2 (Node2 a1 t1 t2)
                                  (treeAdd' comb cmp a (\t4'->Node2 a3 t3 t4')
                                                 (\a4' t4' t5' -> Node3 a3 a4' t3 t4' t5') t4)



