-----------------------------------------------------------------------------
-- |
-- Module      :  SymTab
-- Copyright   :  Malcolm Wallace
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Symbol Table, in particular it's used for cpp from Imports.hs.
-----------------------------------------------------------------------------

module SymTab
  ( SymTab
  , emptyST
  , insertST
  , deleteST
  , lookupST
  , definedST
  , IndTree
  ) where

-- | Symbol Table
type SymTab v = IndTree [(String,v)]

emptyST   :: SymTab v
insertST  :: (String,v) -> SymTab v -> SymTab v
deleteST  :: String -> SymTab v -> SymTab v
lookupST  :: String -> SymTab v -> Maybe v
definedST :: String -> SymTab v -> Bool

emptyST           = itgen maxHash []
insertST (s,v) ss = itiap (hash s) ((s,v):)    ss id
deleteST  s    ss = itiap (hash s) (filter ((/=s).fst)) ss id
lookupST  s    ss = let vs = filter ((==s).fst) ((itind (hash s)) ss)
                    in if null vs then Nothing
                       else (Just . snd . head) vs
definedST s    ss = let vs = filter ((==s).fst) ((itind (hash s)) ss)
                    in (not . null) vs


----
-- | Index Trees (storing indexes at nodes).

data IndTree t = Leaf t | Fork Int (IndTree t) (IndTree t)
     deriving Show

itgen :: Int -> a -> IndTree a
itgen 1 x = Leaf x
itgen n x =
  let n' = n `div` 2
  in Fork n' (itgen n' x) (itgen (n-n') x)

itiap :: --Eval a =>
         Int -> (a->a) -> IndTree a -> (IndTree a -> b) -> b
itiap i f (Leaf x)       k = let fx = f x in {-seq fx-} (k (Leaf fx))
itiap i f (Fork n lt rt) k =
  if i<n then
       itiap i f lt $ \lt' -> k (Fork n lt' rt)
  else itiap (i-n) f rt $ \rt' -> k (Fork n lt rt')

itind :: Int -> IndTree a -> a  
itind i (Leaf x) = x
itind i (Fork n lt rt) = if i<n then itind i lt else itind (i-n) rt

itmap :: (a->b) -> IndTree a -> IndTree b
itmap f (Leaf x) = Leaf (f x)
itmap f (Fork n lt rt) = Fork n (itmap f lt) (itmap f rt)


----
-- Hash values

maxHash :: Int -- should be prime
maxHash = 101

maxHashChoices :: [Int]
maxHashChoices = [7,31,101,1021,10007,50009]

bestMaxHash :: Int -> Int
bestMaxHash n = select n maxHashChoices
  where select n [x] = x
        select n (x:y:xs)
          | n<y       = x
          | otherwise = select n (y:xs)

class Hashable a where
    hashWithMax :: Int -> a -> Int
    hash        :: a -> Int
    hash = hashWithMax maxHash

instance Enum a => Hashable [a] where
    hashWithMax m = h m 0
        where h m a [] = a
              h m a (c:cs) = h m ((17*(fromEnum c)+19*a)`rem`m) cs

----
