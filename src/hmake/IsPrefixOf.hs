-----------------------------------------------------------------------------
-- |
-- Module      :  IsPrefixOf
-- Copyright   :  Haskell Committee
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Small helper module with a single function, isPrefixOf
-----------------------------------------------------------------------------

module IsPrefixOf where

isPrefixOf               :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _           =  True
isPrefixOf (_:_)   []     =  False
isPrefixOf (x:xs) (y:ys)  =  x == y && isPrefixOf xs ys
