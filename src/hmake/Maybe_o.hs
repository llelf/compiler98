-----------------------------------------------------------------------------
-- |
-- Module      :  Maybe
-- Copyright   :  ...
-- 
-- Maintainer  :  Malcolm Wallace <Malcolm.Wallace@cs.york.ac.uk>
-- Stability   :  Stable
-- Portability :  All
--
-- Maybe implementation.  Unused. Not imported by any hmake modules
-- (presumably replaced by standard?)
-----------------------------------------------------------------------------

module Maybe(Maybe(..), thenM) where
-- Maybe together with Just and thenM forms a monad, but is more
-- by accident than by design.
data Maybe a = Nothing | Just a	deriving (Eq, Ord, Text)
Nothing `thenM` _ = Nothing
Just a  `thenM` f = f a
