module BinArray
  ( {-type-} UnboxedArray(..)
  ) where

import GreenCard

newtype UnboxedArray = UBA ForeignObj
