{-# OPTIONS -fglasgow-exts -#include "ghc_floats.h" -fvia-C #-}
module Floats
  ( floatToInt
  , doubleToInt0
  , doubleToInt1
  ) where

foreign import floatToInt   :: Float -> Int
foreign import doubleToInt0 :: Double -> Int
foreign import doubleToInt1 :: Double -> Int

