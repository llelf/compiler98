{-# OPTIONS -fglasgow-exts -#include "ghc_floats.h" -fvia-C #-}
module Floats
  ( floatToInt
  , doubleToInt0
  , doubleToInt1
  ) where

foreign import ccall floatToInt   :: Float -> Int
foreign import ccall doubleToInt0 :: Double -> Int
foreign import ccall doubleToInt1 :: Double -> Int
