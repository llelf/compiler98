module NhcFloats
  ( floatToInt
  , doubleToInts
  ) where

import GreenCard

gr_floatToInt primitive 1 :: Float -> Int

floatToInt :: Float -> Int
floatToInt fl =
  gr_floatToInt fl



gr_doubleToInts primitive 1 :: Double -> (Int,Int)

doubleToInts :: Double -> (Int,Int)
doubleToInts db =
  gr_doubleToInts db


