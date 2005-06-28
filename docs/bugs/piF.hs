module Main where

import BigFloatF
import IO

out :: IO ()
out = putStr (show (pi :: BigFloat))

main :: IO ()
main = hSetBuffering stdout NoBuffering >> out

