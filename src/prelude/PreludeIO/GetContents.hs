module Prelude where

import IO

getContents :: IO [Char]
getContents = hGetContents stdin
