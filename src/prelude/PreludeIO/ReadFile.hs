module Prelude where

import IO

readFile :: FilePath -> IO String
readFile fp =
   openFile fp ReadMode >>= \ handle ->
   hGetContents handle

