module Prelude where

import IO
import TFilePath

writeFile :: FilePath -> String -> IO ()
writeFile fp str =
  mapM_ (hPutChar stdout) str
{-
  openFile fp WriteMode >>= \ handle ->
  mapM_ (hPutChar handle) str >>
  hClose handle
-}
