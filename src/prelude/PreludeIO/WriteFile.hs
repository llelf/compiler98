module Prelude(writeFile) where

import IO
import TFilePath

writeFile :: FilePath -> String -> IO ()
writeFile fp str =
#if 1
  openFile fp WriteMode >>= \ handle ->
  mapM_ (hPutChar handle) str >>
  hClose handle
#else
  mapM_ (hPutChar stdout) str
#endif
