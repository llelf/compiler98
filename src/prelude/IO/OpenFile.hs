module IO where

import IO
import LowIO(primOpenFile)

openFile              :: FilePath -> IOMode -> IO Handle
openFile fp iomode     = primOpenFile fp iomode

