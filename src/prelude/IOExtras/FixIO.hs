module NHC.IOExtras where

import NHC.Internal (unsafePerformIO)

fixIO :: (a -> IO a) -> IO a
fixIO f = let x = unsafePerformIO (f x)
          in return x
