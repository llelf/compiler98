module IOExtras where

import UnsafePerformIO

fixIO :: (a -> IO a) -> IO a
fixIO f = let x = unsafePerformIO (f x)
          in return x
