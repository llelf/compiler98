module IOExtras
  ( performGC
  ) where

foreign import "performGC" performGC :: IO ()
