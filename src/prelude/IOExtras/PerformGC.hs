module IOExtras
  ( performGC
  ) where

foreign import ccall "performGC" performGC :: IO ()
