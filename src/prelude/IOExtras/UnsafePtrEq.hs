module IOExtras 
  ( unsafePtrEq
  ) where

import FFI

foreign import "unsafePtrEq" unsafePtrEq :: a -> a -> Bool

