module Ptr
  ( Ptr		-- abstract, instance of: Eq, Ord, Enum, Show
  , nullPtr	-- :: Ptr a
  , plusPtr	-- :: Ptr a -> Int -> Ptr b
  ) where

import Addr

type Ptr a = Addr

nullPtr :: Ptr a
nullPtr  = nullAddr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr  = plusAddr

