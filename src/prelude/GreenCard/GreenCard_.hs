module GreenCard
  ( IO(..)
  , PackedString
  , toCString
  , fromCString
  , ForeignObj
  , StablePtr(..)
  ) where

import PreludeBuiltin (ForeignObj)
import DIO (IO(..))
import PackedString (PackedString)
import CString (toCString, fromCString)

data StablePtr a = StablePtr a


