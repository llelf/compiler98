module Prelude(Read(..)) where

import Numeric(readSigned,readDec)

instance Read Int where
  readsPrec p = readSigned readDec
