module Prelude(Read(..)) where

--import Numeric(readSigned,readDec)

instance Read Int where
  readsPrec p r = (fromInteger i, t)
                          where
                            (i,t) = readsPrec p r
