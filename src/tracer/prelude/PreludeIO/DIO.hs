module Prelude where

data World = World

--newtype IO a = IO ( World -> Either IOError a)
data IO a = IO ( World -> Either IOError a)


