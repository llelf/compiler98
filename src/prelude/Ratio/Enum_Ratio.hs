module Ratio(Enum(..)) where

import DRatio
import Ord_Ratio
import Real_Ratio
import RealFrac_Ratio

instance  (Integral a) => Enum (Ratio a) where
        toEnum n = fromInteger (toInteger n) :% 1
	fromEnum = fromInteger . truncate        -- may overflow
	enumFrom x = numericEnumFrom x
	enumFromThen x y = numericEnumFromThen x y

