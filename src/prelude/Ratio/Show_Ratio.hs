module Ratio where
--module Ratio(Show(..)) where

import DRatio
import Prec

instance  (Integral a) => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)

    showsType ~(x:%y) = showString "(Ratio " . showsType x . showChar ')'

