module Ratio where
--module Ratio(Read(..)) where

import DRatio
import Prec
import RatioCon

instance  (Read a,Integral a) => Read (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x % y,u) | (x,s)   <- reads r,
					          ("%",t) <- lex s,
						  (y,u)   <- reads t ])
