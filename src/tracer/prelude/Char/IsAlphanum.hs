module Char where

import IsAlpha
import IsDigit

isAlphanum	   :: Char -> Bool
isAlphanum c       = isAlpha c  ||  isDigit c 
