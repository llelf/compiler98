module Prelude where

showType 	    	:: (Show a) => a -> String
showType x 	    	=  showsType x ""
