module Prelude where

unlines	   	:: [String] -> String
#if !defined(TRACING)
unlines 	= concatMap (++ "\n")
#else
unlines x 	= concatMap (++ "\n") x
#endif
