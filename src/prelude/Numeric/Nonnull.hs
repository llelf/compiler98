module Numeric where

#if !defined(TRACING)
nonnull			:: (Char -> Bool) -> ReadS String
#else
nonnull			:: (Char -> Bool) -> String -> [(String,String)]
#endif
nonnull p s		=  [(cs,t) | (cs@(_:_),t) <- [span p s]]
