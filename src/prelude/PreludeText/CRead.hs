module Prelude (Read(..)) where

import Lex
import ReadParen

class  Read a  where
#if !defined(TRACING)
    readsPrec  :: Int -> ReadS a
    readList   :: ReadS [a]
#else
    readsPrec  :: Int -> String -> [(a,String)]
    readList   :: String -> [([a],String)]
#endif

    readList = readParen False (\r -> [pr | ("[",s) <- lex r
                                      , pr      <- readl s])
      where readl s  = [([],t)   | ("]",t) <- lex s] ++
                       [(x:xs,u) | (x,t)  <- readsPrec 0 s
                                 , (xs,u) <- readl' t]
            readl' s = [([],t)   | ("]",t) <- lex s] ++
                       [(x:xs,v) | (",",t)  <- lex s
                                 , (x,u) <- readsPrec 0 t
                                 , (xs,v) <- readl' u]

