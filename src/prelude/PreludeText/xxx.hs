module Prelude where

class  Read a  where
        readsPrec  :: Int -> ReadS a
        readList  :: ReadS [a]

