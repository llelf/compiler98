module Prelude where

reads 	        :: (Read a) => ReadS a
reads		=  readsPrec 0
