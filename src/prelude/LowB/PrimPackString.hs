module PackedString where

import FFIBuiltin(PackedString)

primPackString primitive 2 :: Int -> String -> PackedString

packString :: String -> PackedString
packString str = 
   seq (forceList str) (primPackString (length str) str)
  where
   forceList [] = ()
   forceList (x:xs) = seq x (forceList xs) 
