module FileName (fixFileName, fixFile) where

import Argv

fixFileName = id

fixFile opts p file suf =
  if isUnix opts
  then (case (p,last p) of
          ("",_)  -> ""
          (_,'/') -> p
          (_,_)   -> p++"/") ++ toPath file ++ '.':suf
  else p ++ suf ++ '.':file

toPath = map (\c-> if (c=='.') then '/' else c)
