module FileName where

import Argv

fixFileName = id

fixFile opts p file suf =
  if isUnix opts
  then (case (p,last p) of
          ("",_)  -> ""
          (_,'/') -> p
          (_,_)   -> p++"/") ++ file ++ '.':suf
  else p ++ suf ++ '.':file
