module FileName where

fixFileName = id
--fixFileName f = if (take (7::Int) f == "Prelude") then
--                  case drop (7::Int) f of
--                     [] -> f
--                     xs -> xs
--                else f

fixFile unix p file suf =
--let file =  if take (7::Int) s == "Prelude"
--            then case drop (7::Int) s of [] -> s ; r  -> r
--            else s
--in
  if unix
  then (case (p,last p) of
          ("",_)  -> ""
          (_,'/') -> p
          (_,_)   -> p++"/") ++ file ++ '.':suf
  else p ++ suf ++ '.':file
