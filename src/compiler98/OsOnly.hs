module OsOnly
  (isPrelude
  , fixImportNames, fixRootDir, fixDependFile, fixTypeFile, fixObjectFile
  ) where

isPrelude str = {-take (7::Int)-} str == "Prelude"

fixRootDir isUnix s =
 let rs = reverse s
 in
  if isUnix
  then
    case span (/='/') (stripUnix rs) of
      (rf,rr) -> (reverse rr,reverse rf)
  else
    case span (/='.') rs of
      (rf,rr) -> (reverse (stripRiscos rr),reverse rf)
 where
   stripUnix ('s':'h':'l':'.':r) = r
   stripUnix ('s':'h':    '.':r) = r
   stripUnix                  r  = r

   stripRiscos ('.':'s':'h':'l':rr) = rr
   stripRiscos ('.':'s':'h':    rr) = rr
   stripRiscos                  rr  = rr

fixImportNames isUnix suffix file rootdirs =
  map (\dir-> fixDir isUnix dir ++ (fixFile isUnix file suffix)) rootdirs


fixDir isUnix dir
  | isUnix    = case (dir,last dir) of
                    ("",_)  -> ""
                    (_,'/') -> dir
                    (_,_)   -> dir ++ "/"
  | otherwise = dir

fixTypeFile   isUnix rootdir s = rootdir ++ fixFile isUnix s "hit"
fixObjectFile isUnix rootdir s = rootdir ++ fixFile isUnix s "c"
fixDependFile isUnix rootdir s = rootdir ++ fixFile isUnix s "dep"

fixFile isUnix file suf =
{-
  let file =  if isPrelude s
              then case drop (7::Int) s of [] -> s ; r  -> r
              else s
  in
-}
    if isUnix
      then toUnixPath file ++ '.':suf
      else suf ++ '.':maxTen file

toUnixPath = map (\c-> if (c=='.') then '/' else c)

maxTen file = let tolong =  length file - 10
              in if tolong <= 0 then file
                 else take (10::Int) (strip tolong file)

strip 0 xs = xs
strip n [] = []
strip n (x:xs) = if isVowel x then strip (n-1) xs else x: strip n xs

isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'y' = True
isVowel '\xe1' = True   -- aa
isVowel '\xe0' = True   -- ae
isVowel '\xf0' = True   -- oe
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel 'Y' = True
isVowel '\xc5' = True   -- AA
isVowel '\xc4' = True   -- AE
isVowel '\xd4' = True   -- OE
isVowel _   = False



