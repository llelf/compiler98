module PPLib where

import HbcOnly(space)

infixl 6 `cl`
infixl 5 `nl`

pp s = \i -> s

nest ::  (Int -> [Char]) ->  (Int -> [Char])
nest s = \i -> "    " ++ s (i+4)

a `nl` b = \i -> a i ++ '\n' : space i ++ b i
a `cl` b = \i -> a i ++ b i

ppCommaSep [] = pp ""
ppCommaSep xs =  foldl1 (\x y-> x `cl` pp "," `cl` y) xs

ppSpaceSep [] = pp ""
ppSpaceSep xs =  foldl1 (\x y-> x `cl` pp " " `cl` y) xs

ppSemiSep [] = pp ""
ppSemiSep xs =  foldl1 (\x y-> x `cl` pp ";" `cl` y) xs

ppVertSemi []     = pp ""
ppVertSemi [x]    = x `cl` pp ";"
ppVertSemi (x:xs) = x `cl` pp ";" `nl` ppVertSemi xs

ppVert [] = pp ""
ppVert [x] = x
ppVert (x:xs) = x `nl` ppVert xs
