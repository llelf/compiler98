module PPLib where

import HbcOnly(space)

infixl 6 `cl`
infixl 5 `nl`

nestingDepth :: Int
nestingDepth = 2

type Doc = Int -> String


pp :: String -> Doc
pp s = \i -> s

nest ::  Doc -> Doc
nest s = \i -> space nestingDepth ++ s (i+nestingDepth)


{- combine two documents vertically -}
nl :: Doc -> Doc -> Doc
a `nl` b = \i -> a i ++ '\n' : space i ++ b i

{- combine two documents horizontally -}
cl :: Doc -> Doc -> Doc
a `cl` b = \i -> a i ++ b i


ppCommaSep :: [Doc] -> Doc

ppCommaSep [] = pp ""
ppCommaSep xs =  foldl1 (\x y-> x `cl` pp "," `cl` y) xs

ppSpaceSep [] = pp ""
ppSpaceSep xs =  foldl1 (\x y-> x `cl` pp " " `cl` y) xs

ppSemiSep [] = pp ""
ppSemiSep xs =  foldl1 (\x y-> x `cl` pp ";" `cl` y) xs

ppVertSemi []     = pp ""
ppVertSemi [x]    = x `cl` pp ";"
ppVertSemi (x:xs) = x `cl` pp ";" `nl` ppVertSemi xs


ppVert :: [Doc] -> Doc

ppVert [] = pp ""
ppVert [x] = x
ppVert (x:xs) = x `nl` ppVert xs
