module Extra(module Extra, module HbcOnly, module Maybe) where

import HbcOnly
import Char
import List
import Maybe
#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#endif
#if defined (__GLASGOW_HASKELL__)
import IOExts (trace)
#endif

foldls f z [] = z
foldls f z (x:xs) =
  let z' = f z x
  in seq z' (foldl f z' xs)

strace msg c = if length msg == 0
	       then  c
	       else trace msg c

fstOf a b = a
sndOf a b = b

snub [] = []
snub (x:xs) = x:snub (filter (/=x) xs)

pair   x y   = (x,y)
triple x y z = (x,y,z)

#if !defined(__HASKELL98__)
isNothing Nothing = True
isNothing _       = False
#endif

dropJust (Just v) = v

isLeft (Left a) = True
isLeft _        = False

isRight (Right a) = True
isRight _        = False

dropLeft (Left a) = a

dropRight (Right a) = a

dropEither (Left x) = x
dropEither (Right x) = x

mapPair f g (x,y) = (f x,g y)
mapFst  f   (x,y) = (f x,  y)
mapSnd    g (x,y) = (  x,g y)

findLeft l = 
        f [] l
    where
        f a [] = Right (reverse a)
        f a (Left  e:r) = Left e
        f a (Right x:r) = f (x:a) r

eitherMap f [] = Right []
eitherMap f (x:xs) =
        case f x of
          Left err -> Left err
          Right x' -> case eitherMap f xs of
                        Left err -> Left err
                        Right xs' -> Right (x':xs') 


jRight :: Int -> [Char] -> [Char]
jRight n s = case length s of
                ns -> if ns > n then s
                      else space (n-ns) ++ s

jLeft :: Int -> [Char] -> [Char]
jLeft n s = case length s of
                ns -> if ns > n then s
                      else s ++ space (n-ns)

partitions f [] = []
partitions f (x:xs) =
    gB f (f x) [x] xs
  where
    gB f v a [] = [reverse a]
    gB f v a (x:xs) = if f x == v then gB f v (x:a) xs else reverse a : gB f (f x) [x] xs


----------

mix s [] = ""
mix s xs =  foldl1 (\x y-> x ++ s ++ y) xs

mixSpace = mix " "
mixComma = mix ","
mixLine  = mix "\n"

mixCommaAnd [x] = x
mixCommaAnd [x,y] = x ++ " and " ++ y
mixCommaAnd (x:xs) = x ++ ", " ++ mixCommaAnd xs

rep 0 c = []
rep n c = c:rep (n-1) c

-----------------

assoc :: Eq a => a -> [(a,b)] -> b
assoc a [] = error "assoc!"
assoc a ((k,v):kvs) = if a == k then v
                       else assoc a kvs

assocDef :: Eq a => [(a,b)] -> b -> a -> b
assocDef []          d a = d
assocDef ((k,v):kvs) d a = if a == k then v
                           else assocDef kvs d a

flatten xs = foldr (++) [] xs
-------------------

type Pos = Int

pos2Int p = p

toPos :: Int -> Int -> Pos
toPos l c =  l*10000 + c

noPos :: Pos
noPos = 0

fromPos :: Pos -> (Int,Int)
fromPos p =
 let l = p `div`   10000
     c = p - l*10000
 in (l,c)

strPos :: Pos -> String
strPos 0 = "nopos"
strPos p = case fromPos p of
	     (l,c) -> show l ++ ':':show c


--------------------


data SplitIntegral = SplitPos [Int]
                   | SplitZero
                   | SplitNeg [Int]

-- splitIntegral :: (Integral a) => a -> SplitIntegral
splitIntegral n =
  if n < 0
  then SplitNeg (split' (-n))
  else if n == 0 then SplitZero
  else SplitPos (split' n)
 where
  split' n = if n == 0 then []
             else fromInteger (toInteger (n `mod` 256)) : split' (n `div` 256)

--------------------
type Set a = [a]

emptySet = []

singletonSet a = [a]

listSet xs = (nub xs)

unionSet xs ys = unionSet' xs ys
               where unionSet' [] ys = ys
                     unionSet' (x:xs) ys | x `elem` ys = unionSet' xs ys
                                         | otherwise   = x:unionSet' xs ys

removeSet xs ys = filter (`notElem` ys) xs
---------------------
strChr' :: Char -> Char -> String 
strChr' del '\\' = "\\\\"
strChr' del '\n' = "\\n"
strChr' del '\t' = "\\t"
strChr' del c = if isPrint c 
                 then if c == del 
                      then "\\" ++ [c]
                      else [c]
                 else
                      "\\" ++ map (toEnum . (+(fromEnum '0'))) (ctoo (fromEnum c))
                    where ctoo c = [(c `div` 64),(c `div` 8) `mod` 8,c `mod` 8]
                          
strChr :: Char -> String 
strChr c = "'" ++ strChr' '\'' c ++ "'"

strStr :: String -> String 
strStr s = "\"" ++ concatMap (strChr' '"') s ++ "\""

-----------------------
showErr :: (Pos,String,[String]) -> String
showErr (pos,token,strs) =  strPos pos ++ (" Found " ++ token ++
                                             case nub strs of
					           [] -> " but no token can be accepted here."
  		                                   [x] -> " but expected a " ++ x
						   xs  -> " but expected one of " ++ mix " " xs)

------------------------
isNhcOp :: Char -> Bool
isNhcOp '~' = True; isNhcOp '=' = True; isNhcOp '*' = True
isNhcOp '%' = True; isNhcOp '/' = True; isNhcOp ':' = True
isNhcOp '+' = True; isNhcOp '@' = True; isNhcOp '.' = True
isNhcOp '>' = True; isNhcOp '&' = True; isNhcOp '$' = True
isNhcOp '|' = True; isNhcOp '-' = True
isNhcOp '!' = True; isNhcOp '<' = True
isNhcOp '^' = True; isNhcOp '#' = True; isNhcOp '?' = True
isNhcOp '\\' = True
isNhcOp _ = False

