-- Tracing version of types that are not definable in Haskell
-- or are used by functions that are not definable in Haskell
module TPreludeBuiltinTypes
  (Fun(Fun) -- reexported from Hat
  ,Bool(True,False),Char,Int,Integer,Float,Double,IOError 
   -- identical with Prelude
  ,List(Cons,List),IO
  ,String  -- here for convenience
  ,aTrue,aFalse
  ,module TPreludeBuiltinTypes
  ) where

import Hat as T
import Prelude hiding (IO,String)
import qualified Prelude

-- beside the types that cannot be defined within Haskell
-- also have to define here the types that are used by primitive functions:
-- Bool(,List),String,Tuple0,Tuple2


-- ----------------------------------------------------------------------------
-- types:

-- Bool constructors
aTrue = T.mkAtomCon tPrelude 0 3 "True"
aFalse = T.mkAtomCon tPrelude 0 3 "False"

type String = List Char


-- ----------------------------------------------------------------------------
-- type conversion functions:

-- function type is contravariant
toFun :: (Trace -> c -> R a) -> (Trace -> R b -> d) -> Trace 
      -> R (Fun a b) -> (c -> d)
toFun f g h (R (Fun x) _) = g h . x h . f h 

-- function type is contravariant
fromFun :: (Trace -> R a -> b) -> (Trace -> c -> R d) -> Trace
        -> (b -> c) -> R (Fun a d)
fromFun f g h x = R (Fun (const (g h . x . f h))) h 

toBool :: Trace -> R Bool -> Bool
toBool h (R b _) = b

fromBool :: Trace -> Bool -> R Bool
fromBool t b = con0 mkNoSourceRef t b (if b then aTrue else aFalse)


toList :: (Trace -> R a -> b) -> Trace -> R (List a) -> [b]
toList f h (R (Cons x xs) _) = f h x : toList f h xs
toList f h (R List _) = []

fromList :: (Trace -> a -> R b) -> Trace -> [a] -> R (List b)
fromList f h [] = con0 mkNoSourceRef h List aList
fromList f h (x:xs) = 
  con2 mkNoSourceRef h Cons aCons (ulazySat (f h x) h) 
    (ulazySat (fromList' xs) h)
  where
  fromList' [] = con0 mkNoSourceRef h List aList
  fromList' (x:xs) = 
    con2 mkNoSourceRef h Cons aCons (ulazySat (f h x) h) 
      (ulazySat (fromList' xs) h)

toString :: Trace -> R String -> Prelude.String
toString = toList toChar

fromString :: Trace -> Prelude.String -> R String
fromString = fromList fromChar

-- toPolyList :: R (List a) -> [R a]
-- toPolyList = toList id

-- fromPolyList :: Trace -> [R a] -> R (List a)
-- fromPolyList = fromList (\_ x -> x)


toChar :: Trace -> R Char -> Prelude.Char 
toChar h (R c _) = c

fromChar :: Trace -> Prelude.Char -> R Char
fromChar t c = conChar mkNoSourceRef t c

toInt :: Trace -> R Int -> Prelude.Int
toInt h (R i _) = i

fromInt :: Trace -> Prelude.Int -> R Int
fromInt t i = conInt mkNoSourceRef t i

toInteger :: Trace -> R Integer -> Integer
toInteger h (R i _) = i

fromInteger :: Trace -> Integer -> R Integer
fromInteger t i = conInteger mkNoSourceRef t i

toFloat :: Trace -> R Float -> Float
toFloat h (R f _) = f

fromFloat :: Trace -> Float -> R Float
fromFloat t f = conFloat mkNoSourceRef t f

toDouble :: Trace -> R Double -> Double
toDouble h (R d _) = d

fromDouble :: Trace -> Double -> R Double
fromDouble t f = conDouble mkNoSourceRef t f


toIOError :: Trace -> R IOError -> Prelude.IOError
toIOError h (R e _) = e

fromIOError :: Trace -> Prelude.IOError -> R IOError
fromIOError h e = R e h

