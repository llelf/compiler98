-- a toy prelude for Hat tracing
-- wraps a few essential Prelude functions and types to test portable tracing
module TPrelude where

import Hat as T
import FFI (Addr,ForeignObj,StablePtr
           ,Int8,Int16,Int32,Int64,Word8,Word16,Word32,Word64)
import PackedString(PackedString)
import MagicTypes (Vector) 
  -- magic C-type living in Haskell heap


tMain = T.mkModule "Prelude" "Prelude.hs"

-- ----------------------------------------------------------------------------
-- types:

data List a = Cons (R a) (R (List a)) | List  
  -- type constructor and empty list constructor need to have same name,
  -- because transformation doesn't distinguish the two

aCons = T.mkAtomCon tMain 0 21 ":"
aList = T.mkAtomCon tMain 0 3 "[]"


type Bool = Prelude.Bool
aTrue = T.mkAtomCon tMain 0 3 "True"
aFalse = T.mkAtomCon tMain 0 3 "False"

type Ordering = Prelude.Ordering
aLT = T.mkAtomCon tMain 0 3 "LT"
aEQ = T.mkAtomCon tMain 0 3 "EQ"
aGT = T.mkAtomCon tMain 0 3 "GT"

type Char = Prelude.Char

type String = List TPrelude.Char

type IO a = Prelude.IO (R a)


-- type constructors and data constructors need to have same name,
-- because transformation doesn't distinguish the two
type Tuple0 = ()  -- no new data constructor
aTuple0 = T.mkAtomCon tMain 0 3 "()"
data Tuple2 a b = Tuple2 (R a) (R b) -- not type Tuple2 a b = (R a,R b)
aTuple2 = T.mkAtomCon tMain 0 3 "(,)" 

-- ----------------------------------------------------------------------------
-- type conversion functions:

toChar :: R TPrelude.Char -> Prelude.Char 
toChar (R c _) = c

fromChar :: Trace -> Prelude.Char -> R TPrelude.Char
fromChar = flip mkR

toIO :: (R a -> b) -> R (TPrelude.IO a) -> Prelude.IO b 
toIO f (R io _) = fmap f io

fromIO :: (Trace -> a -> R b) -> Trace -> Prelude.IO a -> R (TPrelude.IO b)
fromIO f t io = mkR (fmap (f t) io) t

toTuple0 :: R TPrelude.Tuple0 -> ()
toTuple0 (R tp _) = tp

fromTuple0 :: Trace -> () -> R TPrelude.Tuple0
fromTuple0 = flip mkR

toList :: (R a -> b) -> R (List a) -> [b]
toList f (R (Cons x xs) _) = f x : toList f xs
toList f (R List _) = []

fromList :: (Trace -> a -> R b) -> Trace -> [a] -> R (List b)
fromList f t [] = con0 mkNoSourceRef t List aList
fromList f t (x:xs) = 
  con2 mkNoSourceRef t Cons aCons (lazySat (f t x) hidden) 
    (lazySat (fromList' xs) hidden)
  where
  fromList' [] = con0 mkNoSourceRef t List aList
  fromList' (x:xs) = 
    con2 mkNoSourceRef t Cons aCons (lazySat (f t x) hidden) 
      (lazySat (fromList' xs) hidden)
  hidden = mkTHidden t

toPolyList :: R (List a) -> [R a]
toPolyList = toList id

fromPolyList :: Trace -> [R a] -> R (List a)
fromPolyList = fromList (\_ x -> x)


-- ----------------------------------------------------------------------------
-- functions:

(-++) :: SR -> Trace 
    -> R (Trace -> (R (List a)) -> R (Trace -> (R (List a)) -> (R (List a))))
(-++) p t = T.fun2 (+++) (*++) p t

(+++) = mkAtomIdToplevel tMain noPos 21 "++"

(*++) :: Trace -> R (List a) -> R (List a) -> R (List a)
(*++) t xs ys = fromPolyList t (toPolyList xs ++ toPolyList ys) 


oreverse :: SR -> Trace 
         -> R (Trace -> R (List a) -> R (List a))
oreverse preverse treverse =
  fun1 a0v0reverse wreverse preverse treverse

a0v0reverse = mkAtomIdToplevel tMain noPos 3 "reverse"

wreverse :: Trace -> R (List a) -> R (List a)
wreverse t xs = fromPolyList t (reverse (toPolyList xs))


omap :: SR -> Trace 
     -> R (Trace -> R (Trace -> R a -> R b) 
     -> R (Trace -> R (List a) -> R (List b)))
omap pmap tmap =
  fun2 a0v0map wmap pmap tmap

a0v0map = mkAtomIdToplevel tMain noPos 3 "map"

wmap :: Trace -> R (Trace -> R a -> R b) -> R (List a) -> R (List b)
wmap t f xs = fromPolyList t (map (ap1 mkNoSourceRef hidden f) (toPolyList xs))
  where
  hidden = mkTHidden t


oputStr :: SR -> Trace 
        -> R (Trace -> (R TPrelude.String) -> R (TPrelude.IO TPrelude.Tuple0))

oputStr pputStr tputStr =
  T.fun1 a8v1putStr wputStr pputStr tputStr

a8v1putStr = T.mkAtomIdToplevel tMain noPos 3 "putStr"

wputStr :: Trace -> R TPrelude.String -> R (TPrelude.IO TPrelude.Tuple0)
wputStr t os = fromIO fromTuple0 t $ do
  let s = toList toChar os
  outputTrace t s
  putStr s
  return ()


unwrap :: R TPrelude.String -> Prelude.String
unwrap (T.R s _) = go s
  where
  go List = []
  go (Cons (R c _) (R rest _)) = c : go rest


-- error :: T.SR -> T.Trace -> T.R (Trace -> T.R String -> a)
-- should have special wrapper that terminates the program
-- writing message into trace


-- ----------------------------------------------------------------------------
-- type conversion of primitve types

instance NmCoerce Int where
    toNm t v sr = conInt sr t v
instance NmCoerce Prelude.Char where
    toNm t v sr = conChar sr t v
instance NmCoerce Integer where
    toNm t v sr = conInteger sr t v
instance NmCoerce Float where
    toNm t v sr = conFloat sr t v
instance NmCoerce Double where
    toNm t v sr = conDouble sr t v
instance NmCoerce Prelude.Bool where
    toNm t False sr = mkR False (mkTNm t aFalse sr) 
    toNm t True  sr = mkR True  (mkTNm t aTrue sr) 
instance NmCoerce Prelude.Ordering where
    toNm t LT sr = mkR LT (mkTNm t aLT sr)
    toNm t EQ sr = mkR EQ (mkTNm t aEQ sr)
    toNm t GT sr = mkR GT (mkTNm t aGT sr)
instance NmCoerce () where
    toNm t v sr = mkR v (mkTNm t mkNTDummy sr)
instance NmCoerce Addr where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce (StablePtr a) where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce ForeignObj where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce PackedString where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce (Vector a) where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
-- These types use dummies for now.  Ideally, we want to convert them to
-- either Int or Integer (depending on size), too lazy now
instance NmCoerce Int8
instance NmCoerce Int16
instance NmCoerce Int32
instance NmCoerce Int64
instance NmCoerce Word8
instance NmCoerce Word16
instance NmCoerce Word32
instance NmCoerce Word64

