#if !defined(TRACING)

module NHC.PackedString(PackedString,
	append,break,concat,cons,drop,dropWhile,
	filter,foldl,foldr,head,index,
	length,lines,map,nil,null,packString,
	reverse,span,splitAt,substr,tail,take,
	takeWhile,unlines,unpackPS,unwords,words) where

-- import qualified Prelude
import Prelude hiding (append,break,concat,cons,drop,dropWhile,
		filter,foldl,foldr,head,Ix(..),
		length,lines,Functor(..),map,nil,null,
		reverse,span,splitAt,substr,tail,take,
		takeWhile,unlines,unwords,words)

import Append
import Break
import Concat
import Cons
import Drop
import DropWhile
import Eq_PackedString
import Filter
import Foldl
import Foldr
import Head
import Index
import Length
import Lines
import Map
import Nil
import Null
import Ord_PackedString
import PackString
import Read_PackedString
import Reverse
import Show_PackedString
import Span
import SplitAt
import Substr
import DPackedString
import Tail
import Take
import TakeWhile
import Unlines
import UnpackPS
import Unwords
import Words

#else

module NHC.PackedString(PackedString, packString, unpackPS) where

import DPackedString
import PackString
import UnpackPS

import Eq_PackedString
import Ord_PackedString
import Show_PackedString

{-
data PackedString = PS String

packString :: String -> PackedString
packString = PS

unpackPS :: PackedString -> String
unpackPS (PS s) = s

instance Show PackedString where
    showsPrec _ (PS s) = showString s

instance Eq PackedString where
    PS a == PS b = a == b

instance Ord PackedString where
    compare (PS a) (PS b) = compare a b
    PS a <  PS b = a <  b
    PS a <= PS b = a <= b
-}

#endif
