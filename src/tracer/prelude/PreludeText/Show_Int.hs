module Prelude(Show(..)) where

--import Text(showSigned,showInt)

instance Show Int where

 -- We don't want to use the generic showsPrec and showInt, as they use Integer
 --  showsPrec = showSigned showInt

  showsPrec p x =
    if x < 0 then showParen (p > 6)
      (showChar '-' . showPosInt (negate x))
    else 
      showPosInt x
   where
     showPosInt :: Int -> String -> String
     showPosInt n r =
	  let
	      nr :: Int -> Char      -- the magic constant 48 is fromEnum '0'
	      nr d = toEnum (48 + d) -- nhc13 can only remove toEnum if it's in a strict context
	      n' = n `quot` 10
	  in (if n' == 0 then id else showPosInt n') (nr (n `rem` 10) : r)

  showsType a = showString "Int"
