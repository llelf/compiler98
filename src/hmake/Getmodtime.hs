module Getmodtime(isOlder, show_When, When(..)) where

import Time

data When = Never | At ClockTime  deriving (Eq, Ord)

instance Show When where
    showsPrec d Never = showString "Never"
    showsPrec d (At i) = showString ("At "++show i)

show_When Never = "Never"
show_When (At t) = show t

isOlder Never _ = True
isOlder _ Never = False
isOlder (At t1) (At t2) = t1 < t2

