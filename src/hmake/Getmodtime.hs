module Getmodtime(isOld, show_When, When(..)) where
--import Either
import Time

data When = Never | At ClockTime  deriving (Eq, Ord)

instance Show When where
    showsPrec d Never = showString "never"
    showsPrec d (At i) = showString ("At "++show i)

show_When Never = "never"
show_When (At t) = show t

isOld Never _ = True
isOld _ Never = False
isOld (At t1) (At t2) = t1 < t2

