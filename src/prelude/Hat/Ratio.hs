module Ratio
	(Ratio
	,Rational
	,(%)
	,numerator
	,denominator
	,approxRational
	) where

infixl 7 :%
infixl 7 %
data Integral a => Ratio a = !a :% !a

(%)			:: (Integral a) => a -> a -> Ratio a
x % y			=  reduce (x * signum y) (abs y)

numerator :: (Integral a) => Ratio a -> a
numerator (x :% y) = x

denominator :: (Integral a) => Ratio a -> a
denominator (x:%y) = y

type Rational  = Ratio Integer

-- approxRational, applied to two real fractional numbers x and epsilon,
-- returns the simplest rational number within epsilon of x.  A rational
-- number n%d in reduced form is said to be simpler than another n'%d' if
-- abs n <= abs n' && d <= d'.  Any real interval contains a unique
-- simplest rational; here, for simplicity, we assume a closed rational
-- interval.  If such an interval includes at least one whole number, then
-- the simplest rational is the absolutely least whole number.  Otherwise,
-- the bounds are of the form q%1 + r%d and q%1 + r'%d', where abs r < d
-- and abs r' < d', and the simplest rational is q%1 + the reciprocal of
-- the simplest rational between d'%r' and d%r.

approxRational          :: (RealFrac a) => a -> a -> Rational
approxRational x eps    =  simplest (x-eps) (x+eps)
        where simplest x y | y < x      =  simplest y x
                           | x == y     =  xr
                           | x > 0      =  simplest' n d n' d'
                           | y < 0      =  negate (simplest' (negate n') d' (negate n) d)
                           | True  =  0 :% 1
                                        where xr@(n :% d) = toRational x
					      (n' :% d')  = toRational y

              simplest' n d n' d'       -- assumes 0 < n%d < n'%d'
                        | r == 0     =  q :% 1
                        | q /= q'    =  (q+1) :% 1
                        | True  =  (q*n''+d'') :% n''
                                     where (q,r)   = quotRem n d
                                           (q',r') = quotRem n' d'
					   (n'' :% d'') = simplest' d' r' d r


instance  (Integral a)  => Enum (Ratio a) where
        toEnum n        = fromInteger (toInteger n) :% 1
	fromEnum        = fromInteger . truncate        -- may overflow
	enumFrom x      = numericEnumFrom x
	enumFromThen x y = numericEnumFromThen x y

instance  (Integral a)	=> Eq (Ratio a)  where
    (x:%y) == (x':%y')	=  x == x' && y == y'
    (x:%y) /= (x':%y')	=  x /= x' || y /= y'

instance  (Integral a)	=> Fractional (Ratio a)  where
    (x:%y) / (x':%y')	=  (x*y') % (y*x')
    recip (x:%y)	=  y%x  --if x < 0 then negate y :% negate x else y:%x
    fromRational (x:%y) =  fromInteger x :% fromInteger y

instance  (Integral a)	=> Num (Ratio a)  where
    (x:%y) + (x':%y')	=  reduce (x*y' + x'*y) (y*y')
    (x:%y) - (x':%y')	=  reduce (x*y' - x'*y) (y*y')
    (x:%y) * (x':%y')	=  reduce (x * x') (y * y')
    negate (x:%y)	=  negate x :% y
    abs (x:%y)		=  abs x :% y
    signum (x:%y)	=  signum x :% 1
    fromInteger x	=  fromInteger x :% 1

instance  (Integral a)	=> Ord (Ratio a)  where
    (x:%y) <= (x':%y')	=  x * y' <= x' * y
    (x:%y) <  (x':%y')	=  x * y' <  x' * y

instance  (Integral a)	=> RealFrac (Ratio a)  where
    properFraction (x :% y) = (fromIntegral q, r :% y)
			      where (q,r) = quotRem x y

instance  (Integral a)	=> Real (Ratio a)  where
    toRational (x:%y)	=  toInteger x :% toInteger y


prec :: Int
prec = 7

instance  (Read a,Integral a) => Read (Ratio a)  where
    readsPrec p  =  readParen (p > prec)
			      (\r -> [(x % y,u) | (x,s)   <- reads r,
					          ("%",t) <- lex s,
						  (y,u)   <- reads t ])

instance  (Integral a) => Show (Ratio a)  where
    showsPrec p (x:%y)	=  showParen (p > prec)
    	    	    	       (shows x . showString " % " . shows y)

    showsType ~(x:%y) = showString "(Ratio " . showsType x . showChar ')'


reduce :: (Integral a) => a -> a -> Ratio a
reduce x y = if y == 0 
             then error "Ratio.%: zero denominator"
					 -- Haskell 1.2 used signum x :% 0
             else (x `quot` d) :% (y `quot` d)
	  where d = gcd x y

