-- bigfloat - An infinite precision calculator and function library
--            for the Haskell functional programming language.
-- Automatically translated from the original Miranda and copiously munged.
--
-- This version, BigFloatF, uses Haskell's Integer type for all internal
-- operations. The companion module, BigFloat, uses Ints for almost everything
-- with the occasional Integer where necessary.  This onm is truly infinite;
-- the other is limited to something of the order of 1.000.000.000 decimals.
--
-- Copyright 2003 Martin Guy, Sittingbourne, UK.
-- Email: <martinwguy@yahoo.it>	 Web: http://freaknet.org/martin
-- Free bignum server in Internet: telnet medialab.freaknet.org 31415
--
-- You can do as you please with this code, as long as you neither try to
-- prevent anyone else from doing as *they* please with it, nor tell lies!
--
-- This is an original work, except for edigits, modified from DAT's example
-- script, and the scientific algorithms ripped from the library of bc.
--
-- History:
-- Version 0, April 1986, Canterbury, UK: addition and multiplication
--	of positive numbers only.
-- Version 1, October 2001, Catania, Sicily, with full arithmetic, e, pi, 
--	sqrt, sin, cos, atan.
-- Version 2, 22 dec 2003, with logarithms and space and speed improvements.
-- Version 3, Oct 2004, Newcastle, turned into Haskell with addition of
--	bn_abs and derived scientific functions.
--
-- Numbers are represented as as infinite series of numbers which in a decimal
-- system would be referred to as decimal places.
-- The number represented by the bignum (-1, 4, [5, 2, 7]) is -0.527 * 10^4.
-- Support is not guaranteed for bases other than 10, but no base is
-- assumed by almost any of the functions.
--
-- bignum is a tuple containing sign, exponent and mantissa in that order.
--
-- in the code we have three principal sorts of functions:
-- bn_*	 treat bignums (sign, exp, mantissa)
-- ubn_* treat unsigned bignums (exp, mantissa)
-- m_* just treat mantissae [num]
-- and normally the bn_ functions use ubn functions to achieve their result,
-- which in turn call m_ functions to perform the detailed calculation.

module BigFloatF (BigFloat)
-- ( bn,ibn,bn_show,bn_0,bn_1,bn_2,bn_is_neg,bn_is_zero,bn_cmp,bn_eq,bn_ne,bn_gt,bn_lt,bn_ge,bn_le,bn_abs,bn_neg,bn_add,bn_sub,bn_twice,bn_half,bn_times,bn_mul,bn_raise,bn_quot,bn_div,bn_sqr,bn_sqrt,bn_rnd,bn_e,bn_pi,bn_pi_2,bn_pi_4,bn_2_pi,bn_ln,bn_exp,bn_pow,bn_sin,bn_cos,bn_atan,bn_sinh,bn_cosh,bn_tanh)
where

import Ratio

-- Private type synonym Bigfloat represents an infinite-precision number.
-- Exported type BigFloat is a new type that callers deal in.

-- Wrapper to make a first-class type from our type synonym
-- and be able to overload the standard operators and functions

newtype BigFloat = MakeBigFloat Bigfloat

toBigFloat :: Bigfloat -> BigFloat
toBigFloat x = MakeBigFloat x
fromBigFloat :: BigFloat -> Bigfloat
fromBigFloat (MakeBigFloat x) = x

instance Eq BigFloat where
	x==y	= bn_eq (fromBigFloat x) (fromBigFloat y)
	x/=y	= bn_ne (fromBigFloat x) (fromBigFloat y)

instance Ord BigFloat where
	x < y	= bn_lt (fromBigFloat x) (fromBigFloat y)
	x > y	= bn_gt (fromBigFloat x) (fromBigFloat y)
	x <= y	= bn_le (fromBigFloat x) (fromBigFloat y)
	x >= y	= bn_ge (fromBigFloat x) (fromBigFloat y)

instance Show BigFloat where
	show	= bn_show . fromBigFloat

instance Num BigFloat where
	x + y	= toBigFloat (bn_add (fromBigFloat x) (fromBigFloat y))
	x - y	= toBigFloat (bn_sub (fromBigFloat x) (fromBigFloat y))
	x * y	= toBigFloat (bn_mul (fromBigFloat x) (fromBigFloat y))
	negate	= toBigFloat . bn_neg . fromBigFloat
	abs	= toBigFloat . bn_abs . fromBigFloat
	fromInteger = toBigFloat . ibn

instance Fractional BigFloat where
	x / y	= toBigFloat (bn_div (fromBigFloat x) (fromBigFloat y))
	recip	= toBigFloat . (bn_div bn_1) . fromBigFloat
	-- fromRational x:%y = toBigFloat (bn_div (ibn x) (ibn y))

instance Floating BigFloat where
	pi	= toBigFloat bn_pi
	exp	= toBigFloat . bn_exp . fromBigFloat
	log	= toBigFloat . bn_ln . fromBigFloat
	sqrt	= toBigFloat . bn_sqrt . fromBigFloat
	x ** y	= toBigFloat ( (fromBigFloat x) `bn_pow` (fromBigFloat y) )
	sin	= toBigFloat . bn_sin . fromBigFloat
	cos	= toBigFloat . bn_cos . fromBigFloat
	tan	= toBigFloat . bn_tan . fromBigFloat
	atan	= toBigFloat . bn_atan . fromBigFloat
	sinh	= toBigFloat . bn_sinh . fromBigFloat
	cosh	= toBigFloat . bn_cosh . fromBigFloat
	tanh	= toBigFloat . bn_tanh . fromBigFloat
	asinh	= toBigFloat . bn_asinh . fromBigFloat
	acosh	= toBigFloat . bn_acosh . fromBigFloat
	atanh	= toBigFloat . bn_atanh . fromBigFloat

---- Here beginneth the internal stuff ----

-- Standard input converted to Haskell by martin on Wed Jul 28 02:20:42 CEST 2004
-------------------- mira2hs functions --------------------

rep :: Integer -> b -> [b]
rep n x = takeger n (repeat x)

-- Integer version of useless Int "take"
takeger :: Integer -> [b] -> [b]
takeger n [] = []
takeger n (a:x)
	| n <= 0 = []
	| otherwise = a : takeger (n-1) x

-------------------- mira2hs functions end --------------------

-- The number base we are working in.
-- It's not guaranteed that anything other than 10 will work - but it might.
-- ubn_text (text to bignum conversion) only works up to base 10.
base :: Integer
base = 10

-- Constants derived from base.

-- base_1 = base - 1
base_1 :: Integer
base_1 = 9
-- base_2 = base ^2
base_2 :: Integer
base_2 = 100
-- base_div_2 = base `div` 2
base_div_2 :: Integer
base_div_2 = 5

-- The number of digits that bn_show prints.  if scale = 0, infinite.
scale :: Integer
scale = 0
-- The truncation function used in the printing functions below.
scaletrunc x
	| (scale > 0) = takeger scale x
	| otherwise = x

-- signed <-> unsigned Bigfloat converters
bn2ubn :: Bigfloat -> UBigfloat
ubn2bn :: UBigfloat -> Bigfloat

-- normaliser strips leading zeroes from mantissa, adjusting
-- exponent accordingly.  It also corrects the "-0" case.
bn_normalise :: Bigfloat -> Bigfloat

-- bn_trim strips trailing zeroes from the mastissa.
bn_trim :: Bigfloat -> Bigfloat


-- Exported functions dealing directly with Bigfloats:

-- bn_make and bn_unmake construct and destruct a Bigfloat.
bn_make :: (Integer,Integer,[Integer]) -> Bigfloat
bn_unmake :: Bigfloat -> (Integer,Integer,[Integer])

-- signof returns the exponent of a Bigfloat
sign_of :: Bigfloat -> Sign

-- exponent_of returns the exponent of a Bigfloat
exponent_of :: Bigfloat -> Exponent

-- mantissa_of returns the mantissa of a Bigfloat
mantissa_of :: Bigfloat -> Mantissa

-- bn converts a decimal string representation of a number
-- into a Bigfloat.  It's the easiest way of entering constants into
-- an equation, and accepts all the formats that "bn_show" outputs.
bn :: [Char] -> Bigfloat

-- bn_show converts a Bigfloat into a printable form: an initial
-- '-' if it's negative, followed by one of three basic formats:
-- - a string of decimal digits if the Bigfloat has no fractional part
--   (ie represents an integer)
-- - a string of digits + a period + a string of digits if the Bigfloat
--   is >= 1 and is not integral
-- - "0." ++ a string of digits if the Bigfloat is < 1.
bn_show :: Bigfloat -> [Char]

-- bn_is_neg tells you whether a Bigfloat is a negative number
-- (0 is not negative!)
bn_is_neg :: Bigfloat -> Bool
bn_is_zero :: Bigfloat -> Bool

-- bn_cmp compares two Bigfloats and returns:
-- 0 if the two numbers have the same value,
-- 1 if the first number is greater than the second
-- -1 if the second number is greater than the first.
bn_cmp :: Bigfloat -> Bigfloat -> Ordering

-- The six usual comparison functions:
-- type 	-- = /= > < >= <=
bn_eq :: Bigfloat -> Bigfloat -> Bool
bn_ne :: Bigfloat -> Bigfloat -> Bool
bn_gt :: Bigfloat -> Bigfloat -> Bool
bn_lt :: Bigfloat -> Bigfloat -> Bool
bn_ge :: Bigfloat -> Bigfloat -> Bool
bn_le :: Bigfloat -> Bigfloat -> Bool

-- bn_rnd takes a positive integer seed parameter and returns a
-- pseudo-random Bigfloat in the range 0 <= bn_rnd n < 1
bn_rnd :: Integer -> Bigfloat

-- bn_abs returns absolute (positive) value of a bigfloat
bn_abs :: Bigfloat -> Bigfloat

-- bn_neg negates a Bigfloat
bn_neg :: Bigfloat -> Bigfloat

-- bn_add adds two Bigfloats together, returning their sum.
bn_add :: Bigfloat -> Bigfloat -> Bigfloat

-- bn_sub subtracts its second Bigfloat parameter from its first,
-- returning the difference.  Since negative numbers are not
-- representable (yet), a negative result is a fatal error.
bn_sub :: Bigfloat -> Bigfloat -> Bigfloat

-- bn_twice multiplies by two.
bn_twice :: Bigfloat -> Bigfloat

-- bn_half divides by two.
bn_half :: Bigfloat -> Bigfloat

-- bn_times multiplies an Integer by a Bigfloat.
-- Done by repeated addition.
bn_times :: Integer -> Bigfloat -> Bigfloat

-- bn_mul multiplies two Bigfloats.
bn_mul :: Bigfloat -> Bigfloat -> Bigfloat

-- bn_raise raises a Bigfloat to an Integer power
bn_raise :: Bigfloat -> Integer -> Bigfloat

-- bn_quot divides a Bigfloat by an Integer
bn_quot :: Bigfloat -> Integer -> Bigfloat

-- bn_div performs division on two Bigfloats
bn_div :: Bigfloat -> Bigfloat -> Bigfloat

-- bn_sqr returns the square of its argument.
bn_sqr :: Bigfloat -> Bigfloat

-- bn_sqrt returns the square root of its argument.
bn_sqrt :: Bigfloat -> Bigfloat

-- Logarithms and trigonometric functions
bn_ln :: Bigfloat -> Bigfloat
bn_exp :: Bigfloat -> Bigfloat
bn_pow :: Bigfloat -> Bigfloat -> Bigfloat
bn_sin :: Bigfloat -> Bigfloat
bn_cos :: Bigfloat -> Bigfloat
bn_atan :: Bigfloat -> Bigfloat


-- Here beginneth...

type Bigfloat	 = ( Sign, Exponent, Mantissa )
type UBigfloat	 = ( Exponent, Mantissa )
type Sign = Integer
type Exponent = Integer
type Mantissa = [Integer]

-- Note: herein, when we use type "mantissa", we mean [num] in which
-- every element x is 0 <= x < base.
-- In all cases dealing with partial results which may contain values
-- outside the permitted range for our number base, we say [num].

-- Some simple constants
bn_0 = bn_make (1, 0, [])
bn_1 = bn_make (1, 1, [1])
bn_2 = bn_make (1, 1, [2])

-- unsigned Bigfloat constants
ubn_0 :: UBigfloat
ubn_0 = (0, [])

--
-- ---------- simple conversion functions ----------
--

-- make a Bigfloat from its parts and vice versa (type-fooling glue)
bn_make (s,e,m) = (s,e,m)
bn_unmake (s,e,m) = (s,e,m)

-- functions to select parts of a Bigfloat
sign_of (s,e,m) = s
exponent_of (s,e,m) = e
mantissa_of (s,e,m) = m

-- functions to select parts of a UBigfloat
uexponent_of (e,m) = e
umantissa_of (e,m) = m

-- Convert Bigfloat to unsigned Bigfloat and vice versa
bn2ubn (1,e,m) = (e,m)
bn2ubn other   = error "Internal error: bn2ubn of negative number"

ubn2bn (e,m) = bn_normalise (1,e,m)

-- bn_normalise converts a Bigfloat into "canonical form", that is to say:
-- - the first digit of the mantissa is significant (not 0).
-- - convert the value zero to canonical form (eliminating "-0").
-- To do this it strips off leading zeroes from mantissa, adjusting exponent
-- accordingly.

bn_normalise (s, e, [])	 = bn_0
bn_normalise (s, e, 0:m) = bn_normalise (s, e-1, m)
bn_normalise other	 = other

-- m_trim strips trailing zero digits from a mantissa.  It's tempting to use
-- it liberally because trailing zeroes cause pointless calculation.
-- In practice, it's hardly ever worth doing.  Instead, we test for a few cases
-- here and there where it costs us next to nothing to check for a generated
-- trailing zero (e.g. in bn_twice and bn_carry1), and then just apply m_trim
-- in bn_show to suppress trailing zeroes in the output.
--
-- This is probably where sums like "bn_e `bn_sub` bn_e" bottom out.
-- Would it be better not to strip trailibg zeroes and output 0.0000000....
-- instead?  I think not.

m_trim :: Mantissa -> Mantissa
m_trim (0:x)
	| (m_is_zero x) = []
	| otherwise = 0 : m_trim x
m_trim (a:x) = a : m_trim x
m_trim [] = []

-- UBigfloat and Bigfloat versions:
ubn_trim :: UBigfloat -> UBigfloat
ubn_trim (e,m) = (e, m_trim m)

bn_trim (s,e,m) = (s, e, m_trim m)


--
-- ---------- text <-> Bigfloat conversion functions ----------
--

-- Convert Bigfloat to text.
bn_show (s,e,m)
	= sign ++ ubn_show (e, m_trim m)	-- suppress trailing zeroes
	  where
	  sign	| (s < 0) = "-"
		| otherwise = ""

-- We produce output in three forms: .456, 123 and 123.456.
-- ubn_show deals with the first form, ubn_show' with the other two forms.
ubn_show :: (Integer,[Integer]) -> [Char]
ubn_show (e,m)
	| (m_is_zero m) = "0"
	| (e > 0) = ubn_show' (e,m)
	| (e <= 0) = "." ++ scaletrunc ((rep (-e) '0') ++ m_show m)

-- Deal with Bigfloats whose exponent > 0  (i.e. >= 1.0)
ubn_show' :: (Integer,[Integer]) -> [Char]
ubn_show' (e, []) = rep e '0'	-- Do final zeroes (if any) of integral bn.
ubn_show' (0, m) = '.' : scaletrunc (m_show m)
ubn_show' (e, (a:x)) = mkdigit a : ubn_show' (e-1, x)

m_show :: [Integer] -> [Char]
m_show m = map mkdigit m

mkdigit :: Integer -> Char
-- mkdigit n | (0 <= n && n < 10) = "0123456789" !! n
          -- | (10 <= n && n < 36) = error "mkdigit: Digit > 9!"
-- Bloomin' Haskell's !! only works for Int :(
mkdigit n
	| (n == 0) = '0'
	| (n == 1) = '1'
	| (n == 2) = '2'
	| (n == 3) = '3'
	| (n == 4) = '4'
	| (n == 5) = '5'
	| (n == 6) = '6'
	| (n == 7) = '7'
	| (n == 8) = '8'
	| (n == 9) = '9'
	| otherwise = error "mkdigit: Digit > 9!"

-- bn: Convert text to Bigfloat
-- Syntax: ['-'] [digit] [digit]* || ['-'] [digit]* ++ '.' ++ [digit]*
-- Once we've found a '.', we can accept an infinite number of decimal places.
-- Until we find a '.' (or the end of the string), we don't know what the
-- exponent will be.
-- For simplicity, we also allow "", "-", "." and "-." as synonyms for 0.
bn a = bn_normalise (bn_trim (bn_text a))
       where
       -- We don't make bn_text public because it returns unnormalised Bigfloats
       -- (thus removing a trap for the unwary).
       bn_text ('-':x) = (-1,e,m) where (e,m) = ubn_text x
       bn_text x = (1,e,m) where (e,m) = ubn_text x

-- Convert text to unsigned Bigfloat
ubn_text :: [Char] -> UBigfloat
ubn_text (a:x)
	| (a == '.') = (0, m_text x)
	| (isdigit a) = (xe+1, (char2num a):xm)
	  where (xe,xm) = ubn_text x
	-- non-isdigit failure case?
ubn_text [] = (0,[])

m_text :: [Char] -> Mantissa
m_text (a:x)
	| (isdigit a) = (char2num a): m_text x
	| otherwise = error "Non-digit in Bigfloat text"
m_text [] = []

isdigit :: Char -> Bool
-- isdigit c = ord '0' <= ord c <= ord '9'
isdigit c = iselem c "0123456789"

iselem c [] = False
iselem c (a:x)
	| c == a = True
	| otherwise = iselem c x

char2num c = index c "0123456789"
	     where
	     index c (a:x)
      	           | c == a = 0
      	           | otherwise = 1 + index c x

-- Convert an Integer to a Bigfloat (the lazy way!)
ibn :: Integer -> Bigfloat
ibn i = bn (show i)

-- nbn would let them import smelly floating point values too,
-- except that show gives us things like "1.0e-42" to deal with.
-- Maybe, one day...
-- nbn :: Integer -> Bigfloat
-- nbn i = bn (show i)

--
-- ----- utility functions used here and there in the rest of the library -----
--


-- ubn_same_exp takes a pair of UBigfloats and returns the same pair such that
-- they have the same exponent.  This means prefixing a load of zeroes and
-- upping the exponent of the UBigfloat with the smaller exponent.

ubn_same_exp :: (UBigfloat, UBigfloat) -> (UBigfloat, UBigfloat)
ubn_same_exp (a, b)
	| (ae == be) = ((ae,am), (be,bm))
	| (ae < be) = (ubn_abnormalise be a, b)
	| (ae > be) = (a, ubn_abnormalise ae b)
	  where (ae,am) = a
		(be,bm) = b

-- ubn_abnormalise sets the exponent of its second parameter to its first
-- parameter, giving as a result an un-normalised Bigfloat. This simply involves
-- prefixing a quantity of zeroes to the mantissa and adjusting the exponent.
-- Of course, the new exponent must be >= the old exponent!

ubn_abnormalise :: Exponent -> UBigfloat -> UBigfloat
ubn_abnormalise new_e (old_e, m)
	| (new_e >= old_e) = (new_e, prefix0s (new_e - old_e) m)
	| otherwise = error "ubn_abnormalise called with duff parameters"

-- Add a number of zeroes to the head of a mantissa.
prefix0s n x
	= takeger n [0,0..] ++ x

-- Return absolute value of a Bigfloat
bn_abs (s,e,m) = (1,e,m)

-- Negate a Bigfloat, avoiding the "-0" syndrome.
bn_neg (s,e,m)
	| (m_is_zero m) = bn_0
	| otherwise = (-s,e,m)

--
-- ----------- Stuff for addition/subtraction of Bigfloats ------------
--

-- bn_add gives the arithmetic sum of two Bigfloats.
	-- 1) both +ve or both -ve
	-- 2) sign differs and absolute value of a >= abs value of b.
	-- 3) sign differs and abs(a) < abs(b)

bn_add (as,ae,am) (bs,be,bm)
	= bn_normalise (s,e,m)
	  where
	  (s,(e,m))
		| (as == bs) = (as, ubn_add (ae,am) (be,bm))
	  	| ((ae,am) `ubn_ge` (be,bm)) = (as, ubn_sub (ae,am) (be,bm))
	 	| otherwise = (bs, ubn_sub (be,bm) (ae,am))

-- bn_sub performs subtraction of 2 Bigfloats.
bn_sub a b = bn_add a (bn_neg b)

-- ubn_add gives the arithmetic sum of two unsigned Bigfloats.
-- Strategy: denormalise the arguments so that the have the same exponent,
-- form the sum of the mantissae, then perform the sum after tacking a 0 onto
-- the heads to catch possible carry from the first digit. It would be
-- slightly faster to do m_carry ( 0: m_add2 ... ) but this is simpler.
ubn_add :: UBigfloat -> UBigfloat -> UBigfloat
ubn_add = ubn_add2

-- Older, simpler implementation of ubn_add.
ubn_add1 :: UBigfloat -> UBigfloat -> UBigfloat
ubn_add1 a b 
	= (ae+1, m_addcarry0 am bm)
	  where
	  ((ae,am),(be,bm)) = ubn_same_exp (a, b)

-- ubn_add2 instead takes advantage of unequal exponents in the operands to
-- avoid putting a string of 0s on the head of one parameter and then adding
-- them in uselessly. To be able to do this
-- 1) the exponents must differ by at least two
-- 2) there must be a non-9 digit in the first, non-overlapping digits of the
--    bigger operand, to prevent possible carry due to the second from affecting
--    the first digit of the result.

ubn_add2 :: UBigfloat -> UBigfloat -> UBigfloat
ubn_add2 (ae,am) (be,bm)
	-- First, make sure ae >= be
	| (ae == be) = (ae+1, m_addcarry0 am bm)
	| (ae > be) = ubn_add2' (ae,am) (be,bm)
	| otherwise = ubn_add2' (be,bm) (ae,am)

-- ubn_add2' knows that ae > be
ubn_add2' :: UBigfloat -> UBigfloat -> UBigfloat
ubn_add2' (ae,am) (be,bm)
	  -- First case: difference is just one
	| (ae == be + 1) = ubn_add2'a (ae,am) (be,bm)
	  -- Second case (the fun one): diff >= 2
	| (ae > be + 1) = ubn_add2'b (ae,am) (be,bm)

-- ubn_add2'a: ae = be + 1
-- It is (just barely) worth separating these two cases, as far as
-- speed is concerned, instead of always using m_addcarry0.
ubn_add2'a :: UBigfloat -> UBigfloat -> UBigfloat
ubn_add2'a (ae,(a:x)) (be,bm)
		-- a) exponent differs by 1 and first digit < 9:
		--    there can be no overflow from digit 1.
	| (a < base_1) = (ae, m_addcarry (a:x) (0:bm))
		-- b) exponent differs by 1 and first digit of a is 9:
		--    there may be an overflow from the first digit.
	| otherwise = (ae+1, m_addcarry0 (a:x) (0:bm))

-- ubn_add2'b: ae >= be + 2
-- Make sure there can be no overflow from the result
-- and pass the work on to the lookahead function.
ubn_add2'b :: UBigfloat -> UBigfloat -> UBigfloat
		-- a) a = 0
ubn_add2'b (ae,[]) (be,bm)	= (be,bm)
		-- b) second digit of a is (implied) 0
ubn_add2'b (ae,[x]) (be,bm)	= (ae, x:0:(prefix0s (ae-be) bm))
		-- c) general case
ubn_add2'b (ae,am) (be,bm)	
	  -- There can be no carry into the first digit if the difference
	  -- between the exponents is >= 2 and any of the intervening digits
	  -- are less than (base-1), 'cos they are sure to absorb any carry
	  -- of 1 from the first pair of overlapping digits.
	| (any_lt_base_1 am (ae-be)) = (ae, ubn_add2'' (ae,am) (be,bm))
	  -- All the protruding digits of a are 9!  This is hard to believe...
	| otherwise = (ae+1, m_addcarry0 am bm)

-- ubn_add2'' knows that ae-be >= 2 and that there can be no overflow
-- from the first digit of the result. Since its result always has
-- the same exponent as its first parameter, we just return the mantissa.
ubn_add2'' :: UBigfloat -> UBigfloat -> Mantissa
ubn_add2'' (ae,[]) (be,bm)	= prefix0s (ae-be) bm
ubn_add2'' (ae,[a]) (be,bm)	= a:0:(prefix0s (ae-be-2) bm)
ubn_add2'' (ae,am) (be,bm)
	| (ae == be + 2) = ubn_add2''a (ae,am) (be,bm)	-- End of recursion
	| (otherwise) = ubn_add2''b (ae,am) (be,bm) 	-- This one may recurse
	
-- ae = be+2: End of recursion
ubn_add2''a (ae,(p:q:x)) (be,bm)
	| (q < base_1) = p : m_addcarry (q:x) (0:bm)	-- WIN!
	| (otherwise) = m_addcarry (p:q:x) (0:0:bm)	-- q == base_1 :-(

-- ae > be+2: Can recurse
ubn_add2''b (ae,(a:x)) (be,bm)
	| (any_lt_base_1 x (ae-be-1)) = a : (ubn_add2'' (ae-1,x) (be,bm))
		-- WIN AND PLAY AGAIN!
	| (otherwise) = m_addcarry (a:x) (prefix0s (ae-be) bm)
		-- Boring case

-- Are any of the first n digits of a mantissa less than base-1 ?
-- (if there are less than n digits in the mantissa, then yes,
-- because the omitted digits are implicitly 0)
any_lt_base_1 x n = or (map (< base_1) (takeger n x))


-- ubn_sub performs subtraction of 2 unsigned Bigfloats.
-- It's up to the caller to ensure that a >= b.
-- This uses the version of subcarry that notices when arg2 is shorter than
-- arg1, so that "n `bn_sub` bn_1" costs almost nothing instead of running a
-- pointless carry down an infinite list.
-- TODO: take advantage of ae > be and return the initial part of am
-- unaltered rather than subtracting a string of zeroes from it.
ubn_sub :: UBigfloat -> UBigfloat -> UBigfloat
ubn_sub a b
	= (ae, m_subcarry2 am bm)
	  where
	  ((ae,am),(be,bm)) = ubn_same_exp (a, b)

-- m_addcarry performs addition of 2 mantissas and carry simultaneously.
-- As usual, it must be guaranteed that there will be no carry from the
-- first digit.  Used in add_skew_carry.
-- It only applies carry1 to the part resulting from an addition.
m_addcarry :: Mantissa -> Mantissa -> Mantissa
m_addcarry a b
	= m_carry1a tocarry ++ nocarry
	  where
	  (tocarry,nocarry) = m_add2 a b

-- m_addcarry0 is like m_addcarry except that it prefixes a zero to the
-- result before performing the carry (thereby guaranteeing no overflow)
m_addcarry0 :: Mantissa -> Mantissa -> Mantissa
m_addcarry0 a b
	= m_carry1a (0:tocarry) ++ nocarry
	  where
	  (tocarry,nocarry) = m_add2 a b

-- m_subcarry subtracts b from a and propagates carry (ok, borrow)
m_subcarry :: Mantissa -> Mantissa -> Mantissa
m_subcarry a b = m_borrow1 (m_sub a b)

-- If the second operand is shorter that the first, the final part is
-- returned "as is" since there's no need to do any borrowing from it.
m_subcarry2 a b
	= (m_borrow1a toborrow) ++ noborrow
	  where
	  (toborrow,noborrow) = m_sub2 a b

-- "m_add" adds two mantissas without performing carry across the result
-- leaving each digit with value from 0 to base*2
m_add :: Mantissa -> Mantissa -> Mantissa
m_add (a:x) (b:y) = (a+b):m_add x y
m_add a [] = a
m_add [] b = b

-- m_add2 returns the mantissa in two parts: the first part is the one
-- resulting from addition (to which carry must be applied); the second
-- part is the part resulting from unequal lengths of a and b.
m_add2 :: Mantissa -> Mantissa -> (Mantissa,Mantissa)
m_add2 (a:x) (b:y)
	= ((a+b):tocarry, nocarry)
	  where
	  (tocarry,nocarry) = m_add2 x y
m_add2 a [] = ([],a)
m_add2 [] b = ([],b)

-- "m_sub" subtracts one mantissa from another without performing borrow,
-- leaving each digit with value from -(base-1) to +(base-1).
m_sub :: Mantissa -> Mantissa -> Mantissa
m_sub (a:x) (b:y) = (a-b):m_sub x y
m_sub a [] = a
m_sub [] b = map negate b

-- m_sub2, like m_add2, returns the part that borrowing needs to be performed
-- on and the part it doesn't.
-- Unlike add2, we only win if the first param is longer than the second.
m_sub2 :: Mantissa -> Mantissa -> (Mantissa,Mantissa)
m_sub2 (a:x) (b:y)
	= ((a-b):tocarry, nocarry)
	  where
	  (tocarry,nocarry) = m_sub2 x y
m_sub2 a [] = ([], a)
m_sub2 [] b = (map negate b, [])

-- m_carry1 performs carry throughout a mantissa.
-- Each term must be <= (base-1)*2.
-- This means that the maximum carry from any digit to the next will be 1,
-- To determine the first digit, it inspects at least the first two elements.
-- It assumes that the digits are sufficient to hold the result, ie that
-- there will be no carry from the first digit.
m_carry1 :: [Integer] -> [Integer]
m_carry1 (a:b:x)
	| (b < base_1) = a : m_carry1 (b : x)
	| (b > base_1) = (a + 1) : m_carry1 ((b - base) : x)
	| otherwise = (a + carry): m_carry1 ((b-carry*base):x)
	  where carry = m_carry1from (b:x)
m_carry1 [a]
	| (a > 0) = [a]
	| otherwise = []
m_carry1 [] = []

-- m_carry1a is like m_carry1 except that it never removes trailing zeroes
m_carry1a :: [Integer] -> [Integer]
m_carry1a (a:b:x)
	| (b < base_1) = a : m_carry1a (b : x)
	| (b > base_1) = (a + 1) : m_carry1a ((b - base) : x)
	| otherwise = (a + carry): m_carry1a ((b-carry*base):x)
	  where carry = m_carry1from (b:x)
m_carry1a [a] = [a] 
m_carry1a [] = []


-- m_carry1from returns the carry from a list of digits
-- where each digit is <= (base-1)*2
-- To determine the carry from a list, it inspects at least the first element.
m_carry1from :: [Integer] -> Integer
m_carry1from (a:x)
	| (a < base_1) = 0
	| (a > base_1) = 1
	| otherwise = m_carry1from x
m_carry1from [] = 0

-- m_borrow1 takes a list of digits from -9 to +9 and performs the borrowing
-- necessary to bring them all in the range 0-9.  The first term of the list
-- must not be negative, and must be sufficient to provide any borrowing
-- required of it by the following digit(s).
m_borrow1 :: [Integer] -> [Integer]
m_borrow1 (a:b:x)
	| (b > 0) = a : m_borrow1 (b:x)
	| (b < 0) = (a-1) : m_borrow1 ((b+base):x)
	| otherwise = (a-borrow) : m_borrow1 ((b + borrow * base) : x)
	  where borrow = m_borrow1from (b:x)
m_borrow1 [a]
	| (a > 0) = [a]
	| (a == 0) = []
	| otherwise = error "Internal error: Negative digit in m_borrow1"
m_borrow1 [] = []

-- m_borrow1a is like m_borrow1 except that it doesn't strip trailing zeroes.
m_borrow1a :: [Integer] -> [Integer]
m_borrow1a (a:b:x)
	| (b > 0) = a : m_borrow1a (b:x)
	| (b < 0) = (a-1) : m_borrow1a ((b+base):x)
	| otherwise = (a-borrow) : m_borrow1a ((b + borrow * base) : x)
	  where borrow = m_borrow1from (b:x)
m_borrow1a [a] = [a]
m_borrow1a [] = []

m_borrow1from (a:x)
	| (a > 0) = 0
	| (a < 0) = 1
	| otherwise = m_borrow1from x
m_borrow1from [] = 0

-- Simple doubler. It gains in lookahead because we know that, when you double
-- a number in an even number base, the value of input digit n+2 cannot affect
-- the result digit n.

bn_twice (s,e,m)
	| (m_is_zero m) = bn_0
	| (base `mod` 2 /= 0) = bn_add (s,e,m) (s,e,m)
	| (head m < base_div_2) = (s, e, m_twice m)
	| otherwise = (s, e+1, m_twice (0:m))

-- m_twice is always applied to a list whose first element is < base/2
m_twice :: [Integer] -> [Integer]
m_twice (a:b:x)
	| (b < base_div_2) = (a+a) : m_twice (b:x)
	| otherwise = (a+a+1) : m_twice ((b - base_div_2) : x)
m_twice [0] = []	-- Final digit 5 (ok, base/2) generates a trailing 0.
m_twice [a] = [a+a]
m_twice [] = []

bn_half (s,e,m)
	-- m_half only works for even number bases
	| (base `mod` 2 == 0) = bn_normalise (s, e, m_half m)
	| otherwise = bn_quot (s,e,m) 2

m_half (a:x)
	| (a `mod` 2 == 0) = (a `div` 2) : m_half x
	| otherwise = (a `div` 2) : (m_add [base_div_2] (m_half x))
m_half [] = []
	  
--
-- --------- Simple comparison functions ----------
--

-- Is a Bigfloat negative?
bn_is_neg (s,e,m) = (s < 0)

-- Is it zero?
bn_is_zero (s,e,m) = m_is_zero m

-- Return 0 if the two numbers have the same value,
-- return 1 if the first number is greater than the second
-- return -1 if the second number is greater than the first.
-- Assumes that the parameters are normalised.
bn_cmp (as,ae,am) (bs,be,bm)
	| (as == bs) = if as > 0 
			then ubn_cmp (ae,am) (be,bm)
			else ubn_cmp (be,bm) (ae,am)
	| otherwise = compare as bs

-- tests functions for equality, inequality, less then, less that or equal,
-- greater than, greater than or equal.
bn_eq a b = bn_cmp a b == EQ
bn_ne a b = bn_cmp a b /= EQ
bn_lt a b = bn_cmp a b == LT
bn_le a b = bn_cmp a b /= GT
bn_gt a b = bn_cmp a b == GT
bn_ge a b = bn_cmp a b /= LT

-- Unsigned Bigfloat comparison; result as per bn_cmp
ubn_cmp :: UBigfloat -> UBigfloat -> Ordering
ubn_cmp (ae,am) (be,bm)
	| (ae > be) = GT
	| (ae < be) = LT
	| (ae == be) = m_cmp am bm

-- tests functions for equality, inequality, less then, less that or equal,
-- greater than, greater than or equal.
ubn_eq, ubn_ne, ubn_lt, ubn_le, ubn_gt, ubn_ge
	:: UBigfloat -> UBigfloat -> Bool
ubn_eq a b = ubn_cmp a b == EQ
ubn_ne a b = ubn_cmp a b /= EQ
ubn_lt a b = ubn_cmp a b == LT
ubn_le a b = ubn_cmp a b /= GT
ubn_gt a b = ubn_cmp a b == GT
ubn_ge a b = ubn_cmp a b /= LT

m_cmp :: Mantissa -> Mantissa -> Ordering
-- Compare two mantissae.
m_cmp (a:x) (b:y) | (a > b) = GT
		| (a < b) = LT
		| (a == b) = m_cmp x y
m_cmp a [] | (m_is_zero a) = EQ
	 | otherwise = GT
m_cmp [] b | (m_is_zero b) = EQ
	 | otherwise = LT

-- equal, not equal, less then [or equal to], greater than [or equal to]
-- for mantissae.
m_eq :: Mantissa -> Mantissa -> Bool
m_ne :: Mantissa -> Mantissa -> Bool
m_eq a b = m_cmp a b == EQ
m_ne a b = m_cmp a b /= EQ
-- More optimised versions of these follow...
-- m_lt a b = m_cmp a b == LT
-- m_le a b = m_cmp a b /= GT
-- m_gt a b = m_cmp a b == GT
-- m_ge a b = m_cmp a b /= LT

-- optimised m_lt, used in m_div
m_lt (a:x) (b:y)
	| (a /= b) = (a < b)
	| otherwise = m_lt x y
m_lt a [] = False
m_lt [] b = not (m_is_zero b)

-- reuse code
m_gt a b = m_lt b a
m_ge a b = not (m_lt a b)
m_le a b = not (m_lt b a)

m_is_zero :: Mantissa -> Bool
-- Are all elements of a mantissa equal to zero?
-- It'd be nice to use a "fold" function here
-- Was:
-- m_is_zero [] = True
-- m_is_zero (a:x)
-- 	= False,	if a > 0
-- 	= m_is_zero x,	if a = 0
-- 	= error "negative number in mantissa!", otherwise
m_is_zero x = and (map (== 0) x)

--
-- ------------- Simple multiply by an integer -------------
--

-- bn_times multiplies Bigfloat by an Integer, done by repeated addition.
-- The *first* parameter is the Integer, which seems more convenient for
-- partial parameterisation purposes.
-- bn_add normalises the result for us.
bn_times n b
	-- TODO | ( not (integer n)) = error "bn_times can only multiply by integers."
	| (n > 0) = bn_times' n b
	| (n < 0) = bn_neg (bn_times' (-n) b)
	| (n == 0) = bn_0

-- reduced case: n always >= 1
-- Optimisation: (2n) x b = (n x b) + (n x b)
-- Optimisation to do: multiply all digits by n, then do a super-carry.
bn_times' n b
	| (n `mod` 2 == 0) = bn_twice (bn_times' (n `div` 2) b)
	| (n > 2) = b `bn_add` (bn_times' (n-1) b)
	| (n == 1) = b

-- Same stuff for mantissae.  Caller must guarantee no overflow, and
-- maximum n is base-1.
m_times :: Integer -> Mantissa -> Mantissa
m_times n m
	| (0 <= n && n < base) = m_carry2 (map (* n) m)
	| otherwise = error ("m_times " ++ show n)

--
-- ------------- Long multiplication -------------
--

-- bn_mul: Multiply two Bigfloats.  bn_sqr is a modified version of this.

bn_mul (as,ae,am) (bs,be,bm)
	= bn_normalise (as*bs, e, m)
	  where (e,m) = ubn_mul (ae,am) (be,bm)

ubn_mul :: UBigfloat -> UBigfloat -> UBigfloat
-- Add a leading 0 so that there's somewhere for the carry to propagate into.
-- If we were to simply m_multiply the mantissas without prefixing a 0, the
-- exponent of the result would be (ae + be - 1) (think: 0.1 * 0.1 = 0.01).
-- The extra digit that m_mul prefixes to the result cancels the "- 1".
ubn_mul (ae,am) (be,bm)
	= (ae+be, m_mul am bm)

-- m_mul multiplies two mantissas.
--
-- NOTE! m_mul prefixes a digit to the result so that there's somewhere for
-- the carry from the first result digit to propagate into,
-- so callers must adjust result exponents accordingly.
--
-- Strategy:
-- sum the columns of the skewed cross product and then do an intelligent
-- super-carry on the result.  The super-carry works by applying a one-digit
-- carry to the list of column sums until we can be sure that the maximum
-- value of each column is (base * 2).  Then it applies m_carry.
-- [Alternate version to try: apply one-digit carry until maximul column value
-- is (base - 1) ^ 2, then apply m_carry2.]
--
-- We do this by giving two lists to the carry-preparation function:
-- - the list of column sums
-- - the list of maximum values for the column sums.
-- and applying the one-digit carry to both lists until the maximum value
-- of the leading digits is within the required maximum.

m_mul :: [Integer]->[Integer]->[Integer]
m_mul a b = supercarry (0:(partial_products a b)) max_mul_vals

supercarry list maxvals = m_carry1 (reduce_for_carry list maxvals)

partial_products :: [Integer]->[Integer]->[Integer]
partial_products a b = add_skew (cross_product a b)

-- cross_product:
-- given [A,B,C] [a,b,c]
-- calculate partial result
--        [ [Aa,Ab,Ac],
--	    [Ba,Bb,Bc],
--	    [Ca,Cb,Cc] ]
cross_product :: [Integer]->[Integer]->[[Integer]]
cross_product a b =
	map (ma_times b) a
	where
	ma_times l n = map (* n) l

-- add_skew: Return the sum of a potentially infinite number of mantissas,
-- offsetting the rows by one place:
-- given  [ [Aa,Ab,Ac],
--	    [Ba,Bb,Bc],
--	    [Ca,Cb,Cc] ]
-- return [ [Aa,Ab,Ac],
--	   +   [Ba,Bb,Bc],
--	   +      [Ca,Cb,Cc] ]
--          ================
--          [r1,r2,r3,r4,r5]

add_skew :: [[Integer]] -> [Integer]
add_skew ((a:x):y) = a : m_add x (add_skew y)
--add_skew ([]:y) = 0 : add_skew y  -- never happens (?)

-- Sum of a single row is just that row.
add_skew [x] = x

-- We get handed an empty list when they multiply by bn_0.
add_skew [] = []

-- max_mul_vals gives the maximum possible value of a partial product term.
-- max_mul_vals = 0 : partial_products nines nines  where  nines = [9,9..]
-- Turns out to be equal to
max_mul_vals = [0, base_1 ^ 2 .. ]

-- reduce_for_carry takes a list of column sums and a list of the maximum
-- possible values of each column sum, and applies a one-digit carry until
-- the terms it returns are sure to be <= (base-1) * 2
-- The second list is always infinite.
-- In practice, repeated applications of carry1digit to max_mul_vals do this:
-- [0,81,162,243,324,405,486,567,648,729,810,891,972,1053,1134,1215,1296,1377..
-- [8,17,26,35,44,53,62,71,80,90,89,98,107,116,125,134,143,152,161,171,170,179..
-- [9,9,9,9,9,9,9,9,9,8,18,18,18,18,18,18,18,18,18,18,17,27,27,27,27,27,27,27..
-- [9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9..
-- and the ..9,9,9,8,18,18,18.. pattern always repeats itself further down the
-- list in successive iterations.
-- Thus we can tell when the current digit is gonna be ok by looking for when
-- the following digit's maximum possible value exceeds 9 (the 8,17 or 8,18
-- point); at this point a new invocation of carry1digit is necessary from the
-- "8" digit onwards.
-- Note! Although this reduces all terms in the 2nd parameter to the range
-- 0-base-1, it *does not* necessarily reduce all terms of the first parameter
-- to this range. For pathological cases like
--	bn_sqr (bn "999999999999999999999999999999999999999999"),
-- there is a "..9,9,9,9,9,9,10.." near the end that would require many
-- iterations of carry1digit to propagate the carry back to its proper place.
-- Only the final application of carry1 can resolve this (see m_mul above).
reduce_for_carry :: [Integer] -> [Integer] -> [Integer]
reduce_for_carry (a:b:x) (c:d:y)
	| (d < base) = a : reduce_for_carry (b:x) (d:y)
	| otherwise = reduce_for_carry (carry1digit (a:b:x)) (carry1digit (c:d:y))
--reduce_for_carry [a] y = [a]
--reduce_for_carry [] y = []
reduce_for_carry x y = x	-- synthesis of the above two cases

-- carry1digit just performs the carry from each digit into the previous
-- one without progating further.
carry1digit (a:b:x) = (a + b `div` base) : carry1digit (b `mod` base : x)
--carry1digit [a] = [a]
--carry1digit [] = []
carry1digit x = x


-- Perform carry throughout number. Each term must be <= (base-1)^2.
-- This ensures that the maximum carry from any digit to the next will be
-- (base-2) because:
-- max_digit = (base - 1) * (base - 1) = (base^2 - 2 * base + 1)
-- max_carry = (max_digit + max_carry) `div` base
-- If max_carry is (base - 2) then (max_digit + max_carry) `div` base =
--	( (base^2 - 2 * base + 1) + (base - 2) ) `div` base =
--	( (base^2 - 2 * base + (1 + (base - 2) ) `div` base =
--	( (base^2 - 2 * base + (base - 1) ) `div` base =
--	( (base - 2) + 0 ) = base - 2
-- Example of highest carry:
--	[0 81 81 81] -> [0 81 89 1] -> [0 89 9 1] -> [8 9 9 1]

m_carry2 :: [Integer] -> [Integer]
m_carry2 (a:b:c:x)

	-- Max carry from b is (base - 2), so if b `mod` base < 2, the carry
	-- from x cannot affect a.  This turns out to be a win so rarely that
	-- the extra checking slows us down more than we gain.
	-- = (a + b `div` base) : m_carry2 ((b `mod` base) : c : x),
	--	if b `mod` base < 2	

	-- Carry from x into c can only affect carry from c into b by 1,
	-- so if (b + (c `div` 10)) `mod` 10 is less than 9,
	-- x won't affect the carry from b into a.
	| bc `mod` base < base_1 = (a + bc `div` base) : m_carry2 (bc `mod` base : c `mod` base : x)
	
	-- general case
	| otherwise = (a + carry) : m_carry2 ((b - carry * base) : c : x)
	  where
	  bc = b + c `div` base
	  carry = m_carry2from (b:c:x)

m_carry2 [a,b]
	| (b `mod` base > 0) = [a + b `div` base, b `mod` base]
	| otherwise = [a + b `div` base]
m_carry2 [a]
	| (a > 0) = [a]
	| otherwise = []
m_carry2 [] = []

-- Return the carry from the list of numbers where each number <= (base-1)^2
-- Maximum carry from any digit into the previous one is (base-2).
-- To find the carry from A:B:C... in base 10:
-- If A `mod` base < 2, carry = a `div` base (carry from B cannot affect carry from A)
-- The carry from x into b can only affect the carry from b into a by one.
-- So if (a + (b `div` 10)) `mod` 10 is less than 9,
-- x won't affect the carry from a.

m_carry2from :: [Integer] -> Integer
m_carry2from (a:b:x)
-- This first case gains so infrequently that it ends up a net loss.
--	= a `div` base			, if a `mod` base < 2
	| (ab `mod` base < base_1) = ab `div` base
	| otherwise = (a + m_carry2from(b:x)) `div` base
	  where ab = a + (b `div` base)
m_carry2from [a] = a `div` base
m_carry2from [] = 0

-- ---------- end of long multiplication stuff ----------


--
-- bn_raise - simple power function by repeated multiplication
-- a is the Bigfloat to raise to a power;
-- x is the Integer power to raise it to.
-- Optimisation: bn_raise x (2*n) = sqr (bn_raise x n)

bn_raise a x
	-- TODO | ( not (integer x)) = error "bn_raise only does integer powers"
	| (x < 0) = bn_div bn_1 (bn_raise' a (-x))
	| (x == 0) = bn_1
	| otherwise = bn_raise' a x
bn_raise' a x
	| (x `mod` 2 == 0) = bn_sqr (bn_raise' a (x `div` 2))
        | (x >= 2) = bn_mul a (bn_raise' a (x-1))
	| (x == 1) = a

-- bn_sqr - square a Bigfloat, used frequently in the scientific functions,
-- hence optimised here to avoid pointlessly doing certain things twice.
-- was: bn_sqr x = x `bn_mul` x
bn_sqr (as,ae,am)
	= bn_normalise (1, e, m)
	  where (e,m) = ubn_sqr (ae,am)

ubn_sqr (ae,am)
	= (ae+ae, m_sqr am)

m_sqr :: [Integer]->[Integer]
m_sqr a = supercarry (0:(add_skew2 (square_product a))) max_mul_vals

-- square_product is like cross_product except that it folds the lower left
-- triangle of partial results onto the upper right triangle (since the
-- cross product of a single number is symmetrical about its main diagonal).
-- Given [a,b,c]
-- calculate partial result
--	[ [aa,2ab,2ac,..],
--	  [bb,2bc,2bd,..],
--	  [cc,2cd,2ce,..] ]
square_product :: [Integer]->[[Integer]]
square_product (a:x)
	= (a*a : map (* (a+a)) x) : square_product x
square_product [] = []

-- add_skew2: Return the sum of a potentially infinite number of mantissas,
-- offsetting the rows by two places:
-- given  [ [aa,2ab,2ac,..],
--	    [bb,2bc,2bd,..],
--	    [cc,2cd,2ce,..] ]
-- return [ [aa,2ab,2ac,..],
--	   +        [bb,2bc,2bd,..],
--	   +                [cc,2cd,2ce,..] ]
--          ================
--          [r1,r2,r3,r4,r5]

add_skew2 :: [[Integer]] -> [Integer]
add_skew2 ((a:b:x):y) = a : b : m_add x (add_skew2 y)

-- Sum of a single row is just that row.
add_skew2 [x] = x

-- This happens when squaring numbers with an odd number of digits
add_skew2 ([a]:y) = a : 0 : add_skew2 y

--add_skew2 ([]:y) = 0 : 0 : add_skew2 y  -- never happens (?)

-- We get handed an empty list when they multiply by bn_0.
add_skew2 [] = []

--
-- bn_quot: Long division of a Bigfloat by an integer.
-- Uses Haskell's marvellous infinitely-long integers to do all the hard work.
-- The time it takes is pretty much independent of the value of q.
--

bn_quot (s,e,m) q
	-- TODO | ( not (integer q)) = error "bn_quot can only divide by integers"
	| (q == 0) = error "bn_quot cannot divide by 0"
	| (m_is_zero m) = bn_0
	| (q == 1) = (s,e,m)
	| (q == -1) = (-s,e,m)
	| (q > 1) = bn_normalise (s, e, m_quot (m,q))
	| (q < -1) = bn_normalise (-s, e, m_quot (m,-q))

m_quot :: ([Integer],Integer) -> [Integer]
m_quot (m,q)
	| (m_is_zero m) = []
	| otherwise = (a `div` q) : m_quot (shiftin ((a `mod` q):x), q)
	  where
	  (a:x) = m
	  shiftin (a:b:x) = (a * base + b) : x
	  shiftin [a] = [a * base]

--
-- bn_div: Long division
--

bn_div (as,ae,am) (bs,be,bm)
	= bn_normalise (as*bs, ae-be+1, m_div am bm)

ubn_div (ae,am) (be,bm) = (ae-be+1, m_div am bm)

-- m_div takes two mantissae and divides the first by the second, returning a
-- mantissa.
-- div_loop requires divisor <= dividend
-- - zero divided by anything is 0.
-- - div_loop returns the first digit of the result and the remainder of the
--   dividend after the divisor has been subtracted from it "dig" times.
-- "dend" is the dividend; "sor" is the divisor.
--
-- mdiv' seems to be necessary in Haskell cos I don't know how to get a where
-- to apply to all 4 cases

m_div dend sor = m_div' dend sor (m_div_loop (0, dend, sor))

m_div' dend sor (dig, dend2, sor2)
	| (m_is_zero dend) = []
	-- The following line is logically ok but hits so rarely that
	-- it slows things down more than it speeds them up.
	-- | if sor `m_gt` dend = 0: m_div dend (0:sor)
	| (dend2 == []) = [dig]   -- exact
	| (head dend2 == 0) = dig : m_div (tail dend2) sor
	| otherwise = dig : m_div dend2 (0:sor)

{- was:  (works with hugs)
m_div dend sor
	| (m_is_zero dend) = []
	-- The following line is logically ok but hits so rarely that
	-- it slows things down more than it speeds them up.
	-- | if sor `m_gt` dend = 0: m_div dend (0:sor)
	| (dend2 == []) = [dig]   -- exact
	| (head dend2 == 0) = dig : m_div (tail dend2) sor
	| otherwise = dig : m_div dend2 (0:sor)
	  where
	  (dig, dend2, sor2) = m_div_loop (0, dend, sor)
-}

-- Work out how many times the divisor can be subtracted from the dividend
-- without the dividend going negative.
--
-- "hack" is our conservative first approximation for the first digit, formed
-- by dividing the first two digits of dend and sor.
-- The slow m_div_loop is then used to arrive at the exact result.
-- The >2-2-0-1 order is empirically the fastest using bn_pi as the test piece.
m_div_loop (0, dend, sor)
	| hack > 2 = m_div_loop' (hack, dend `m_subcarry` (m_times hack sor), sor)
	| hack == 2 = m_div_loop' (2, dend `m_subcarry` (m_twice sor), sor)
	| hack == 0 = m_div_loop' (0, dend, sor)
	| hack == 1 = m_div_loop' (1, dend `m_subcarry` sor, sor)
	  where
	  -- The "+ 1" makes sure we don't exceed in our estimate.
	  hack = (take2 dend) `div` (take2 sor + 1)

take2 (a:b:x) = a * base + b
take2 [a] = a * base
take2 [] = 0

-- This is the regular version, done by repeated subtraction.
-- Using m_subcarry2 here and above turns out to be >10% slower that m_subcarry
-- in this context, probably because the optimisation never comes into effect.

m_div_loop' x
	= until div_loop_done div_loop_body x
	  where
	  div_loop_done (dig, dend, sor) = dend `m_lt` sor
	  div_loop_body (dig, dend, sor)
		= (dig+1, m_subcarry dend sor, sor)


--
-- ---------------- Square root ---------------------
--

-- Square root adapted from Mr C. Woo's algorithm for abacus:
-- convert number in base 10 to base 100 by pairing digits,
-- then each application of sqrt_loop generates a digit of the result.
bn_sqrt (s,e,m)
	| (s /= 1) = error "bn_sqrt of negative number"
	| (e `mod` 2 == 0) = bn_normalise (1, e `div` 2, m_sqrt m)
	| (e `mod` 2 == 1) = bn_normalise (1, (e+1) `div` 2, m_sqrt (0:m))

m_sqrt :: [Integer] -> [Integer]
m_sqrt [] = []			-- sqrt(0)
m_sqrt m = m_sqrt' (0, (toIntegerHd (m_sqrbase m)))

-- convert the head of a list of Integer to Integer to prime the sqrt engine
toIntegerHd :: [Integer] -> (Integer, [Integer])
toIntegerHd (a:x) = (toInteger a, x)

-- Convert mantissa in base b to mantissa in base b^2.
m_sqrbase :: [Integer] -> [Integer]
m_sqrbase (a:b:m) = (a * base + b) : m_sqrbase m
m_sqrbase [a] = [a * base]
m_sqrbase [] = []

m_sqrt' :: (Integer,(Integer,[Integer])) -> [Integer]
m_sqrt' (r1,(hd_o1,tl_o1))
	| (hd_o1 == 0 && m_is_zero tl_o1) = []
	| otherwise = (fromInteger (r2 `mod` toInteger base)) : m_sqrt' (r2 * toInteger base, m_sqrt_shiftin (hd_o2,tl_o1))
	  where
	  (r2,hd_o2) = m_sqrt_loop (r1,hd_o1)

-- Shift another term of the operand into the calculation.
m_sqrt_shiftin :: (Integer, [Integer]) -> (Integer, [Integer])
m_sqrt_shiftin (a,b:x) = (a * toInteger base_2 + toInteger b, x)
m_sqrt_shiftin (a,[]) = (a * toInteger base_2, [])

m_sqrt_loop :: (Integer,Integer) -> (Integer,Integer)
m_sqrt_loop ro
	= until sqrt_loop_done sqrt_loop_body ro
	  where
	  sqrt_loop_done (r, o) = 2*r >= o	-- was: 2*r+1 > o
	  sqrt_loop_body (r, o) = (r+1, o - (2*r+1))

--
-- ------------ Number generators ---------------------
--

-- bn_rnd gives a pseudo-random Bigfloat in the range 0 <= bn_rnd x < 1
-- based on the seed, an integer >= 0.
-- There's no science behind these constants: I just picked them out of the air.
bn_rnd seed
	= bn_normalise (1, 0, rnd seed)
	  where
	  rnd seed 
		= newseed `mod` base : rnd newseed
		  where newseed = (seed * 91971 + 7473) `mod` 327671

--
-- bn_e: A minimally modified version of DAT's edigits
--

bn_e = bn_make (1, 1, e_digits)

e_digits :: [Integer]
e_digits = (2: e_convert(repeat 1))

e_convert :: [Integer] -> [Integer]
e_convert x
	= (head x'):e_convert (tail x')
          where x' = e_norm 2 (0:map (10*) x)

e_norm :: Integer -> [Integer] -> [Integer]
e_norm c (d:e:x)
	| (e `mod` c + 9 < c) = d + e `div` c: e' `mod` c : x'
        | otherwise = d + e' `div` c : e' `mod` c : x'
          where
          (e':x') = e_norm (c+1) (e:x)

-- TEST VERSION OF BN_E
-- A better function may be the classic SUM (n <- 0..) (1 / n!)
-- Since running edigits for thousands of digits generates some enormous
-- Miranda integers which make the heap grow indefinitely.
-- The series converges fast enough for sum_series from 1/9! onwards.
-- (but it's not anything like as fast as the e_digits version, taking
-- nearly three times the reductions and over four times the CPU!)
	-- Terms 0,1,2
-- bn_e2a = bn "2.5"
	-- Terms 3..8
-- bn_e2b = foldr bn_add bn_0 (map (bn_1 `bn_quot`) [6,24,120,720,5040,40320])
	-- Terms 9..
-- bn_e2c = bn_sum_series (map (bn_1 `bn_quot`) [ a | (a,n) <- (40320*9,10), (a*n,n+1) .. ])
	-- e
-- bn_e2 = bn_e2a `bn_add` bn_e2b `bn_add` bn_e2c
	-- Lazy man's version of terms 0-8
-- bn_e2ab = bn_make (1, 1, [2,7,1,8,2,7,8,7] ++ concat (repeat [6,9,8,4,1,2]))
-- bn_e3 = bn_e2ab `bn_add` bn_e2c

--
-- ----------------- Logarithms and trigonometric functions -----------------
--
-- From libmath.b, part of the GNU "bc":
--
-- exp x = 1 + x + x^2/2! + x^3/3! ..., if x <= 1
--       = (exp (x/2))^2, otherwise
--
-- ln x = 2 * (a + a^3/3 + a^5/5 ...), if 0.5 < x < 2
--        where a = (x - 1) / (x + 1)
--      = 2 * ln (sqrt x), otherwise
--
-- sin x = x - x^3/3! + x^5/5! - x^7/7! ...
--
-- cos x = sin (x + pi/2) where pi/2 = (atan 1) * 2
--
-- atan x = x - x^3/3 + x^5/5 - x^7/7 ..., if x < c
--        = atan(c) + atan ( (x - c) / (1 + x * c) ), otherwise
--	    where c = 0.2

-- bn_exp
--
-- exp x = 1 + x + x^2/2! + x^3/3! ..., if x <= 1
--       = (exp (x/2))^2, otherwise
--
-- CONVERGANCE:
-- x <= 1, so we just need to be sure that the divisor gets multiplied by
-- at least 10 for each iteration.  This is true for the term x^10/10! onward.
-- If x <= 0.5 instead, it's true from the term x^5/5! onward.
-- For x <= 0.1, it's true for the whole series.
--
-- We use sum_series from the third term (x^2/2!) onwards.  For the 4th term
-- to be 1/10th of the 3rd one, x/3 must be <= 0.1
-- This is a shame because once the bottom reaches 10 * x, the series converges
-- ever faster.  A better strategy might be to sum the first N terms by hand
-- (where N is determined by the value of x) and then apply sum_series to the
-- rest, avoiding the call to bn_sqr.
--
-- An alternative strategy for negative values of x would be to reduce x
-- to -1 by recursion and then pair the terms before summing the series.

bn_exp x
	| (bn_is_zero x) = bn_1
	| (bn_is_neg x) = bn_1 `bn_div` (bn_exp (bn_neg x))
	| (x `bn_gt` (bn "0.3")) = bn_sqr (bn_exp (bn_half x))
	| otherwise = (bn_1 `bn_add` x) `bn_add` (bn_sum_series (zipWith bn_quot (exp_top x) exp_bot))

-- exp_top x = [ a | a <- bn_sqr x, bn_mul x a .. ]
exp_top x = bn_sqr x : map (bn_mul x) (exp_top x)	-- TODO (SLOOOW)
-- exp_bot = [ a | (a,n) <- (2,3), (a*n,n+1) .. ]
exp_bot = map fac [2..]

fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n-1)

-- bn_ln
--
-- From the math library of bc:
-- ln x = 2 * (a + a^3/3 + a^5/5 ...), if 0.5 < x < 2
--        where a = (x - 1) / (x + 1)
--      = 2 * ln (sqrt x), otherwise
--
-- CONVERGANCE:
-- 0.5 < x < 2, so (x-1)/(x+1) ranges from (-.5/1.5) to (1/3), ie -1/3 to +1/3.
-- Each term is slightly less than a^2 times the previous one...
-- unfortunately 1/9th is not quite 1/10th!
-- For the series to converge, we need
--	a^2 <= 1/10   which is to say   -1/sqrt(10) <= a <= 1/sqrt(10)
-- Lower limit:
--	(x-1)/(x+1) = -1/sqrt(10)
--	x-1 = -1/sqrt(10) * (x+1)
--	x-1 = (-1/sqrt(10))x + (-1/sqrt(10))
--	x (1 + 1/sqrt(10)) = 1 - 1/sqrt(10)
--	x = (1 - 1/sqrt(10)) / (1 + 1/sqrt(10))
-- Upper limit:
--	(x-1)/(x+1) = 1/sqrt(10)
--	x-1 = (1/sqrt(10)) x + 1/sqrt(10)
--	x (1 - 1/sqrt(10)) = 1 + 1/sqrt(10)
--	x = (1 + 1/sqrt(10)) / (1 - 1/sqrt(10))
--
-- one_root10 =  bn_1 `bn_div` (bn_sqrt (bn "10"))
-- one_plus_root10 =  bn_1 `bn_add` one_root10
-- one_minus_root10 =  bn_1 `bn_sub` one_root10
-- lower_limit = one_minus_root10 `bn_div` one_plus_root10
-- upper_limit = one_plus_root10 `bn_div` one_minus_root10
--
-- Results:
--	 .5194938532 (we use ...533 to be sure)
--	1.9249505911
--
-- Results:
--	 .51949385329... (we use ...8533)
--	1.9249505911...
--
-- Fortunately, numbers very close to these limits calculate faster
-- than other numbers, since the resulting series converges faster.
-- We therefore keep these limits as wide as we can.
--
-- If we were to sum pairs of terms before calling sum_series, we would get a
-- convergance of order a^4 instead of a^2, which allows us to process a wider
-- range of values of x without having to recurse (.28013 - 3.56977). This
-- proves to be three or four times faster in cases where x has few digits and
-- is in the range .281-.519 or 1.925-3.569, because the time taken by bn_ln
-- depends on the number of digits in the operand more than anything else,
-- and a square root almost always gives an infinite number if digits.
-- In all other cases, this strategy turns out to be three or four times slower.

-- Limits within which the summation of the series will work
ln_lower_limit = bn ".5194938533"
ln_upper_limit = bn "1.9249505911"

bn_ln x
	| (bn_is_zero x) = error "Can't take log of zero"
	| (bn_is_neg x) = error "Can't take log of negative numbers"
	| (x `bn_eq` bn_1) = bn_0
	| (x `bn_lt` ln_lower_limit || x `bn_gt` ln_upper_limit) = bn_twice (bn_ln (bn_sqrt x))
	| otherwise = bn_ln' x

-- Sum the series
bn_ln' x
	= bn_twice (bn_sum_series (zipWith bn_quot (ln_top x) [1,3..]))

-- Implement Miranda's second form of list comprehensions:
-- given a starting value a and a function f, return the infinite list
-- a (f a) (f (f a)) ..
gen2 a f = a : (gen2 (f a) f)

ln_top x = gen2 a (bn_mul asq)
	-- = [n | n <- a, n `bn_mul` asq ..]
	  where
	  asq = bn_sqr a
	  a = (x `bn_sub` bn_1) `bn_div` (x `bn_add` bn_1)

-- Now we can do a power function as we should.  Mind you, it's incredibly slow!
bn_pow a x = bn_exp (bn_ln a `bn_mul` x)

-- bn_sin
--
-- sin x = x - x^3/3! + x^5/5! - x^7/7! ...
--
-- CONVERGANCE:
-- In each term, the top increases by a maximum factor of pi/2^2 (1.570796..)
-- and the bottom increases by 2*3 then 4*5 then 6*7, so the minimum
-- convergence factor is 1.57 / 6 = 0.1309.  For the series formed by summing
-- pairs of adjacent terms, it is always less than 0.1.
--
-- NOTE! Our value of bn_pi is calculated from the sum of a series,
-- so bn_sin will be at its fastest from -pi/2 to +pi/2
-- (Though you can't take the sin of exactly pi/2 because bn_gt in bn_sin'
-- goes off to infinity.)
--
-- bn_sum_series normalises the result for us.

-- A) reduce range of argument to first quadrant: 0..pi/2
--   1) reduce to positive numbers: sin(n) = -sin(-n)
bn_sin n | (bn_is_zero n) = bn_0
	 | (bn_is_neg n) = bn_neg (bn_sin' (bn_neg n))
	 | otherwise = bn_sin' n
--   2) reduce to 0..2*pi: for n > 2 * pi, sin(n) = sin(n - 2*pi)
bn_sin' n | (n `bn_gt` bn_2_pi) = bn_sin' (bn_sub n bn_2_pi)
	  | otherwise = bn_sin'' n
--   3) reduce to 0..pi: for pi < n <= 2*pi, sin(n) = -sin(n - pi)
bn_sin'' n | (n `bn_gt` bn_pi) = bn_neg (bn_sin''' (bn_sub n bn_pi))
	   | otherwise = bn_sin''' n
--   4) reduce to 0..pi/2: for pi/2 < n <= pi, sin(n) = sin(pi - n)
bn_sin''' n | (n `bn_gt` bn_pi_2) = bn_sin'''' (bn_sub bn_pi n)
	    | otherwise = bn_sin'''' n
-- B) sum the series: sin x = x - x^3/3! + x^5/5! - x^7/7!
bn_sin'''' n = bn_sum_series (sin_series n)

-- \\ Create the series for sin --

-- invert the sign of the even terms of the series
-- sin_top x = [ x, -x^3, x^5, -x^7 .. ]
-- NB: sin_top is re-used below in atan_series
-- sin_top x = [ a | a <- x, (bn_mul minus_x_squared) a .. ]
sin_top x = gen2 x (bn_mul minus_x_squared)
	    where minus_x_squared = bn_neg (bn_mul x x)

-- The divisors of the terms of the series:
-- sin_bot = [ factorial n | n <- [1,3..] ]
-- sin_bot = [ a | (a,n) <- (1,2), (a*n*(n+1),n+2) .. ]
sin_bot = [ fac n | n <- [1,3..] ]	-- TODO (SLOW)

sin_series x = addpairs (zipWith bn_quot (sin_top x) sin_bot)

-- Add pairs of terms in a series (also used in bn_atan, below)
addpairs :: [Bigfloat] -> [Bigfloat]
addpairs (a:b:x) = bn_add a b : addpairs x
addpairs [a] = [a]
addpairs [] = []

-- cosine - obvious enough, since we have sin: cos(x) = sin(x + pi/2)
-- We add special test for cos(0) since sin(pi/2) never returns.
bn_cos x
	| (bn_is_zero x) = bn_1		-- avoid bottom-out case
	| otherwise = bn_sin (x `bn_add` bn_pi_2)


-- atan x = x - x^3/3 + x^5/5 - x^7/7 ..., if x < c
--        = atan(c) + atan ( (x - c) / (1 + x * c) ), otherwise
--	    where c = 0.2
--
-- By inspection with a test function, the series converges faster than
-- one digit per term (paired terms, that is) with c = 0.5; even faster
-- with c = 0.2 (which is the value used inside "GNU bc" whence we robbed the
-- algorithm).  Since, for us, the cost of doing the mul - `div` etc involved
-- in recursing is higher than that of summing the series, we use 0.5.
-- Experimentally (with bc), .562341325 converges ok (by a factor of >10)
-- while .56241326 doesn't quite.

c_lim = bn ".562341325"

-- bn_atan2 takes this two steps further: It lets you specify c at runtime,
-- and applies two recursive calls in one go if appropriate.  This has three
-- advantages: it saves a humungous amount of multiplication and division,
-- lets us calculate atan(c) once instead of twice and avoids indefinite
-- recursion in intermediate results in many cases when the top and bottom
-- of the division attain unfortunately-related infinitely-long values.
-- In practice, with c = 0.5:
-- 0 <= x <= .5		atan(x)
-- .5 < x < 1.3XXX	atan(.5) + atan(d)
-- 1.3XXX < x < 5.5	atan(.5) + atan(.5) + atan(e)
-- 5.5 <= x		atan(.5) + atan(.5) + atan(.5) + atan(f)
--
-- 0.5 seemed a good general purpose value for c - other values run slower
-- except for some special cases - until I found out that it never gives an
-- answer to atan(7), atan(9), atan(12), atan(15) and probably others too.
--
-- In the case of atan(1), used for pi, c=0.4 runs staggeringly faster than 0.5:
-- atan2 1 0.5 => atan 0.5 + atan 0.33333333333..
-- atan2 1 0.4 => (2 * atan 0.4) + atan 0.02439.. which converges much faster
-- (requiring 345,003 reductions instead of 1,907,882 for 20 decimal places).
-- Your mileage may vary taking the atan of other specific values.

bn_atan2 :: Bigfloat -> Bigfloat -> Bigfloat
bn_atan2 x c
	| (c `bn_gt` c_lim) = error "Internal error: bn_atan2 limit check"
	| (bn_is_zero x) = bn_0
	| (bn_is_neg x) = bn_neg (bn_atan2' (bn_neg x) c)
	| otherwise = bn_atan2' x c

-- bn_atan
-- Specialised version of bn_atan2, using constant c_use for value of c
-- giving speedup for the constant calculations that drop out.
-- The speedup is large (more than twice) when several atans are performed
-- in one Miranda execution since atan(c) and derived calculation are only
-- done once.

-- For the generic constant-c version, the value of c to use.
-- No single digit version of c works for small positive integers:
-- they all (.2, .3, .4, .5) go into infinite regress in the summation
-- of the series, so we use a 2-digit value.
--
-- Experimentally, using values from .41 to .56 for c_use
-- .45 and .48 bottom out in atan(6)
-- .56 bottoms out on atan(37)
-- .49 was the fastest in tests from 1 to 101

c_use = bn ".49"

bn_atan x
	| (bn_is_zero x) = bn_0
	| (bn_is_neg x) = bn_neg (bn_atan' (bn_neg x))
	| otherwise = bn_atan' x

-- atan2 x c
--	= atan x			, if x <= c
--	= atan c + atan d 		, if x > c && d <= c
--	= atan c + atan c + atan2 e c	, otherwise
--	  where
--	  d = (x - c) / (1 + x * c)
--	  e = (d - c) / (1 + d * c)
--
--  e = (x - 2c - x c^2) / (1 + 2xc - c^2)
--  or  (x(1-c^2) - 2c) / (1 + 2xc - c^2)
--
-- bn_atan2' knows that its parameter x is not negative.

bn_atan2' :: Bigfloat -> Bigfloat -> Bigfloat
bn_atan2' x c
	| (x `bn_le` c) = bn_atan'' x
	| (d `bn_le` c) = bn_atan'' c `bn_add` (bn_atan2' d c)
	| otherwise = bn_twice (bn_atan'' c) `bn_add` (bn_atan2' e c)
	  where
  	  d = (x `bn_sub` c) `bn_div` (bn_1 `bn_add` x_mul_c)
	  e = etop `bn_div` ebot
	  etop = (x `bn_mul` (bn_1 `bn_sub` csq)) `bn_sub` (bn_twice c)
	  ebot = bn_1 `bn_add` (bn_twice x_mul_c) `bn_sub` csq
	  csq = bn_sqr c
	  x_mul_c = x `bn_mul` c

-- Now that we have arctangent, we can calculate pi as 4 * atan(1)
--
-- "Now I, even I would celebrate
--  In rhymes unapt the great
--  Immortal Syracusan, rivaled nevermore
--  Who, in his wondrous lore
--  Passed on before
--  Left men his guidance
--  How to circles mensurate."
-- bn_pi = bn "3.141592653589793238462643383279" :-)
bn_pi30 = bn "3.141592653589793238462643383279"

bn_2_pi = bn_twice bn_pi		-- 2 * pi  (used in bn_sin)
bn_pi = bn_twice bn_pi_2		-- pi
bn_pi_2 = bn_twice bn_pi_4		-- pi / 2  (used in bn_sin)
-- bn_pi_4 = bn_atan2 bn_1 (bn "0.4")	-- pi / 4

-- for pi/4, calculated with atan(1) using c = 0.4,
-- we expand the first iteration of the atan series with constant values:
-- d = (1 - .4) / (1 + .4) = .6/1.4 = 3/7 = 0.428571..., which is not < .4
-- so...
-- etop = (1 * (1 - .4^2)) - (2 * .4) = 1-.16 - .8 = 0.04
-- ebot = 1 + (2 * 1 * .4) - (.4^2) = 1.8 - .16 = 1.64
-- e = 0.04 / 1.64 = 4 / 164 = 2 / 82 = 1 / 41
-- pi/4 = (2 * atan(0.4)) + atan(1/41)
-- this is 4.5% better in space and speed than the bn_pi_4 above.

bn_pi_4 = bn_twice (bn_atan'' (bn ".4")) `bn_add` (bn_atan'' (bn_quot bn_1 41))


-- Optimised version using constant c_use as the value of c
bn_atan' :: Bigfloat -> Bigfloat
bn_atan' x
	| (x `bn_le` c_use) = bn_atan'' x
	| (d `bn_le` c_use) = atan_c `bn_add` (bn_atan' d)
	| otherwise = twice_atan_c `bn_add` (bn_atan' e)
	  where
  	  d = (x `bn_sub` c_use) `bn_div` (bn_1 `bn_add` x_mul_c)
	  e = etop `bn_div` ebot
	  etop = (x `bn_mul` one_sub_sqr_c) `bn_sub` twice_c
	  ebot = one_sub_sqr_c `bn_add` (bn_twice x_mul_c)
	  x_mul_c = x `bn_mul` c_use

-- In Miranda, global constant functions seem to fare better than derived
-- constant calculations in where clauses.
atan_c = bn_atan'' c_use
twice_atan_c = bn_twice atan_c
sqr_c = bn_sqr c_use
twice_c = bn_twice c_use
one_sub_sqr_c = bn_1 `bn_sub` sqr_c

-- START OF WORK IN PROGRESS ON OPTIMISED ATAN WITH FIXED C=0.5

-- This works and is 3% faster in space and speed than bn_atan,
-- but bottoms out on certain values (7, 9, 12, 15 ...)
-- probably because unfortunate pairs occur in the summing series:
-- infinitely-long terms that, when subtracted, would give a
-- finite-length sum.

bn_atan5 x
	| (bn_is_zero x) = bn_0
	| (bn_is_neg x) = bn_neg (bn_atan5' (bn_neg x))
	| otherwise = bn_atan5' x

-- Even more specialised version uses constant 0.5 for value of c
-- This lets us use bn_times, bn_quot, bn_half instead of mul and `div`
-- atan5 x
--	= atan x			, if x <= .5
--	= atan .5 + atan d 		, if x > .5 && d <= .5
--	= atan .5 + atan .5 + atan5 e 	, otherwise
--	  where
--	  d = (x - .5) / (1 + x * .5) = (x - .5) / (1 + x/2)
--	  e = (d - .5) / (1 + d * .5)
--
--  e = (x - 2*.5 - x .5^2) / (1 + 2x*.5 - .5^2)
--    = (x(1-.5^2) - 2*.5) / (1 + x - .25)
--    = (.75*x - 1) / (.75 + x)
bn_atan5' :: Bigfloat -> Bigfloat
bn_atan5' x
	| (x `bn_le` bn_p5) = bn_atan'' x
	| (d `bn_le` bn_p5) = atan_p5 `bn_add` (bn_atan5' d)
	| otherwise = twice_atan_p5 `bn_add` (bn_atan5' e)
	  where
  	  d = (x `bn_sub` bn_p5) `bn_div` (bn_1 `bn_add` (bn_half x))
	  e = (bn_quot (bn_times 3 x) 4 `bn_sub` bn_1) `bn_div` (bn_p75 `bn_add` x)

bn_p5 = bn(".5")
bn_p75 = bn(".75")
atan_p5 = bn_atan'' bn_p5
twice_atan_p5 = bn_twice atan_p5

-- END OF WORK IN PROGRESS ON OPTIMISED ATAN

-- This version always sums the series.  x must be <= c_lim
-- so that the series converges ok.
bn_atan'' x = bn_sum_series (atan_series x)

-- The terms of the series for atan(x).
-- The dividends are the same as for sin(), so we reuse them.
atan_series x = addpairs (zipWith bn_quot (sin_top x) [1,3..])

bn_tan x = (bn_sin x) `bn_div` (bn_cos x)
bn_sinh x = bn_half ( bn_exp x `bn_sub` (bn_exp (bn_neg x)) )
bn_cosh x = bn_half ( bn_exp x `bn_add` (bn_exp (bn_neg x)) )
bn_tanh x = (bn_sinh x) `bn_div` (bn_cosh x)
bn_asinh x = bn_ln ( x `bn_add` (bn_sqrt (bn_sqr x `bn_add` bn_1)) )
bn_acosh x = bn_ln ( x `bn_add` (bn_sqrt (bn_sqr x `bn_sub` bn_1)) )
bn_atanh x = bn_half ( bn_ln (bn_add bn_1 x) `bn_sub` (bn_ln (bn_sub bn_1 x)) )

-- bn_sum_series sums a converging series of Bigfloat.  It requires that every
-- term in the series be less that 1/10th (ok, 1/baseth) of the previous one
-- and that all elements in the series be of the same sign (all +ve or all -ve).
-- ubn2bn normalises the result for us.
-- This copes with series in which the terms are all positive or all negative.
-- The check for bn_is_zero (head s) is necessary because if the first element
-- is zero, we can't tell whether the elements are all positive or all negative.
-- If we are passed a series of mixed positive and negative terms by mistake,
-- the calculation will throw a fatal error in bn2ubn.
-- Every term in the series has already been normalised.
bn_sum_series :: [Bigfloat] -> Bigfloat
	-- first, eliminate two simple cases
	-- (These never happen because we are always handed infinite lists)
-- bn_sum_series [] = bn_0
-- bn_sum_series [a] = a
	-- now reduce the job to summing a positive series
bn_sum_series s
	| (bn_is_zero (head s)) = bn_sum_series (tail s)
	| (bn_is_neg (head s)) = bn_neg (bn_sum_series (map bn_neg s))
	| otherwise = ubn2bn (ubn_sum_series (map bn2ubn s))

-- unsigned version - tack a leading zero onto all terms so that we can be
-- sure that the sum of any term and all the following ones will have the
-- same exponent as the term itself.  The fact that the first digit of each
-- term will now be 0 is taken account of in the unequal-exponent lazy
-- lookahead code, so is not as much of a lossage as it might seem.
-- Without it, we produce "10" digits from time to time.
-- However it is much faster (2/3 of the reductions). Can we use this
-- by letting it produce 10 digits and then applying m_carry1?
ubn_sum_series :: [UBigfloat] -> UBigfloat
ubn_sum_series s
	= ubn_sum_series2 (map prefix_0 s)
	  where
	  prefix_0 (e,m) = (e+1, 0:m)

-- ubn_sum_series2 is the second version of the series summer that uses the same
-- kind of logic as ubn_add2 to return the initial digit from the first term
-- through inspection only of the first term and the exponent of the second.
--
-- When we can no longer be sure that the digits of the result will be the
-- same as the digits of the first term, we add the first and second terms
-- and repeat the process with the sum as the first term of the list.
--
-- We know that zeroes have been prefixed to all terms, guaranteeing that the
-- exponent of the sum from a particular term onwards will not grow as a result
-- of adding in the following terms, and that the maximum addition from the
-- first digit of the second term and all that follow it will be 1.  This
-- corresponds to the logic of seeing whether there are any non-9 digits in
-- ubn_add2.

ubn_sum_series2 :: [UBigfloat] -> UBigfloat
	  -- From now on we don't normalise or adjust exponents, so return
	  -- the exponent and work out the mantissa.
ubn_sum_series2 ((ae,am):x) = (ae, ubn_sum_series2' ((ae,am):x))
ubn_sum_series2 [] = ubn_0

-- ubn_sum_series2' just returns the mantissa of the sum, which has the same
-- exponent as the first term of the series.
ubn_sum_series2' :: [UBigfloat] -> Mantissa
-- Most common case first...
ubn_sum_series2' ((ae,(a:ax)):(be,bm):x)
	  -- We can be sure of the first digit of a if ae-be >= 2 and there is a
	  -- digit between the first digit of a and the digit above the first of
	  -- b whose value is less that (base-1).  Remember that the first digit
	  -- of b will be 0, and so the most that the sum of b onwards could add
	  -- to the final result will be a 1 in the first position of b.
	  -- Note: the any_lt test *includes* the first digit of a that overlaps b.
	  -- I think this is right...  Here's the worst case:
	  -- a: 0 8 9 9 9 9 9 9 9 9 9 9
	  -- b:   0 9 9 9 9 9 9 9 9 9
	  -- c:     0 9 9 9 9 9 9 9 9
	  -- b+c: 1 0 9 9 9 9 9 9 9 9 
	  -- abc: 9 
	  -- .... so given the 8, the first digit cannot be affected by adding in
	  -- b,c...  ok! In fact, this is worse than the worst case because
	  -- .0099 is not < 1/10th of .0899
	| (ae-be) >= 2 && any_lt_base_1 ax (ae-be)
	  = a : ( ubn_sum_series2' ( ((ae-1),ax):(be,bm):x ) )
		
	  -- Otherwise add the first two terms of the series together and repeat the
	  -- process with this as the new first term of the series, which will give
	  -- us more distance between the exponents.
	| otherwise
	  = ubn_sum_series2' ((ae, (m_addcarry (a:ax) (prefix0s (ae-be) bm))) : x)
		
-- If am is exhausted, eliminate the term and proceed with the rest
ubn_sum_series2' ((ae,[]):(be,bm):x)
	= prefix0s (ae-be) (ubn_sum_series2' ((be,bm):x))
-- Deal with terminal cases
ubn_sum_series2' [(e,m)] = m		-- One item
ubn_sum_series2' [] = []		-- No items

-- END OF WORK IN PROGRESS ON SERIES SUMMER

--
-- === End of bigfloat.hs ===
--
