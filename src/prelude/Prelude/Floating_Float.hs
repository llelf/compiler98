module Prelude(Floating(..)) where

--import qualified Ratio

instance  Floating Float  where
    pi			=  3.1415926535897932 -- 384626433832795028841972 -- Enough decimals
    exp	x		=  exp x	-- MAGIC
    log	x		=  log x	-- MAGIC
    sqrt x		=  sqrt x	-- MAGIC
    sin	x		=  sin x	-- MAGIC
    cos	x		=  cos x	-- MAGIC
    tan	x		=  tan x	-- MAGIC
    asin x		=  asin x	-- MAGIC
    acos x		=  acos x	-- MAGIC
    atan x		=  atan x	-- MAGIC
    sinh x              = 0.5 * (exp x - exp (-x))
    cosh x              = 0.5 * (exp x + exp (-x))
    tanh x              = (a-b)/(a+b) where a = exp x ; b = exp (-x)
    asinh x = log (x + sqrt (1+x*x))
    acosh x = log (x + (x+1) * sqrt ((x-1)/(x+1)))
    atanh x = log ((x+1) / sqrt (1 - x*x))

