module Prelude where

-- Num
foreign import "primDoubleSignumC" primDoubleSignum :: Double -> Double
foreign import "primDoubleAbsC" primDoubleAbs :: Double -> Double
foreign import "primDoubleAddC" primDoubleAdd :: Double -> Double -> Double
foreign import "primDoubleSubC" primDoubleSub :: Double -> Double -> Double
foreign import "primDoubleMulC" primDoubleMul :: Double -> Double -> Double
-- Fractional
foreign import "primDoubleDivC" primDoubleDiv :: Double -> Double -> Double

-- Eq and Ord
foreign import "primDoubleEqC" primDoubleEq   :: Double -> Double -> Bool
foreign import "primDoubleLeC" primDoubleLe   :: Double -> Double -> Bool
foreign import "primDoubleLtC" primDoubleLt   :: Double -> Double -> Bool

primDoubleNe :: Double -> Double -> Bool
primDoubleNe a b = not (primDoubleEq a b)
primDoubleGe :: Double -> Double -> Bool
primDoubleGe a b = primDoubleLe b a
primDoubleGt :: Double -> Double -> Bool
primDoubleGt a b = primDoubleLt b a

-- Floating
foreign import "primDoubleExpC"  primDoubleExp  :: Double -> Double
foreign import "primDoubleLogC"  primDoubleLog  :: Double -> Double
foreign import "primDoubleSqrtC" primDoubleSqrt :: Double -> Double
foreign import "primDoubleSinC"  primDoubleSin  :: Double -> Double
foreign import "primDoubleCosC"  primDoubleCos  :: Double -> Double
foreign import "primDoubleTanC"  primDoubleTan  :: Double -> Double
foreign import "primDoubleAsinC" primDoubleAsin :: Double -> Double
foreign import "primDoubleAcosC" primDoubleAcos :: Double -> Double
foreign import "primDoubleAtanC" primDoubleAtan :: Double -> Double
