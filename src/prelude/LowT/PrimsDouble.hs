module Prelude where

-- Num
foreign import ccall "primDoubleSignumC" primDoubleSignum :: Double -> Double
foreign import ccall "primDoubleAbsC" primDoubleAbs :: Double -> Double
foreign import ccall "primDoubleAddC" primDoubleAdd :: Double -> Double -> Double
foreign import ccall "primDoubleSubC" primDoubleSub :: Double -> Double -> Double
foreign import ccall "primDoubleMulC" primDoubleMul :: Double -> Double -> Double
-- Fractional
foreign import ccall "primDoubleDivC" primDoubleDiv :: Double -> Double -> Double

-- Eq and Ord
foreign import ccall "primDoubleEqC" primDoubleEq   :: Double -> Double -> Bool
foreign import ccall "primDoubleLeC" primDoubleLe   :: Double -> Double -> Bool
foreign import ccall "primDoubleLtC" primDoubleLt   :: Double -> Double -> Bool

primDoubleNe :: Double -> Double -> Bool
primDoubleNe a b = not (primDoubleEq a b)
primDoubleGe :: Double -> Double -> Bool
primDoubleGe a b = primDoubleLe b a
primDoubleGt :: Double -> Double -> Bool
primDoubleGt a b = primDoubleLt b a

-- Floating
foreign import ccall "primDoubleExpC"  primDoubleExp  :: Double -> Double
foreign import ccall "primDoubleLogC"  primDoubleLog  :: Double -> Double
foreign import ccall "primDoubleSqrtC" primDoubleSqrt :: Double -> Double
foreign import ccall "primDoubleSinC"  primDoubleSin  :: Double -> Double
foreign import ccall "primDoubleCosC"  primDoubleCos  :: Double -> Double
foreign import ccall "primDoubleTanC"  primDoubleTan  :: Double -> Double
foreign import ccall "primDoubleAsinC" primDoubleAsin :: Double -> Double
foreign import ccall "primDoubleAcosC" primDoubleAcos :: Double -> Double
foreign import ccall "primDoubleAtanC" primDoubleAtan :: Double -> Double
