module Prelude where

-- Num
foreign import ccall "primFloatSignumC" primFloatSignum :: Float -> Float
foreign import ccall "primFloatAbsC" primFloatAbs :: Float -> Float
foreign import ccall "primFloatAddC" primFloatAdd :: Float -> Float -> Float
foreign import ccall "primFloatSubC" primFloatSub :: Float -> Float -> Float
foreign import ccall "primFloatMulC" primFloatMul :: Float -> Float -> Float
-- Fractional
foreign import ccall "primFloatDivC" primFloatDiv :: Float -> Float -> Float

-- Eq and Ord
foreign import ccall "primFloatEqC" primFloatEq   :: Float -> Float -> Bool
foreign import ccall "primFloatLeC" primFloatLe   :: Float -> Float -> Bool
foreign import ccall "primFloatLtC" primFloatLt   :: Float -> Float -> Bool

primFloatNe :: Float -> Float -> Bool
primFloatNe a b = not (primFloatEq a b)
primFloatGe :: Float -> Float -> Bool
primFloatGe a b = primFloatLe b a
primFloatGt :: Float -> Float -> Bool
primFloatGt a b = primFloatLt b a

-- Floating
foreign import ccall "primFloatExpC"  primFloatExp	:: Float -> Float
foreign import ccall "primFloatLogC"  primFloatLog	:: Float -> Float
foreign import ccall "primFloatSqrtC" primFloatSqrt	:: Float -> Float
foreign import ccall "primFloatSinC"  primFloatSin	:: Float -> Float
foreign import ccall "primFloatCosC"  primFloatCos	:: Float -> Float
foreign import ccall "primFloatTanC"  primFloatTan	:: Float -> Float
foreign import ccall "primFloatAsinC" primFloatAsin	:: Float -> Float
foreign import ccall "primFloatAcosC" primFloatAcos	:: Float -> Float
foreign import ccall "primFloatAtanC" primFloatAtan	:: Float -> Float

