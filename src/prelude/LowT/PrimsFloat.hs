module Prelude where

-- Num
foreign import "primFloatSignumC" primFloatSignum :: Float -> Float
foreign import "primFloatAbsC" primFloatAbs :: Float -> Float
foreign import "primFloatAddC" primFloatAdd :: Float -> Float -> Float
foreign import "primFloatSubC" primFloatSub :: Float -> Float -> Float
foreign import "primFloatMulC" primFloatMul :: Float -> Float -> Float
-- Fractional
foreign import "primFloatDivC" primFloatDiv :: Float -> Float -> Float

-- Eq and Ord
foreign import "primFloatEqC" primFloatEq   :: Float -> Float -> Bool
foreign import "primFloatLeC" primFloatLe   :: Float -> Float -> Bool
foreign import "primFloatLtC" primFloatLt   :: Float -> Float -> Bool

primFloatNe :: Float -> Float -> Bool
primFloatNe a b = not (primFloatEq a b)
primFloatGe :: Float -> Float -> Bool
primFloatGe a b = primFloatLe b a
primFloatGt :: Float -> Float -> Bool
primFloatGt a b = primFloatLt b a

-- Floating
foreign import "primFloatExpC"  primFloatExp	:: Float -> Float
foreign import "primFloatLogC"  primFloatLog	:: Float -> Float
foreign import "primFloatSqrtC" primFloatSqrt	:: Float -> Float
foreign import "primFloatSinC"  primFloatSin	:: Float -> Float
foreign import "primFloatCosC"  primFloatCos	:: Float -> Float
foreign import "primFloatTanC"  primFloatTan	:: Float -> Float
foreign import "primFloatAsinC" primFloatAsin	:: Float -> Float
foreign import "primFloatAcosC" primFloatAcos	:: Float -> Float
foreign import "primFloatAtanC" primFloatAtan	:: Float -> Float

