module Ix(Ix(..)) where

import RangeSize

instance  (Ix a1, Ix a2, Ix a3, Ix a4) => Ix (a1,a2,a3,a4)  where
    range ((l1,l2,l3,l4),(u1,u2,u3,u4)) =
          [(i1,i2,i3,i4) | i1 <- range (l1,u1),
                           i2 <- range (l2,u2),
                           i3 <- range (l3,u3),
                           i4 <- range (l4,u4)]

    index ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
        index (l4,u4) i4 + rangeSize (l4,u4) * (
         index (l3,u3) i3 + rangeSize (l3,u3) * (
           index (l2,u2) i2 + rangeSize (l2,u2) * (
             index (l1,u1) i1)))

    inRange ((l1,l2,l3,l4),(u1,u2,u3,u4)) (i1,i2,i3,i4) =
         inRange (l1,u1) i1
           && inRange (l2,u2) i2
           && inRange (l3,u3) i3
           && inRange (l4,u4) i4
