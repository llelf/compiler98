module Complex where

infix 6 :+

#if !defined(TRACING)
data  (RealFloat a) => Complex a = !a :+ !a
#else
data  (RealFloat a) => Complex a =  a :+  a
#endif
