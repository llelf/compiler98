-- ----------------------------------------------------------------------------
-- library used by all programs transformed for tracing by Hat
-- combinators in Haskell and interfaces to C-functions

module Hat 
  (R(R),mkR,SR,Trace,NmType,ModuleTraceInfo
  ,NmCoerce(toNm)
  ,ap1,ap2,ap3,ap4,ap5,ap6,ap7,ap8,ap9,ap10,ap11,ap12,ap13,ap14,ap15
  ,rap1,rap2,rap3,rap4,rap5,rap6,rap7,rap8,rap9,rap10,rap11,rap12,rap13
  ,rap14,rap15
  ,pap0,pap1,pap2,pap3,pap4,pap5,pap6,pap7,pap8,pap9,pap10,pap11,pap12
  ,pap13,pap14
  ,lazySat,lazySatLonely,eagerSat
  ,fun1,fun2,fun3,fun4,fun5,fun6,fun7,fun8,fun9,fun10,fun11,fun12,fun13
  ,fun14,fun15
  ,indir
  ,conInt,conChar,conInteger,conFloat,conDouble,conRational
  ,conCons
  ,con0,con1,con2,con3,con4,con5,con6,con7,con8,con9,con10,con11,con12
  ,con13,con14,con15
  ,prim0,prim1,prim2,prim3,prim4,prim5,prim6,prim7,prim8,prim9,prim10,prim11
  ,prim12,prim13,prim14,prim15
  ,cn1,cn2,cn3,cn4,cn5,cn6,cn7,cn8
  ,pa0,pa1,pa2,pa3,pa4
  ,mkTRoot,mkTNm
  ,mkSourceRef,mkNoSourceRef,mkAtomCon,mkAtomId,mkAtomIdToplevel,mkModule
  ,mkTHidden
  ,mkNTInt,mkNTChar,mkNTInteger,mkNTRational,mkNTFloat,mkNTDouble,mkNTTuple
  ,mkNTFun,mkNTCase,mkNTLambda,mkNTDummy,mkNTCString,mkNTIf,mkNTGuard
  ,mkNTContainer,mkNTRational
  ,openTrace,closeTrace,outputTrace,fatal
  ,Pos,noPos
  ) where

-- import Prelude (Int,Char,Integer,Float,Double,Rational,String,IO
--                ,Bool(True,False),Ordering(LT,EQ,GT))
-- import qualified Prelude 
  -- qualified to mark clearly where original Prelude is used
import Ratio (numerator,denominator)

import PackedString (PackedString,packString) -- NONPORTABLE
-- import MagicTypes (NmType,CStructure) -- NONPORTABLE
  -- magic C-type living in Haskell heap

type Pos = Int
noPos = 0

-- ----------------------------------------------------------------------------

openTrace :: String -> IO ()
openTrace progname = openTrace' (packString progname)

foreign import "openTrace"
  openTrace' :: PackedString -> IO () 

foreign import "closeTrace"
  closeTrace :: IO ()

-- actually for pattern match error only:
foreign import "fatal"
  fatal :: Trace -> a 

-- ----------------------------------------------------------------------------
-- part from PreludeDebug (only for untrusted code)

{-
Invariant: the trace argument of R is always fully evaluated.
Trace arguments that are passed to functions are always fully evaluated.
No, not for pap: pap will force evaluation immediately.
-}

data R a = R a Trace

type Fun a b = Trace -> R a -> R b


{- data constructor R strict in trace argument -}
mkR :: a -> Trace -> R a
mkR x t = t `Prelude.seq` R x t



newtype SR     = SR Int
newtype Trace  = Trace Int
newtype NmType = NmType Int
newtype ModuleTraceInfo = MTI Int

-- ----------------------------------------------------------------------------
-- toNm used to coerce primitive return value from a foreign function
-- into a Trace structure
-- simplifies transformation; also in n+k patterns don't know exact type
-- relies on transformed type just being a type synonym for untraced type
class NmCoerce a where
  toNm :: Trace -> a -> SR -> R a


{-
counterpart to 'enter' for primitives: ensures that the result trace
is fully evaluated (the trace contains the result value).
  ** now `enter' has been eliminated, no longer sure about this defn **
-}
primEnter :: NmCoerce a => SR -> Trace -> a -> R a
primEnter sr t e = let vn = toNm t e sr
                      in e `Prelude.seq` vn

-- ----------------------------------------------------------------------------
-- combinators for n-ary application in a non-projective context.

ap1 :: SR -> Trace -> R (Trace -> R a -> R r) -> R a -> R r 

ap1 sr t (R rf tf) a@(R _ at) = 
  let t' = mkTAp1 t tf at sr
  in  lazySat (rf t' a) t'

ap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  let t' = mkTAp2 t tf at bt sr
  in  lazySat (pap1 sr t t' (rf t' a) b) t'

ap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t' = mkTAp3 t tf at bt ct sr
  in  lazySat (pap2 sr t t' (rf t' a) b c) t'


ap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t' = mkTAp4 t tf at bt ct dt sr
  in  lazySat (pap3 sr t t' (rf t' a) b c d) t'


ap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) = 
  let t' = mkTAp5 t tf at bt ct dt et sr
  in  lazySat (pap4 sr t t' (rf t' a) b c d e) t'

ap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) = 
  let t' = mkTAp6 t tf at bt ct dt et ft sr
  in  lazySat (pap5 sr t t' (rf t' a) b c d e f) t'

ap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) = 
  let t' = mkTAp7 t tf at bt ct dt et ft gt sr
  in  lazySat (pap6 sr t t' (rf t' a) b c d e f g) t'

ap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
  let t' = mkTAp8 t tf at bt ct dt et ft gt ht sr
  in  lazySat (pap7 sr t t' (rf t' a) b c d e f g h) t'

ap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
  let t' = mkTAp9 t tf at bt ct dt et ft gt ht it sr
  in  lazySat (pap8 sr t t' (rf t' a) b c d e f g h i) t'


ap10 :: SR -> Trace 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i (Fun j r)))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R r 

ap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
  let t' = mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
  in  lazySat (pap9 sr t t' (rf t' a) b c d e f g h i j) t'

ap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) = 
  let t' = mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
  in  lazySat (pap10 sr t t' (rf t' a) b c d e f g h i j k) t'

ap12 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) l@(R _ lt) =  
  let t' = mkTAp12 t tf at bt ct dt et ft gt ht it jt kt lt sr
  in  lazySat (pap11 sr t t' (rf t' a) b c d e f g h i j k l) t'

ap13 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) l@(R _ lt) m@(R _ mt) =  
  let t' = mkTAp13 t tf at bt ct dt et ft gt ht it jt kt lt mt sr
  in  lazySat (pap12 sr t t' (rf t' a) b c d e f g h i j k l m) t'

ap14 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) =  
  let t' = mkTAp14 t tf at bt ct dt et ft gt ht it jt kt lt mt nt sr
  in  lazySat (pap13 sr t t' (rf t' a) b c d e f g h i j k l m n) t'

ap15 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) o@(R _ ot) =  
  let t' = mkTAp15 t tf at bt ct dt et ft gt ht it jt kt lt mt nt ot sr
  in  lazySat (pap14 sr t t' (rf t' a) b c d e f g h i j k l m n o) t'



{- 
Combinators for n-ary applications in a projective context 
The difference to the ap_n combinators is that in the case
of trusting no hidden trace node has to be created,
because the skipped trace node has the same type as the 
parent redex.
-}

-- suspected:

rap1 :: SR -> Trace -> R (Fun a r) -> R a -> R r

rap1 sr t (R rf tf) a@(R _ at) = 
  let t' = mkTAp1 t tf at sr
  in  rf t' a


rap2 :: SR -> Trace -> R (Fun a (Fun b r)) -> R a -> R b -> R r

rap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  let t' = mkTAp2 t tf at bt sr
  in  pap1 sr t t' (rf t' a) b

rap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t' = mkTAp3 t tf at bt ct sr
  in pap2 sr t t' (rf t' a) b c

rap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t' = mkTAp4 t tf at bt ct dt sr
  in pap3 sr t t' (rf t' a) b c d

rap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) = 
  let t' = mkTAp5 t tf at bt ct dt et sr
  in  pap4 sr t t' (rf t' a) b c d e

rap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) = 
  let t' = mkTAp6 t tf at bt ct dt et ft sr
  in  pap5 sr t t' (rf t' a) b c d e f

rap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) = 
  let t' = mkTAp7 t tf at bt ct dt et ft gt sr
  in  pap6 sr t t' (rf t' a) b c d e f g

rap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
  let t' = mkTAp8 t tf at bt ct dt et ft gt ht sr
  in  pap7 sr t t' (rf t' a) b c d e f g h

rap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
  let t' = mkTAp9 t tf at bt ct dt et ft gt ht it sr
  in  pap8 sr t t' (rf t' a) b c d e f g h i

rap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
  let t' = mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
  in  pap9 sr t t' (rf t' a) b c d e f g h i j

rap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) = 
  let t' = mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
  in  pap10 sr t t' (rf t' a) b c d e f g h i j k

rap12 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) l@(R _ lt) =  
  let t' = mkTAp12 t tf at bt ct dt et ft gt ht it jt kt lt sr
  in  pap11 sr t t' (rf t' a) b c d e f g h i j k l

rap13 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) l@(R _ lt) m@(R _ mt) =  
  let t' = mkTAp13 t tf at bt ct dt et ft gt ht it jt kt lt mt sr
  in  pap12 sr t t' (rf t' a) b c d e f g h i j k l m

rap14 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) =  
  let t' = mkTAp14 t tf at bt ct dt et ft gt ht it jt kt lt mt nt sr
  in  pap13 sr t t' (rf t' a) b c d e f g h i j k l m n

rap15 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) o@(R _ ot) =  
  let t' = mkTAp15 t tf at bt ct dt et ft gt ht it jt kt lt mt nt ot sr
  in  pap14 sr t t' (rf t' a) b c d e f g h i j k l m n o


-- Combinators for n-ary application used by the combinators above.
-- Introduces a new application node if the function is a saturated
-- application.

pap0 :: Trace -> R r -> R r

pap0 t e = e


pap1 :: SR -> Trace -> Trace -> R (Trace -> R a -> R r) -> R a -> R r

pap1 sr p t (R rf tf) a =
  if t `sameAs` tf 
    then rf t a
    else case a of 
           R _ at ->
             let t' = mkTAp1 p tf at sr
             in  t' `Prelude.seq` eagerSat (rf t' a) t'

pap2 sr p t (R rf tf) a b =
  if t `sameAs` tf 
    then pap1 sr p t (rf t a) b
    else case a of 
           R _ at -> case b of 
             R _ bt ->
               let t' = mkTAp2 p tf at bt sr
               in  eagerSat (pap1 sr p t' (rf t' a) b) t'


pap3 sr p t (R rf tf) a b c =
  if t `sameAs` tf 
    then pap2 sr p t (rf t a) b c
    else case a of 
           R _ at -> case b of 
             R _ bt -> case c of 
               R _ ct -> 
                 let t' = mkTAp3 p tf at bt ct sr
                 in eagerSat (pap2 sr p t' (rf t' a) b c) t'


pap4 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
  if t `sameAs` tf 
    then pap3 sr p t (rf t a) b c d
    else let t' = mkTAp4 p tf at bt ct dt sr
         in  eagerSat (pap3 sr p t' (rf t' a) b c d) t'

pap5 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) =
  if t `sameAs` tf 
    then pap4 sr p t (rf t a) b c d e
    else let t' = mkTAp5 p tf at bt ct dt et sr
         in  eagerSat (pap4 sr p t' (rf t' a) b c d e) t'

pap6 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                      f@(R _ ft) =
  if t `sameAs` tf 
    then pap5 sr p t (rf t a) b c d e f
    else let t' = mkTAp6 p tf at bt ct dt et ft sr
         in  eagerSat (pap5 sr p t' (rf t' a) b c d e f) t'

pap7 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                      f@(R _ ft) g@(R _ gt) =
  if t `sameAs` tf 
    then pap6 sr p t (rf t a) b c d e f g
    else let t' = mkTAp7 p tf at bt ct dt et ft gt sr
         in  eagerSat (pap6 sr p t' (rf t' a) b c d e f g) t'

pap8 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                      f@(R _ ft) g@(R _ gt) h@(R _ ht) =
  if t `sameAs` tf 
    then pap7 sr p t (rf t a) b c d e f g h
    else let t' = mkTAp8 p tf at bt ct dt et ft gt ht sr
         in  eagerSat (pap7 sr p t' (rf t' a) b c d e f g h) t'


pap9 :: SR -> Trace -> Trace
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i r))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i 
     -> R r 

pap9 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                      f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) =
  if t `sameAs` tf 
    then pap8 sr p t (rf t a) b c d e f g h i
    else let t' = mkTAp9 p tf at bt ct dt et ft gt ht it sr
         in  eagerSat (pap8 sr p t' (rf t' a) b c d e f g h i) t'

pap10 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                       f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) =
  if t `sameAs` tf 
    then pap9 sr p t (rf t a) b c d e f g h i j
    else let t' = mkTAp10 p tf at bt ct dt et ft gt ht it jt sr
         in eagerSat (pap9 sr p t' (rf t' a) b c d e f g h i j) t'

pap11 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                       f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                       k@(R _ kt) =
  if t `sameAs` tf 
    then pap10 sr p t (rf t a) b c d e f g h i j k
    else let t' = mkTAp11 p tf at bt ct dt et ft gt ht it jt kt sr
         in  eagerSat (pap10 sr p t' (rf t' a) b c d e f g h i j k) t'

pap12 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                       f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                       k@(R _ kt) l@(R _ lt) =
  if t `sameAs` tf 
    then pap11 sr p t (rf t a) b c d e f g h i j k l
    else let t' = mkTAp12 p tf at bt ct dt et ft gt ht it jt kt lt sr
         in  eagerSat (pap11 sr p t' (rf t' a) b c d e f g h i j k l) t'

pap13 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                       f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                       k@(R _ kt) l@(R _ lt) m@(R _ mt) =
  if t `sameAs` tf 
    then pap12 sr p t (rf t a) b c d e f g h i j k l m
    else let t' = mkTAp13 p tf at bt ct dt et ft gt ht it jt kt lt mt sr
         in  eagerSat (pap12 sr p t' (rf t' a) b c d e f g h i j k l m) t'

pap14 sr p t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                       f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                       k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) =
  if t `sameAs` tf 
    then pap13 sr p t (rf t a) b c d e f g h i j k l m n
    else let t' = mkTAp14 p tf at bt ct dt et ft gt ht it jt kt lt mt nt sr
         in  eagerSat (pap13 sr p t' (rf t' a) b c d e f g h i j k l m n) t'



{-
Assure that a trace component of wrapped value exists by construction of Sat.
Directly used for Cafs
and used for fun_n, prim_n.
-}
lazySat :: R a -> Trace -> R a

lazySat x t = 
  let sat = mkTSatA t
  in mkR (mkTSatB sat `Prelude.seq` -- mark entering of evaluation
          case x of -- create trace for (unevaluated x/v)
            R v vt ->
              v `Prelude.seq` -- evaluate v and thus extend trace for v
              mkTSatC sat vt `Prelude.seq` -- set trace for evaluated v
              v) -- return value
       sat


lazySatLonely :: R a -> Trace -> R a

lazySatLonely x t = 
  let sat = mkTSatALonely t
  in mkR (mkTSatBLonely sat `Prelude.seq` -- mark entering of evaluation
          case x of -- create trace for (unevaluated x/v)
            R v vt ->
              v `Prelude.seq` -- evaluate v and thus extend trace for v
              mkTSatCLonely sat vt `Prelude.seq` -- set trace for evaluated v
              v) -- return value
       sat


{-
Creates a Sat but not for the purpose of assuring that the trace component
for a possibly unevaluated expression exists.
The given wrapped expression is evaluated before it is returned.
Thus this combinator avoids chains of SatCs.
However, it doesn't seem to improve runtime speed as I hoped.
-}
eagerSat :: R a -> Trace -> R a

eagerSat x t =
  let sat = mkTSatA t
  in sat `Prelude.seq` mkTSatB sat `Prelude.seq`
     case x of
       R v vt ->
         v `Prelude.seq`
         mkTSatC sat vt `Prelude.seq`
         x


-- The following combinator is currently not used.
-- It should be used to not to loose information about pattern bindings.
-- (in transformation of DeclPat).

{- Add name of pattern as an indirection -}
patvar :: NmType -> R a -> SR -> Trace -> R a

patvar nm (R v vt) sr t = 
  mkR v (mkTInd (mkTNm t nm sr) vt)


{- Combinators for transforming n-ary functions. -}

{- use caf instead
fun0 :: NmType -> (Trace -> R r) -> SR -> Trace -> R r

fun0 nm rf sr t = 
  let t' = mkTNm t nm sr
  in t' `Prelude.seq` lazySat (rf t') t'  -- t here correct?
-}

fun1 :: NmType -> (Trace -> R a -> R r) -> SR -> Trace 
     -> R (Trace -> R a -> R r)

fun1 nm rf sr t = 
  mkR (\t a -> rf t a)
      (mkTNm t nm sr)

fun2 nm rf sr t = 
  mkR (\t a ->
      R (\t b -> rf t a b)
        t)
      (mkTNm t nm sr)

fun3 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c -> rf t a b c)
          t)
        t)
      (mkTNm t nm sr)

fun4 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d -> rf t a b c d)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun5 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e -> rf t a b c d e)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun6 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f -> rf t a b c d e f)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun7 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g -> rf t a b c d e f g)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun8 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h -> rf t a b c d e f g h)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun9 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i -> rf t a b c d e f g h i)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun10 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j -> rf t a b c d e f g h i j)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun11 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j ->
                        R (\t k -> rf t a b c d e f g h i j k)
                          t)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun12 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j ->
                        R (\t k ->
                          R (\t l -> rf t a b c d e f g h i j k l)
                            t)
                          t)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun13 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j ->
                        R (\t k ->
                          R (\t l ->
                            R (\t m -> rf t a b c d e f g h i j k l m)
                              t)
                            t)
                          t)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun14 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j ->
                        R (\t k ->
                          R (\t l ->
                            R (\t m ->
                              R (\t n -> rf t a b c d e f g h i j k l m n)
                                t)
                              t)
                            t)
                          t)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun15 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e ->
              R (\t f ->
                R (\t g ->
                  R (\t h ->
                    R (\t i ->
                      R (\t j ->
                        R (\t k ->
                          R (\t l ->
                            R (\t m ->
                              R (\t n ->
                                R (\t o -> rf t a b c d e f g h i j k l m n o)
                                  t)
                                t)
                              t)
                            t)
                          t)
                        t)
                      t)
                    t)
                  t)
                t)
              t)
            t)
          t)
        t)
      (mkTNm t nm sr)


{- For lambda-bound variables in projective context. -}
indir :: Trace -> R a -> R a
indir t (R v t') = mkR v (mkTInd t t')


{- Combinators for literals in expressions. -}
conInt :: SR -> Trace -> Int -> R Int
conInt sr t n = mkR n (mkTNm t (mkNTInt n) sr)

conChar :: SR -> Trace -> Char -> R Char
conChar sr t c = mkR c (mkTNm t (mkNTChar c) sr)

conInteger :: SR -> Trace -> Integer -> R Integer
conInteger sr t b = mkR b (mkTNm t (mkNTInteger b) sr)

conFloat :: SR -> Trace -> Float -> R Float
conFloat sr t b = mkR b (mkTNm t (mkNTFloat b) sr)

conDouble :: SR -> Trace -> Double -> R Double
conDouble sr t b = mkR b (mkTNm t (mkNTDouble b) sr)

conRational :: SR -> Trace -> Rational -> R Rational
conRational sr t b =  mkR b (mkTNm t (mkNTRational b) sr)


conCons :: SR -> Trace -> (R Char -> R [Char] -> [Char]) -> Trace 
        -> Char -> R [Char] -> R [Char]
conCons sr t con tnm c b@(R _ bt) =
  let at = mkTNm t (mkNTChar c) sr
  in mkR (con (R c at) b) (mkTAp2 t tnm at bt sr)


{- Combinators for saturated n-ary applications of data constructors. -}

-- suspected:

con0 :: SR -> Trace -> r -> NmType -> R r
con0 sr t cn nm =
  mkR cn (mkTNm t nm sr)

con1 :: SR -> Trace -> (R a -> r) -> NmType -> R a -> R r
con1 sr t cn nm a@(R _ at) =
  mkR (cn a) (mkTAp1 t (mkTNm t nm sr) at sr)

con2 sr t cn nm a@(R _ at) b@(R _ bt) =
  mkR (cn a b) (mkTAp2 t (mkTNm t nm sr) at bt sr)

con3 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) =
  mkR (cn a b c) (mkTAp3 t (mkTNm t nm sr) at bt ct sr)

con4 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
  mkR (cn a b c d) (mkTAp4 t (mkTNm t nm sr) at bt ct dt sr)

con5 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) =
  mkR (cn a b c d e) (mkTAp5 t (mkTNm t nm sr) at bt ct dt et sr)

con6 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) =
  mkR (cn a b c d e f) (mkTAp6 t (mkTNm t nm sr) at bt ct dt et ft sr)

con7 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) =
  mkR (cn a b c d e f g) (mkTAp7 t (mkTNm t nm sr) at bt ct dt et ft gt sr)

con8 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) h@(R _ ht) =
  mkR (cn a b c d e f g h) 
    (mkTAp8 t (mkTNm t nm sr) at bt ct dt et ft gt ht sr)

con9 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) =
  mkR (cn a b c d e f g h i) 
    (mkTAp9 t (mkTNm t nm sr) at bt ct dt et ft gt ht it sr)

con10 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) =
  mkR (cn a b c d e f g h i j)
    (mkTAp10 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt sr)

con11 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) =
  mkR (cn a b c d e f g h i j k)
    (mkTAp11 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt sr)

con12 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) =
  mkR (cn a b c d e f g h i j k l)
    (mkTAp12 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt sr)

con13 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) =
  mkR (cn a b c d e f g h i j k l m)
    (mkTAp13 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt sr)

con14 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) =
  mkR (cn a b c d e f g h i j k l m n)
    (mkTAp14 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt nt sr)

con15 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) o@(R _ ot) =
  mkR (cn a b c d e f g h i j k l m n o)
    (mkTAp15 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt nt ot sr)



{-
Combinators for calling foreign functions:   prim_n
Definitions identical to those of fun_n, except for the use of primEnter
instead of enter and that the unwrapped arguments are passed.
(Maybe common abstraction should be defined?)
-}

prim0 :: NmCoerce r => NmType -> r -> SR -> Trace -> R r

prim0 nm rf sr t = 
  let tf = mkTNm t nm sr
  in primEnter sr tf rf  -- primEnter strict in tf

prim1 :: NmCoerce r => NmType -> (a -> r) -> SR -> Trace -> R (Fun a r)

prim1 nm rf sr t = 
  mkR (\t (R a at) -> primEnter sr t (rf a))
    (mkTNm t nm sr)


prim2 :: NmCoerce r => 
         NmType -> (a -> b -> r) -> SR -> Trace -> R (Fun a (Fun b r))

prim2 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)-> primEnter sr t (rf a b))
      t)
    (mkTNm t nm sr)

prim3 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)-> primEnter sr t (rf a b c))
        t)
      t)
    (mkTNm t nm sr)

prim4 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)-> primEnter sr t (rf a b c d))
          t)
        t)
      t)
    (mkTNm t nm sr)


prim5 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)->
          R (\t (R e et)-> primEnter sr t (rf a b c d e))
            t)
          t)
        t)
      t)
    (mkTNm t nm sr)

prim6 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)->
          R (\t (R e et)->
            R (\t (R f ft)-> primEnter sr t (rf a b c d e f))
              t)
            t)
          t)
        t)
      t)
    (mkTNm t nm sr)

prim7 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)->
          R (\t (R e et)->
            R (\t (R f ft)->
              R (\t (R g gt)-> 
                  primEnter sr t (rf a b c d e f g))
                t)
              t)
            t)
          t)
        t)
      t)
    (mkTNm t nm sr)

prim8 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)->
          R (\t (R e et)->
            R (\t (R f ft)->
              R (\t (R g gt)->
                R (\t (R h ht)-> 
                    primEnter sr t (rf a b c d e f g h))
                  t)
                t)
              t)
            t)
          t)
        t)
      t)
    (mkTNm t nm sr)

prim9 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                      primEnter sr t9 (rf a b c d e f g h i))
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim10 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                        primEnter sr t10 (rf a b c d e f 
                                                         g h i j))
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim11 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                      R (\t11 (R k kt)->
                          primEnter sr t11 (rf a b c d e f 
                                                           g h i j k))
                        t10)
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim12 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                      R (\t11 (R k kt)->
                        R (\t12 (R l lt)->
                            primEnter sr t12 (rf a b c d e f 
                                                             g h i j k l))
                          t11)
                        t10)
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim13 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                      R (\t11 (R k kt)->
                        R (\t12 (R l lt)->
                          R (\t13 (R m mt)->
                              primEnter sr t13 (rf a b c d e f 
                                                         g h i j k l m))
                            t12)
                          t11)
                        t10)
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim14 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                      R (\t11 (R k kt)->
                        R (\t12 (R l lt)->
                          R (\t13 (R m mt)->
                            R (\t14 (R n nt)->
                                primEnter sr t14 (rf a b c d e f 
                                                         g h i j k l m n))
                              t13)
                            t12)
                          t11)
                        t10)
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)

prim15 nm rf sr t = 
  mkR (\t1 (R a at)->
    R (\t2 (R b bt)->
      R (\t3 (R c ct)->
        R (\t4 (R d dt)->
          R (\t5 (R e et)->
            R (\t6 (R f ft)->
              R (\t7 (R g gt)->
                R (\t8 (R h ht)->
                  R (\t9 (R i it)->
                    R (\t10 (R j jt)->
                      R (\t11 (R k kt)->
                        R (\t12 (R l lt)->
                          R (\t13 (R m mt)->
                            R (\t14 (R n nt)->
                              R (\t15 (R o ot)->
                                  primEnter sr t15 (rf a b c d e f 
                                                         g h i j k l m n o))
                                t14)
                              t13)
                            t12)
                          t11)
                        t10)
                      t9)
                    t8)
                  t7)
                t6)
              t5)
            t4)
          t3)
        t2)
      t1)
    (mkTNm t nm sr)




{-
cni
Transform a function into a wrapped function with the given trace.
Used for partially applied constructors
-}

cn1 :: (R a1 -> b) -> Trace -> R (Fun a1 b)
cn1 rf t = R (\t a ->
             R (rf a)
               t)
             t

cn2 :: (R a1 -> R a2 -> b) -> Trace -> R (Fun a1 (Fun a2 b))
cn2 rf t = R (\t a ->
             R (\t b ->
               R (rf a b)
                 t)
               t)
             t

cn3 :: (R a1 -> R a2 -> R a3 -> b) -> Trace -> R (Fun a1 (Fun a2 (Fun a3 b)))
cn3 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (rf a b c)
                   t)t)t)t

cn4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> Trace 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 b))))
cn4 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (\t d ->
                   R (rf a b c d)
                     t)t)t)t)t

cn5 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (\t d ->
                   R (\t e ->
                     R (rf a b c d e)
                       t)t)t)t)t)t

cn6 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (\t d ->
                   R (\t e ->
                     R (\t f ->
                       R (rf a b c d e f)
                         t)t)t)t)t)t)t

cn7 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (\t d ->
                   R (\t e ->
                     R (\t f ->
                       R (\t g ->
                         R (rf a b c d e f g)
                           t)t)t)t)t)t)t)t

cn8 rf t = R (\t a ->
             R (\t b ->
               R (\t c ->
                 R (\t d ->
                   R (\t e ->
                     R (\t f ->
                       R (\t g ->
                         R (\t h ->
                           R (rf a b c d e f g h)
                             t)t)t)t)t)t)t)t)t


{- 
pai 
Create application node for function that is partially applied to i
arguments and transform partial application with given function.
Used for partially applied data constructors.
-}

-- suspected:

pa0 :: b -> (b -> Trace -> c) -> SR -> Trace -> NmType -> c

pa0 c cni sr t nm =
  cni c Prelude.$! (mkTNm t nm sr) 


pa1 :: (R a1 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType -> R a1 -> c

pa1 c cni sr t nm a1@(R _ t1) =
  cni (c a1) Prelude.$! mkTAp1 t (mkTNm t nm sr) t1 sr


pa2 :: (R a1 -> R a2 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> c

pa2 c cni sr t nm a1@(R _ t1) a2@(R _ t2) =
  cni (c a1 a2) Prelude.$! mkTAp2 t (mkTNm t nm sr) t1 t2 sr


pa3 :: (R a1 -> R a2 -> R a3 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> R a3 -> c

pa3 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) =
  cni (c a1 a2 a3) Prelude.$! mkTAp3 t (mkTNm t nm sr) t1 t2 t3 sr


pa4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace  -> NmType
    -> R a1 -> R a2 -> R a3 -> R a4 -> c

pa4 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) a4@(R _ t4) =
  cni (c a1 a2 a3 a4) Prelude.$! mkTAp4 t (mkTNm t nm sr) t1 t2 t3 t4 sr


-- ----------------------------------------------------------------------------
-- part from HatArchive

-- foreign import "primSameTrace" sameAs :: Trace -> Trace -> Bool

sameAs :: Trace -> Trace -> Bool
(Trace fileptr1) `sameAs` (Trace fileptr2) = fileptr1 == fileptr2


-- new combinators for portable transformation:

mkNoSourceRef :: SR
mkNoSourceRef = SR 0

foreign import "primSourceRef"
  mkSourceRef :: ModuleTraceInfo -> Int -> SR

mkAtomCon :: ModuleTraceInfo -> Int -> Int -> String -> NmType
mkAtomCon mti pos fixPri unqual = mkAtomCon' mti pos fixPri (packString unqual)

foreign import "primAtomCon"
  mkAtomCon' :: ModuleTraceInfo -> Int -> Int -> PackedString -> NmType

mkAtomId :: ModuleTraceInfo -> Int -> Int -> String -> NmType
mkAtomId mti pos fixPri unqual = mkAtomId' mti pos fixPri (packString unqual)

foreign import "primAtomId"
  mkAtomId' :: ModuleTraceInfo -> Pos -> Int -> PackedString -> NmType

mkAtomIdToplevel :: ModuleTraceInfo -> Pos -> Int -> String -> NmType
mkAtomIdToplevel mti pos fixPri unqual = 
  mkAtomIdToplevel' mti pos fixPri (packString unqual)

foreign import "primAtomIdToplevel"
  mkAtomIdToplevel' :: ModuleTraceInfo -> Pos -> Int -> PackedString -> NmType

mkModule :: String -> String -> ModuleTraceInfo
mkModule unqual filename = mkModule' (packString unqual) (packString filename)

foreign import "primModule"
  mkModule' :: PackedString -> PackedString -> ModuleTraceInfo

outputTrace :: Trace -> String -> IO ()
outputTrace trace output = outputTrace' trace (packString output)

foreign import "outputTrace"
  outputTrace' :: Trace -> PackedString -> IO ()


----
-- Trace constructors
----


foreign import "primTRoot"
 mkTRoot :: Trace

foreign import "primTAp1"
 mkTAp1 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp2"
 mkTAp2 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp3"
 mkTAp3 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp4"
 mkTAp4 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp5"
 mkTAp5 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp6"
 mkTAp6 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp7"
 mkTAp7 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp8"
 mkTAp8 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp9"
 mkTAp9 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp10"
 mkTAp10 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp11"
 mkTAp11 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp12"
 mkTAp12 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> Trace	-- arg 12
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp13"
 mkTAp13 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> Trace	-- arg 12
	-> Trace	-- arg 13
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp14"
 mkTAp14 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> Trace	-- arg 12
	-> Trace	-- arg 13
	-> Trace	-- arg 14
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTAp15"
 mkTAp15 :: Trace		-- trace of application
	-> Trace	-- fn
	-> Trace	-- arg 1
	-> Trace	-- arg 2
	-> Trace	-- arg 3
	-> Trace	-- arg 4
	-> Trace	-- arg 5
	-> Trace	-- arg 6
	-> Trace	-- arg 7
	-> Trace	-- arg 8
	-> Trace	-- arg 9
	-> Trace	-- arg 10
	-> Trace	-- arg 11
	-> Trace	-- arg 12
	-> Trace	-- arg 13
	-> Trace	-- arg 14
	-> Trace	-- arg 15
	-> SR		-- src ref
	-> Trace	-- result


foreign import "primTNm"
 mkTNm :: Trace		-- trace of Nm
	-> NmType	-- NmType
	-> SR		-- src ref
	-> Trace	-- result

foreign import "primTInd"
 mkTInd :: Trace		-- trace 1
	-> Trace	-- trace 2
	-> Trace	-- result

foreign import "primTHidden"
 mkTHidden :: Trace	-- trace
	-> Trace	-- result

foreign import "primTSatA"
 mkTSatA :: Trace	-- trace of unevaluated expr
	-> Trace	-- result

foreign import "primTSatB"
 mkTSatB :: Trace	-- original SatA
	-> Trace	-- result

foreign import "primTSatC"
 mkTSatC :: Trace	-- original SatB (or SatA)
	-> Trace	-- trace of reduced value
	-> Trace	-- result

foreign import "primTSatALonely"
 mkTSatALonely :: Trace	-- trace of unevaluated expr
	-> Trace	-- result

foreign import "primTSatBLonely"
 mkTSatBLonely :: Trace	-- original SatA
	-> Trace	-- result

foreign import "primTSatCLonely"
 mkTSatCLonely :: Trace	-- original SatB (or SatA)
	-> Trace	-- trace of reduced value
	-> Trace	-- result


----
-- NmType constructors
----
foreign import "primNTInt"
 mkNTInt	:: Int -> NmType
foreign import "primNTChar"
 mkNTChar	:: Char -> NmType
mkNTInteger :: Integer -> NmType
mkNTInteger i = mkNTInteger' ((Prelude.fromInteger i)::Int)
foreign import "primNTInteger"
 mkNTInteger'	:: Int -> NmType
mkNTRational :: Rational -> NmType
mkNTRational r = 
  mkNTRational' ((Prelude.fromInteger (Ratio.numerator r))::Int) 
                ((Prelude.fromInteger (Ratio.denominator r))::Int)
foreign import "primNTRational"
 mkNTRational' :: Int -> Int -> NmType
foreign import "primNTFloat"
 mkNTFloat	:: Float -> NmType
foreign import "primNTDouble"
 mkNTDouble	:: Double -> NmType
foreign import "primNTTuple"
 mkNTTuple	:: NmType
foreign import "primNTFun"
 mkNTFun	:: NmType
foreign import "primNTCase"
 mkNTCase	:: NmType
foreign import "primNTLambda"
 mkNTLambda	:: NmType
foreign import "primNTDummy"
 mkNTDummy	:: NmType
foreign import "primNTCString"
 mkNTCString	:: PackedString -> NmType
foreign import "primNTIf"
 mkNTIf		:: NmType
foreign import "primNTGuard"
 mkNTGuard	:: NmType
foreign import "primNTContainer"
 mkNTContainer	:: NmType

-- ----------------------------------------------------------------------------
-- End