-- ----------------------------------------------------------------------------
-- library used by all programs transformed for tracing by Hat
-- combinators in Haskell and interfaces to C-functions

module Hat 
  (R(R),mkR,Fun(Fun),SR,Trace,NmType,ModuleTraceInfo
  ,tPrelude,Tuple0(Tuple0),Tuple2(Tuple2),aTuple0,aTuple2
  ,List(Cons,List),aCons,aList
  ,NmCoerce(toNm)
  ,ap1,ap2,ap3,ap4,ap5,ap6,ap7,ap8,ap9,ap10,ap11,ap12,ap13,ap14,ap15
--  ,rap1,rap2,rap3,rap4,rap5,rap6,rap7,rap8,rap9,rap10,rap11,rap12,rap13
--  ,rap14,rap15
--  ,pap0,pap1,pap2,pap3,pap4,pap5,pap6,pap7,pap8,pap9,pap10,pap11,pap12
--  ,pap13,pap14
  ,lazySat,lazySatLonely,eagerSat
  ,fun1,fun2,fun3,fun4,fun5,fun6,fun7,fun8,fun9,fun10,fun11,fun12,fun13
  ,fun14,fun15
  ,ulazySat,hiddenRoot
  ,uap1,uap2,uap3,uap4
  ,ufun1,ufun2,ufun3
  ,indir
  ,fromConInteger,fromConRational
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
  ,unsafeIOTrace 
  ) where

import Ratio (numerator,denominator)
import IOExtras (unsafePerformIO,IORef,newIORef,readIORef,writeIORef)
import Char (ord,chr,intToDigit)

import FFI (Ptr(..),unsafePerformIO,CString,withCString)	-- PORTABLE
--import PackedString (PackedString,packString) -- NONPORTABLE
  -- import MagicTypes (NmType,CStructure) -- NONPORTABLE
    -- magic C-type living in Haskell heap

useString :: (CString -> a) -> (String -> a)
f `useString` s = unsafePerformIO (
                      withCString s (\s'-> let z = f s' in z `seq` return z))

type Pos = Int
noPos = 0

-- ----------------------------------------------------------------------------

openTrace :: String -> IO ()
openTrace progname = withCString progname openTrace'

foreign import "openTrace"
  openTrace' :: CString -> IO () 

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

trace :: R a -> Trace
trace (R a t) = t



{- data constructor R strict in trace argument -}
mkR :: a -> Trace -> R a
mkR x t = t `Prelude.seq` R x t



newtype SR     = SR Int
newtype Trace  = Trace Int -- a hidden trace has a set bit
newtype NmType = NmType Int
newtype ModuleTraceInfo = MTI Int

-- for debugging
unsafeIOTrace :: String -> Trace -> ()
unsafeIOTrace t (Trace i) = 
  unsafePerformIO (putStr (t ++ " -> " ++ showHex i ++ "\n"))

showHex :: Int -> String
showHex = ("0x"++) . map (intToDigit . (`mod` 16)) . reverse . takeWhile (>0) 
          . iterate (`div` 16)

-- ----------------------------------------------------------------------------
-- transformed primitive types (undefinable and with special syntax)

-- module name:
tPrelude = mkModule "Prelude" "Prelude.hs" False

newtype Fun a b = Fun (Trace -> R a -> R b)

-- type constructors and data constructors need to have same name,
-- because transformation doesn't distinguish the two
data Tuple0 = Tuple0  -- () would do, but this way like other tuples  
data Tuple2 a b = Tuple2 (R a) (R b) -- not type Tuple2 a b = (R a,R b)
aTuple0 = mkAtomCon tPrelude 0 3 "()"
aTuple2 = mkAtomCon tPrelude 0 3 "(,)" 

data List a = Cons (R a) (R (List a)) | List  
  -- type constructor and empty list constructor need to have same name,
  -- because transformation doesn't distinguish the two
aCons = mkAtomCon tPrelude 0 21 ":"
aList = mkAtomCon tPrelude 0 3 "[]"

-- ----------------------------------------------------------------------------
-- combinators for n-ary application in a non-projective context.

ap1 :: SR -> Trace -> R (Fun a r) -> R a -> R r 

ap1 sr t (R (Fun rf) tf) a@(R _ at) = 
  let t' = mkTAp1 t tf at sr
  in  lazySat (rf t' a) t'

ap2 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) = 
  let t' = mkTAp2 t tf at bt sr
  in  lazySat (pap1 sr t t' (rf t' a) b) t'

ap3 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t' = mkTAp3 t tf at bt ct sr
  in  lazySat (pap2 sr t t' (rf t' a) b c) t'


ap4 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t' = mkTAp4 t tf at bt ct dt sr
  in  lazySat (pap3 sr t t' (rf t' a) b c d) t'


ap5 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                         e@(R _ et) = 
  let t' = mkTAp5 t tf at bt ct dt et sr
  in  lazySat (pap4 sr t t' (rf t' a) b c d e) t'

ap6 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                         e@(R _ et) f@(R _ ft) = 
  let t' = mkTAp6 t tf at bt ct dt et ft sr
  in  lazySat (pap5 sr t t' (rf t' a) b c d e f) t'

ap7 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                         e@(R _ et) f@(R _ ft) g@(R _ gt) = 
  let t' = mkTAp7 t tf at bt ct dt et ft gt sr
  in  lazySat (pap6 sr t t' (rf t' a) b c d e f g) t'

ap8 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                         e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
  let t' = mkTAp8 t tf at bt ct dt et ft gt ht sr
  in  lazySat (pap7 sr t t' (rf t' a) b c d e f g h) t'

ap9 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                         e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                         i@(R _ it) = 
  let t' = mkTAp9 t tf at bt ct dt et ft gt ht it sr
  in  lazySat (pap8 sr t t' (rf t' a) b c d e f g h i) t'


ap10 :: SR -> Trace 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i (Fun j r)))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R r 

ap10 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) = 
  let t' = mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
  in  lazySat (pap9 sr t t' (rf t' a) b c d e f g h i j) t'

ap11 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) k@(R _ kt) = 
  let t' = mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
  in  lazySat (pap10 sr t t' (rf t' a) b c d e f g h i j k) t'

ap12 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) =  
  let t' = mkTAp12 t tf at bt ct dt et ft gt ht it jt kt lt sr
  in  lazySat (pap11 sr t t' (rf t' a) b c d e f g h i j k l) t'

ap13 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) 
                          m@(R _ mt) =  
  let t' = mkTAp13 t tf at bt ct dt et ft gt ht it jt kt lt mt sr
  in  lazySat (pap12 sr t t' (rf t' a) b c d e f g h i j k l m) t'

ap14 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) 
                          m@(R _ mt) n@(R _ nt) =  
  let t' = mkTAp14 t tf at bt ct dt et ft gt ht it jt kt lt mt nt sr
  in  lazySat (pap13 sr t t' (rf t' a) b c d e f g h i j k l m n) t'

ap15 sr t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                          e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                          i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) 
                          m@(R _ mt) n@(R _ nt) o@(R _ ot) =  
  let t' = mkTAp15 t tf at bt ct dt et ft gt ht it jt kt lt mt nt ot sr
  in  lazySat (pap14 sr t t' (rf t' a) b c d e f g h i j k l m n o) t'



{- 
Combinators for n-ary applications in a projective context.
Is more efficient than ap?, because of the following optimisation:
No Sat is needed, because when the application is needed in the trace,
the application also needs to be evaluated.
However, the application is only linked with the trace after whnf is
reached. If the computation is aborted in between, there is no link
and hence no chance for hat-detect to locate all children.
So currently don't use this optimisation.

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
-}

-- Combinators for n-ary application used by the combinators above.
-- Introduces a new application node if the function is a saturated
-- application.

pap0 :: Trace -> R r -> R r

pap0 t e = e


pap1 :: SR -> Trace -> Trace -> R (Fun a r) -> R a -> R r

pap1 sr p t (R (Fun rf) tf) a =
  if t `sameAs` tf 
    then rf t a
    else case a of 
           R _ at ->
             let t' = mkTAp1 p tf at sr
             in  t' `Prelude.seq` eagerSat (rf t' a) t'

pap2 sr p t (R (Fun rf) tf) a b =
  if t `sameAs` tf 
    then pap1 sr p t (rf t a) b
    else case a of 
           R _ at -> case b of 
             R _ bt ->
               let t' = mkTAp2 p tf at bt sr
               in  eagerSat (pap1 sr p t' (rf t' a) b) t'


pap3 sr p t (R (Fun rf) tf) a b c =
  if t `sameAs` tf 
    then pap2 sr p t (rf t a) b c
    else case a of 
           R _ at -> case b of 
             R _ bt -> case c of 
               R _ ct -> 
                 let t' = mkTAp3 p tf at bt ct sr
                 in eagerSat (pap2 sr p t' (rf t' a) b c) t'


pap4 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
  if t `sameAs` tf 
    then pap3 sr p t (rf t a) b c d
    else let t' = mkTAp4 p tf at bt ct dt sr
         in  eagerSat (pap3 sr p t' (rf t' a) b c d) t'

pap5 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                            e@(R _ et) =
  if t `sameAs` tf 
    then pap4 sr p t (rf t a) b c d e
    else let t' = mkTAp5 p tf at bt ct dt et sr
         in  eagerSat (pap4 sr p t' (rf t' a) b c d e) t'

pap6 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                            e@(R _ et) f@(R _ ft) =
  if t `sameAs` tf 
    then pap5 sr p t (rf t a) b c d e f
    else let t' = mkTAp6 p tf at bt ct dt et ft sr
         in  eagerSat (pap5 sr p t' (rf t' a) b c d e f) t'

pap7 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                            e@(R _ et) f@(R _ ft) g@(R _ gt) =
  if t `sameAs` tf 
    then pap6 sr p t (rf t a) b c d e f g
    else let t' = mkTAp7 p tf at bt ct dt et ft gt sr
         in  eagerSat (pap6 sr p t' (rf t' a) b c d e f g) t'

pap8 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                            e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) =
  if t `sameAs` tf 
    then pap7 sr p t (rf t a) b c d e f g h
    else let t' = mkTAp8 p tf at bt ct dt et ft gt ht sr
         in  eagerSat (pap7 sr p t' (rf t' a) b c d e f g h) t'


pap9 :: SR -> Trace -> Trace
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i r))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i 
     -> R r 

pap9 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                            e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                            i@(R _ it) =
  if t `sameAs` tf 
    then pap8 sr p t (rf t a) b c d e f g h i
    else let t' = mkTAp9 p tf at bt ct dt et ft gt ht it sr
         in  eagerSat (pap8 sr p t' (rf t' a) b c d e f g h i) t'

pap10 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                             e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                             i@(R _ it) j@(R _ jt) =
  if t `sameAs` tf 
    then pap9 sr p t (rf t a) b c d e f g h i j
    else let t' = mkTAp10 p tf at bt ct dt et ft gt ht it jt sr
         in eagerSat (pap9 sr p t' (rf t' a) b c d e f g h i j) t'

pap11 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                             e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                             i@(R _ it) j@(R _ jt) k@(R _ kt) =
  if t `sameAs` tf 
    then pap10 sr p t (rf t a) b c d e f g h i j k
    else let t' = mkTAp11 p tf at bt ct dt et ft gt ht it jt kt sr
         in  eagerSat (pap10 sr p t' (rf t' a) b c d e f g h i j k) t'

pap12 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                             e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                             i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) =
  if t `sameAs` tf 
    then pap11 sr p t (rf t a) b c d e f g h i j k l
    else let t' = mkTAp12 p tf at bt ct dt et ft gt ht it jt kt lt sr
         in  eagerSat (pap11 sr p t' (rf t' a) b c d e f g h i j k l) t'

pap13 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                             e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                             i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) 
                             m@(R _ mt) =
  if t `sameAs` tf 
    then pap12 sr p t (rf t a) b c d e f g h i j k l m
    else let t' = mkTAp13 p tf at bt ct dt et ft gt ht it jt kt lt mt sr
         in  eagerSat (pap12 sr p t' (rf t' a) b c d e f g h i j k l m) t'

pap14 sr p t (R (Fun rf) tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) 
                             e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) 
                             i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) 
                             m@(R _ mt) n@(R _ nt) =
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


-- specially marked Sat that doesn't belong to the preceding trace node
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
     -> R (Fun a r)
fun1 nm rf sr t = 
  let tf = mkTNm t nm sr
  in R (Fun (\t a -> 
               if hidden t
                 then (rf $! (mkTAp1 t tf (trace a) mkNoSourceRef)) a 
                 else rf t a))
       tf

fun2 :: NmType -> (Trace -> R a -> R b -> R r) -> SR -> Trace
     -> R (Fun a (Fun b r))
fun2 nm rf sr t = 
  let tf = mkTNm t nm sr
  in R (Fun (\t a ->
       R (Fun (\t b -> 
                 if hidden t 
                   then (rf $! (mkTAp2 t tf (trace a) (trace b) 
                                            mkNoSourceRef)) a b
                   else rf t a b))
         t))
       tf

fun3 nm rf sr t =
  let tf = mkTNm t nm sr
  in R (Fun (\t a ->
       R (Fun (\t b ->
         R (Fun (\t c -> 
                   if hidden t
                     then (rf $! (mkTAp3 t tf (trace a) (trace b) 
                                              (trace c) mkNoSourceRef)) a b c
                     else rf t a b c))
           t))
         t))
       tf

{-
fun1 :: NmType -> (Trace -> R a -> R r) -> SR -> Trace 
     -> R (Fun a r)
fun1 nm rf sr t = 
  mkR (Fun (\t a -> rf t a))
      (mkTNm t nm sr)

fun2 :: NmType -> (Trace -> R a -> R b -> R r) -> SR -> Trace
     -> R (Fun a (Fun b r))
fun2 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b -> rf t a b))
        t))
      (mkTNm t nm sr)

fun3 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c -> rf t a b c))
          t))
        t))
      (mkTNm t nm sr)
-}
fun4 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d -> rf t a b c d))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun5 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e -> rf t a b c d e))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun6 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f -> rf t a b c d e f))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun7 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g -> rf t a b c d e f g))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun8 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h -> rf t a b c d e f g h))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun9 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i -> rf t a b c d e f g h i))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun10 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j -> rf t a b c d e f g h i j))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun11 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j ->
                        R (Fun (\t k -> rf t a b c d e f g h i j k))
                          t))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun12 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j ->
                        R (Fun (\t k ->
                          R (Fun (\t l -> rf t a b c d e f g h i j k l))
                            t))
                          t))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun13 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j ->
                        R (Fun (\t k ->
                          R (Fun (\t l ->
                            R (Fun (\t m -> rf t a b c d e f g h i j k l m))
                              t))
                            t))
                          t))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun14 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j ->
                        R (Fun (\t k ->
                          R (Fun (\t l ->
                            R (Fun (\t m ->
                              R (Fun (\t n -> rf t a b c d e f g h i j k l m n))
                                t))
                              t))
                            t))
                          t))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)

fun15 nm rf sr t = 
  mkR (Fun (\t a ->
      R (Fun (\t b ->
        R (Fun (\t c ->
          R (Fun (\t d ->
            R (Fun (\t e ->
              R (Fun (\t f ->
                R (Fun (\t g ->
                  R (Fun (\t h ->
                    R (Fun (\t i ->
                      R (Fun (\t j ->
                        R (Fun (\t k ->
                          R (Fun (\t l ->
                            R (Fun (\t m ->
                              R (Fun (\t n ->
                                R (Fun (\t o -> rf t a b c d e f g h i j k l m n o))
                                  t))
                                t))
                              t))
                            t))
                          t))
                        t))
                      t))
                    t))
                  t))
                t))
              t))
            t))
          t))
        t))
      (mkTNm t nm sr)


{- For lambda-bound variables in projective context. -}
indir :: Trace -> R a -> R a
indir t (R v t') = R v (mkTInd t t')


{- Combinators for literals in expressions. -}
fromConInteger :: Prelude.Num a => SR -> Trace -> Integer -> R a
fromConInteger sr t i = 
  R (Prelude.fromInteger i) (mkTNm t (mkNTInteger i) sr)

fromConRational :: Prelude.Fractional a => SR -> Trace -> Rational -> R a
fromConRational sr t r = 
  R (Prelude.fromRational r) (mkTNm t (mkNTRational r) sr)

conInt :: SR -> Trace -> Int -> R Int
conInt sr t n = R n (mkTNm t (mkNTInt n) sr)

conChar :: SR -> Trace -> Char -> R Char
conChar sr t c = R c (mkTNm t (mkNTChar c) sr)

conInteger :: SR -> Trace -> Integer -> R Integer
conInteger sr t b = R b (mkTNm t (mkNTInteger b) sr)

conFloat :: SR -> Trace -> Float -> R Float
conFloat sr t b = R b (mkTNm t (mkNTFloat b) sr)

conDouble :: SR -> Trace -> Double -> R Double
conDouble sr t b = R b (mkTNm t (mkNTDouble b) sr)

conRational :: SR -> Trace -> Rational -> R Rational
conRational sr t b =  R b (mkTNm t (mkNTRational b) sr)


conCons :: SR -> Trace -> (R Char -> R [Char] -> [Char]) -> Trace 
        -> Char -> R [Char] -> R [Char]
conCons sr t con tnm c b@(R _ bt) =
  let at = mkTNm t (mkNTChar c) sr
  in R (con (R c at) b) (mkTAp2 t tnm at bt sr)


{- Combinators for saturated n-ary applications of data constructors. -}

-- suspected:

con0 :: SR -> Trace -> r -> NmType -> R r
con0 sr t cn nm =
  R cn (mkTNm t nm sr)

con1 :: SR -> Trace -> (R a -> r) -> NmType -> R a -> R r
con1 sr t cn nm a@(R _ at) =
  R (cn a) (mkTAp1 t (mkTNm t nm sr) at sr)

con2 sr t cn nm a@(R _ at) b@(R _ bt) =
  R (cn a b) (mkTAp2 t (mkTNm t nm sr) at bt sr)

con3 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) =
  R (cn a b c) (mkTAp3 t (mkTNm t nm sr) at bt ct sr)

con4 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
  R (cn a b c d) (mkTAp4 t (mkTNm t nm sr) at bt ct dt sr)

con5 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) =
  R (cn a b c d e) (mkTAp5 t (mkTNm t nm sr) at bt ct dt et sr)

con6 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) =
  R (cn a b c d e f) (mkTAp6 t (mkTNm t nm sr) at bt ct dt et ft sr)

con7 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) =
  R (cn a b c d e f g) (mkTAp7 t (mkTNm t nm sr) at bt ct dt et ft gt sr)

con8 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) h@(R _ ht) =
  R (cn a b c d e f g h) 
    (mkTAp8 t (mkTNm t nm sr) at bt ct dt et ft gt ht sr)

con9 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) =
  R (cn a b c d e f g h i) 
    (mkTAp9 t (mkTNm t nm sr) at bt ct dt et ft gt ht it sr)

con10 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) =
  R (cn a b c d e f g h i j)
    (mkTAp10 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt sr)

con11 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) =
  R (cn a b c d e f g h i j k)
    (mkTAp11 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt sr)

con12 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) =
  R (cn a b c d e f g h i j k l)
    (mkTAp12 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt sr)

con13 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) =
  R (cn a b c d e f g h i j k l m)
    (mkTAp13 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt sr)

con14 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) =
  R (cn a b c d e f g h i j k l m n)
    (mkTAp14 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt nt sr)

con15 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                 f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                 k@(R _ kt) l@(R _ lt) m@(R _ mt) n@(R _ nt) o@(R _ ot) =
  R (cn a b c d e f g h i j k l m n o)
    (mkTAp15 t (mkTNm t nm sr) at bt ct dt et ft gt ht it jt kt lt mt nt ot sr)





{-
cni
Transform a function into a wrapped function with the given trace.
Used for partially applied constructors
-}

cn1 :: (R a1 -> b) -> Trace -> R (Fun a1 b)
cn1 rf t = R (Fun (\t a ->
             R (rf a)
               t))
             t

cn2 :: (R a1 -> R a2 -> b) -> Trace -> R (Fun a1 (Fun a2 b))
cn2 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (rf a b)
                 t))
               t))
             t

cn3 :: (R a1 -> R a2 -> R a3 -> b) -> Trace -> R (Fun a1 (Fun a2 (Fun a3 b)))
cn3 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (rf a b c)
                   t))t))t))t

cn4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> Trace 
    -> R (Fun a1 (Fun a2 (Fun a3 (Fun a4 b))))
cn4 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (Fun (\t d ->
                   R (rf a b c d)
                     t))t))t))t))t

cn5 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (Fun (\t d ->
                   R (Fun (\t e ->
                     R (rf a b c d e)
                       t))t))t))t))t))t

cn6 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (Fun (\t d ->
                   R (Fun (\t e ->
                     R (Fun (\t f ->
                       R (rf a b c d e f)
                         t))t))t))t))t))t))t

cn7 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (Fun (\t d ->
                   R (Fun (\t e ->
                     R (Fun (\t f ->
                       R (Fun (\t g ->
                         R (rf a b c d e f g)
                           t))t))t))t))t))t))t))t

cn8 rf t = R (Fun (\t a ->
             R (Fun (\t b ->
               R (Fun (\t c ->
                 R (Fun (\t d ->
                   R (Fun (\t e ->
                     R (Fun (\t f ->
                       R (Fun (\t g ->
                         R (Fun (\t h ->
                           R (rf a b c d e f g h)
                             t))t))t))t))t))t))t))t))t


{- 
pai 
Create application node for function that is partially applied to i
arguments and transform partial application with given function.
Used for partially applied data constructors.
-}

pa0 :: b -> (b -> Trace -> c) -> SR -> Trace -> NmType -> c

pa0 c cni sr t nm =
  cni c (mkTNm t nm sr) 


pa1 :: (R a1 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType -> R a1 -> c

pa1 c cni sr t nm a1@(R _ t1) =
  cni (c a1) (mkTAp1 t (mkTNm t nm sr) t1 sr)


pa2 :: (R a1 -> R a2 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> c

pa2 c cni sr t nm a1@(R _ t1) a2@(R _ t2) =
  cni (c a1 a2) (mkTAp2 t (mkTNm t nm sr) t1 t2 sr)


pa3 :: (R a1 -> R a2 -> R a3 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> R a3 -> c

pa3 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) =
  cni (c a1 a2 a3) (mkTAp3 t (mkTNm t nm sr) t1 t2 t3 sr)


pa4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace  -> NmType
    -> R a1 -> R a2 -> R a3 -> R a4 -> c

pa4 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) a4@(R _ t4) =
  cni (c a1 a2 a3 a4) (mkTAp4 t (mkTNm t nm sr) t1 t2 t3 t4 sr)


-- ----------------------------------------------------------------------------
-- toNm used to coerce primitive return value from a foreign function
-- into a Trace structure
-- all instances have to be strict in the unwrapped value
-- simplifies transformation; also in n+k patterns don't know exact type
-- relies on transformed type just being a type synonym for untraced type
-- hence will not work in the long run.
class NmCoerce a where
  toNm :: Trace -> a -> SR -> R a


{-
counterpart to 'enter' for primitives: ensures that the result trace
is fully evaluated (the trace contains the result value).
  ** now `enter' has been eliminated, no longer sure about this defn **
-}
primEnter :: NmCoerce a => SR -> Trace -> a -> R a
primEnter sr t e = toNm t e sr

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
  R (Fun (\t (R a at) -> primEnter sr t (rf a)))
    (mkTNm t nm sr)


prim2 :: NmCoerce r => 
         NmType -> (a -> b -> r) -> SR -> Trace -> R (Fun a (Fun b r))

prim2 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)-> primEnter sr t (rf a b)))
      t))
    (mkTNm t nm sr)

prim3 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)-> primEnter sr t (rf a b c)))
        t))
      t))
    (mkTNm t nm sr)

prim4 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)->
        R (Fun (\t (R d dt)-> primEnter sr t (rf a b c d)))
          t))
        t))
      t))
    (mkTNm t nm sr)


prim5 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)->
        R (Fun (\t (R d dt)->
          R (Fun (\t (R e et)-> primEnter sr t (rf a b c d e)))
            t))
          t))
        t))
      t))
    (mkTNm t nm sr)

prim6 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)->
        R (Fun (\t (R d dt)->
          R (Fun (\t (R e et)->
            R (Fun (\t (R f ft)-> primEnter sr t (rf a b c d e f)))
              t))
            t))
          t))
        t))
      t))
    (mkTNm t nm sr)

prim7 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)->
        R (Fun (\t (R d dt)->
          R (Fun (\t (R e et)->
            R (Fun (\t (R f ft)->
              R (Fun (\t (R g gt)-> 
                  primEnter sr t (rf a b c d e f g)))
                t))
              t))
            t))
          t))
        t))
      t))
    (mkTNm t nm sr)

prim8 nm rf sr t = 
  R (Fun (\t (R a at)->
    R (Fun (\t (R b bt)->
      R (Fun (\t (R c ct)->
        R (Fun (\t (R d dt)->
          R (Fun (\t (R e et)->
            R (Fun (\t (R f ft)->
              R (Fun (\t (R g gt)->
                R (Fun (\t (R h ht)-> 
                    primEnter sr t (rf a b c d e f g h)))
                  t))
                t))
              t))
            t))
          t))
        t))
      t))
    (mkTNm t nm sr)

prim9 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                      primEnter sr t9 (rf a b c d e f g h i)))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim10 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                        primEnter sr t10 (rf a b c d e f 
                                                         g h i j)))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim11 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                      R (Fun (\t11 (R k kt)->
                          primEnter sr t11 (rf a b c d e f 
                                                           g h i j k)))
                        t10))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim12 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                      R (Fun (\t11 (R k kt)->
                        R (Fun (\t12 (R l lt)->
                            primEnter sr t12 (rf a b c d e f 
                                                             g h i j k l)))
                          t11))
                        t10))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim13 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                      R (Fun (\t11 (R k kt)->
                        R (Fun (\t12 (R l lt)->
                          R (Fun (\t13 (R m mt)->
                              primEnter sr t13 (rf a b c d e f 
                                                         g h i j k l m)))
                            t12))
                          t11))
                        t10))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim14 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                      R (Fun (\t11 (R k kt)->
                        R (Fun (\t12 (R l lt)->
                          R (Fun (\t13 (R m mt)->
                            R (Fun (\t14 (R n nt)->
                                primEnter sr t14 (rf a b c d e f 
                                                         g h i j k l m n)))
                              t13))
                            t12))
                          t11))
                        t10))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

prim15 nm rf sr t = 
  R (Fun (\t1 (R a at)->
    R (Fun (\t2 (R b bt)->
      R (Fun (\t3 (R c ct)->
        R (Fun (\t4 (R d dt)->
          R (Fun (\t5 (R e et)->
            R (Fun (\t6 (R f ft)->
              R (Fun (\t7 (R g gt)->
                R (Fun (\t8 (R h ht)->
                  R (Fun (\t9 (R i it)->
                    R (Fun (\t10 (R j jt)->
                      R (Fun (\t11 (R k kt)->
                        R (Fun (\t12 (R l lt)->
                          R (Fun (\t13 (R m mt)->
                            R (Fun (\t14 (R n nt)->
                              R (Fun (\t15 (R o ot)->
                                  primEnter sr t15 (rf a b c d e f 
                                                         g h i j k l m n o)))
                                t14))
                              t13))
                            t12))
                          t11))
                        t10))
                      t9))
                    t8))
                  t7))
                t6))
              t5))
            t4))
          t3))
        t2))
      t1))
    (mkTNm t nm sr)

-- ----------------------------------------------------------------------------
-- combinators for untraced code

-- Assure that a trace component exists iff it is demanded
-- first trace is the hidden call of untraced code
-- second trace becomes trace of expression iff the expression is evaluated
-- but has hidden trace (is partial application)

-- Don't know in which order trace and value are demanded or if at all
-- Only if trace is demanded first, a Sat is created

ulazySat :: R a -> Trace -> R a

ulazySat x h =
  let status = unsafePerformIO (newIORef (Hidden h))
  in status `seq`
     R (case unsafePerformIO (readIORef status) of
          Hidden _ -> 
            case x of 
              R rv tv -> unsafePerformIO (writeIORef status (Eval tv)) `seq` rv
          Sat sat -> mkTSatBLonely sat `seq` 
                     case x of 
                       R rv tv -> mkTSatCLonely sat tv `seq` rv)
       (case unsafePerformIO (readIORef status) of
          Hidden h -> let sat = mkTSatALonely h 
                      in unsafePerformIO (writeIORef status (Sat sat)) `seq` 
                         sat
          Eval t -> t)
        
data Status = Hidden Trace  -- neither value nor trace yet demanded
            | Sat Trace     -- trace demanded, value not yet
            | Eval Trace    -- value demanded, trace not yet
                            

hiddenRoot :: Trace
hiddenRoot = mkTHidden mkTRoot

-- combinators for n-ary application

uap1 :: SR -> Trace -> R (Fun a r) -> R a -> R r
uap1 sr h (R (Fun rf) tf) a = ulazySat
  (case rf h a of
     R rv tv -> R rv (if hidden tv 
                        then mkTAp1 h tf (trace a) sr
                        else tv))
  h

uap2 :: SR -> Trace -> R (Fun a (Fun b r)) -> R a -> R b -> R r
uap2 sr h f a b = ulazySat
  (case uap1 sr h f a of
     R (Fun rf) tf -> case rf h b of
                        R rv tv -> R rv
                          (if hidden tv
                            then mkTAp1 h tf (trace b) sr
                            else tv))
  h

uap3 :: SR -> Trace -> R (Fun a (Fun b (Fun c r))) -> R a -> R b -> R c -> R r
uap3 sr h f a b c = ulazySat
  (case uap2 sr h f a b of
     R (Fun rf) tf -> case rf h c of
                        R rv tv -> R rv
                          (if hidden tv
                            then mkTAp1 h tf (trace c) sr
                            else tv))
  h

uap4 :: SR -> Trace -> R (Fun a (Fun b (Fun c (Fun d r)))) 
     -> R a -> R b -> R c -> R d -> R r
uap4 sr h f a b c d = ulazySat
  (case uap3 sr h f a b c of
     R (Fun rf) tf -> case rf h d of
                        R rv tv -> R rv
                          (if hidden tv
                             then mkTAp1 h tf (trace d) sr
                             else tv))
  h

-- combinators for transforming n-ary functions

ufun1 :: NmType -> (Trace -> R a -> R r) -> SR -> Trace -> R (Fun a r)
ufun1 nm rf sr t = R (Fun (\t a -> rf (mkTHidden t) a)) (mkTNm t nm sr)

ufun2 :: NmType -> (Trace -> R a -> R b -> R r) -> SR -> Trace
     -> R (Fun a (Fun b r))
ufun2 nm rf sr t =
  R (Fun (\t a ->
    R (Fun (\t b -> rf (mkTHidden t) a b))
      t))
    (mkTNm t nm sr)

ufun3 :: NmType -> (Trace -> R a -> R b -> R c -> R r) -> SR -> Trace
     -> R (Fun a (Fun b (Fun c r)))
ufun3 nm rf sr t =
  R (Fun (\t a ->
    R (Fun (\t b ->
      R (Fun (\t c -> rf (mkTHidden t) a b c))
        t))
      t))
    (mkTNm t nm sr)


-- ----------------------------------------------------------------------------
-- interface to foreign C functions

sameAs :: Trace -> Trace -> Bool
(Trace fileptr1) `sameAs` (Trace fileptr2) = fileptr1 == fileptr2


-- new combinators for portable transformation:

mkNoSourceRef :: SR
mkNoSourceRef = SR 0

foreign import "primSourceRef"
  mkSourceRef :: ModuleTraceInfo -> Int -> SR

mkAtomCon :: ModuleTraceInfo -> Int -> Int -> String -> NmType
mkAtomCon mti pos fixPri unqual = (mkAtomCon' mti pos fixPri) `useString` unqual

foreign import "primAtomCon"
  mkAtomCon' :: ModuleTraceInfo -> Int -> Int -> CString -> NmType

mkAtomId :: ModuleTraceInfo -> Int -> Int -> String -> NmType
mkAtomId mti pos fixPri unqual = (mkAtomId' mti pos fixPri) `useString` unqual

foreign import "primAtomId"
  mkAtomId' :: ModuleTraceInfo -> Pos -> Int -> CString -> NmType

mkAtomIdToplevel :: ModuleTraceInfo -> Pos -> Int -> String -> NmType
mkAtomIdToplevel mti pos fixPri unqual = 
  (mkAtomIdToplevel' mti pos fixPri) `useString` unqual

foreign import "primAtomIdToplevel"
  mkAtomIdToplevel' :: ModuleTraceInfo -> Pos -> Int -> CString -> NmType

mkModule :: String -> String -> Bool -> ModuleTraceInfo
mkModule unqual filename = (mkModule' `useString` unqual) `useString` filename

foreign import "primModule"
  mkModule' :: CString -> CString -> Bool -> ModuleTraceInfo

outputTrace :: Trace -> String -> IO ()
outputTrace trace output = withCString output (outputTrace' trace)

foreign import "outputTrace"
  outputTrace' :: Trace -> CString -> IO ()


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
 mkTInd :: Trace	-- trace 1
	-> Trace	-- trace 2
	-> Trace	-- result

foreign import "hidden" 
 hidden :: Trace -> Bool

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
 mkNTCString	:: CString -> NmType
foreign import "primNTIf"
 mkNTIf		:: NmType
foreign import "primNTGuard"
 mkNTGuard	:: NmType
foreign import "primNTContainer"
 mkNTContainer	:: NmType

-- ----------------------------------------------------------------------------
-- End
