module Prelude where

-- WARNING: all expressions here must use ONLY definitions from here.
-- Do not use ordinary Prelude functions - they are not available.
-- Do not use syntax (e.g. n+k patterns) which might be translated
--    by the compiler to normal Prelude functions.


import FFIBuiltin(PackedString)
import Ratio (Ratio)
--import DIO		-- needed for attaching traces to prim I/O operations.
import DEither		-- traced version needed, as for DIO.
import FFIBuiltin (Addr,ForeignObj,StablePtr)
import FFIBuiltin (Int8,Int16,Int32,Int64,Word8,Word16,Word32,Word64)
import PreludeBuiltin (Vector)

import HatArchive


{-
Invariant: the trace argument of R is always fully evaluated.
Trace arguments that are passed to functions are always fully evaluated.
No, not for pap: pap will force evaluation immediately.
-}

data R a = R a Trace

{- unused
instance Eq a => Eq (R a) where
    R x _ == R y _ = x == y

instance Ord a => Ord (R a)
-}

type Fun a b = Trace -> R a -> R b


{- data constructor R strict in trace argument -}
mkR :: a -> Trace -> R a
mkR x t = t `_seq` R x t


-- used internally by cCheckEvaluation
data _Value = _Evaluating | _Evaluated | _Closure

{-
data E a = E a
-}

-- toNm required to coerce return value from a primitive into a Trace structure
class NmCoerce a where
    toNm :: Trace -> a -> SR -> R a
--    toNm t v sr = mkR v (mkTNm t mkNTDummy sr)	
--    -- for safety, we hope never required
instance NmCoerce Int where
    toNm t v sr = mkR v (mkTNm t (mkNTInt v) sr)
instance NmCoerce Char where
    toNm t v sr = mkR v (mkTNm t (mkNTChar v) sr)
instance NmCoerce Integer where
    toNm t v sr = mkR v (mkTNm t (mkNTInteger v) sr)
instance NmCoerce Float where
    toNm t v sr = mkR v (mkTNm t (mkNTFloat v) sr)
instance NmCoerce Double where
    toNm t v sr = mkR v (mkTNm t (mkNTDouble v) sr)
instance NmCoerce Bool where
    toNm t False sr = mkR False (mkTNm t (mkNTConstr' 0) sr)  -- 0 wrong!
    toNm t True  sr = mkR True  (mkTNm t (mkNTConstr' 1) sr)  -- 1 wrong!
instance NmCoerce () where
    toNm t v sr = mkR v (mkTNm t (mkNTConstr' 0) sr)  -- 0 wrong!
instance NmCoerce Addr where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce (StablePtr a) where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce ForeignObj where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce PackedString where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
instance NmCoerce (Vector a) where
    toNm t v sr = mkR v (mkTNm t mkNTContainer sr)
{- Pattern matching on the trace that has been written to file is
definitely impossible. However, this FFI extension should be different
anyway.
instance NmCoerce (Either a b) where
    toNm t (Left  (R v (Nm _ nm x))) sr = 
      let t' = mkR v (mkTNm t nm x) in t' `myseq` mkR (Left t') t
    toNm t (Right (R v (Nm _ nm x))) sr = 
      let t' = mkR v (mkTNm t nm x) in t' `myseq` mkR (Right t') t
-}

-- These types use dummies for now.  Ideally, we want to convert them to
-- either Int or Integer (depending on size), but we can't do that yet
-- because the instances of fromIntegral are only available in traced
-- versions!
instance NmCoerce Int8
instance NmCoerce Int16
instance NmCoerce Int32
instance NmCoerce Int64
instance NmCoerce Word8
instance NmCoerce Word16
instance NmCoerce Word32
instance NmCoerce Word64


fatal primitive 1 :: Trace -> a
--cContains primitive 1 :: E a -> [R b]
--cGetConstrNm primitive 1 :: a -> NmType
--cGetFunNm primitive 1 :: a -> NmType
--cCheckEvaluation primitive 1 :: E a -> _Value
cPointerEquality primitive 2 :: E a -> E a -> Bool
{- cSeq primitive 2 :: a -> (E b) -> b -}
cEnter primitive 3 :: NmType -> Trace -> E a -> a
cInitializeDebugger primitive 1 :: E a -> a
--trusted primitive 2 :: Trace -> Trace -> Bool
--trust primitive 1 :: Trace -> Bool

enter :: NmType -> Trace -> a -> a
enter nm t e = cEnter nm t (E e)

{-
counterpart to 'enter' for primitives: ensures that the result trace
is fully evaluated (the trace contains the result value).
-}
primEnter :: NmCoerce a => SR -> NmType -> Trace -> a -> R a
primEnter sr nm t e = let v  = enter nm t e
                          vn = toNm t v sr
                      in v `myseq` vn


t_guard :: SR -> R Bool -> (Trace -> a) -> (Trace -> a) -> Trace -> a

t_guard sr (R gv gt) e cont t = 
  if trustedFun t 
    then if gv then e t else cont t
    else let t' = mkTAp2 t (mkTNm t mkNTGuard sr) gt t sr
	 in  t' `myseq` if gv then e t' else cont t'
         -- no SAT necessary, because the unevaluated form never
         -- has to be shown (the result wrapped expression is only
         -- created when it also has to be evaluated)


tif :: SR -> R Bool -> (Trace -> R a) -> (Trace -> R a) -> Trace -> R a

tif sr (R iv it) e1 e2 t = 
  if trustedFun t 
    then if iv then e1 t else e2 t
    else let t' = mkTAp2 t (mkTNm t mkNTIf sr) it t sr
         in  lazySat (if iv then e1 t' else e2 t') t'


{- used by _Driver -}
initializeDebugger :: a -> a
initializeDebugger a = 
    cInitializeDebugger (E a)


{-
A version of `seq' that is sure to work here 
myseq :: a -> b -> b

myseq a b = cSeq a (E b)
-}

{- A version of $! that is sure to work here -}
myseqAp :: (a -> b) -> a -> b

myseqAp f x = x `myseq` f x


{- used by 
   PrimExitWith, PrimPackString, PrimUnpackPS, Enum_Char, Eq_Int, LowVector -}
rseq :: R a -> b -> b

rseq (R v _) b = cSeq v (E b)


{- used by PreludeIO.Monad_IO 
Sets in a global variable the trace of the passed function.
This trace is the parent for subsequent output.
-}
setOutputContext :: SR -> Trace -> R (Trace -> R (Trace -> R a -> R b)
                                            -> R (Trace -> R a -> R b))
setOutputContext sr t =
    R (\t xf@(R rv rt) ->
       R (\t w ->
           _tprim_setOutputContext rt (E (ap1 sr t xf w)))
         t)
       t

_tprim_setOutputContext primitive 2 :: Trace -> E (R a) -> R a



-- ---------------------------------------------------------- --

{- Mark trace as incomplete due to trusting. -}
hide :: Trace -> Trace
hide t = if hidden t then t else mkTHidden t -- collapse Hidden chains


{- Check if function and application trace are trusted. -}
trusted :: Trace -> Trace -> Bool
trusted t tf = trustedFun tf `tand` trustedFun t

True  `tand` x = x
False `tand` _ = False


{- combinators for n-ary application in a non-projective context. -}
ap1 :: SR -> Trace -> R (Trace -> R a -> R r) -> R a -> R r 


ap1 sr t (R rf tf) a@(R _ at) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then (if hidden t then t else mkTHidden t)
             else mkTAp1 t tf at sr
  in  t1 `myseq` rf t1 a

ap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then (if hidden t then t else mkTHidden t)
             else mkTAp2 t tf at bt sr
  in  case (rf t1 a) of
        (R rf tf) -> if t1 `sameAs` tf 
                       then rf t1 b
                       else let t2 = mkTAp1 t1 tf bt sr
                            in  t2 `myseq` rf t2 b

ap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then (if hidden t then t else mkTHidden t)
             else mkTAp3 t tf at bt ct sr
  in  case (rf t1 a) of
        (R rf tf) -> 
          let t2 = if t1 `sameAs` tf 
                      then t1 
                      else mkTAp2 t1 tf bt ct sr
          in case (rf t2 b) of
               (R rf tf) ->
                 if t2 `sameAs` tf 
                   then rf t2 c
                   else let t3 = mkTAp1 t2 tf ct sr
                        in  t3 `myseq` rf t3 c

ap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then (if hidden t then t else mkTHidden t)
             else mkTAp4 t tf at bt ct dt sr
  in  case (rf t1 a) of
        (R rf tf) -> 
          let t2 = if t1 `sameAs` tf 
                      then t1 
                      else mkTAp3 t1 tf bt ct dt sr
          in case (rf t2 b) of
               (R rf tf) ->
                 let t3 = if t2 `sameAs` tf
                            then t2
                            else mkTAp2 t2 tf ct dt sr
                 in case (rf t3 c) of
                      (R rf tf) ->
                        if t3 `sameAs` tf 
                          then rf t3 d
                          else let t4 = mkTAp1 t3 tf dt sr
                               in  t4 `myseq` rf t4 d
      
{-
ap1 sr t (R rf tf) a@(R _ at) = 
  let t' = if trusted t tf
             then hide t
             else mkTAp1 t tf at sr
  in  t' `myseq` rf t' a

ap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp2 t tf at bt sr
  in  pap1 sr t' (rf t' a) b

ap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp3 t tf at bt ct sr
  in  pap2 sr t' (rf t' a) b c


ap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp4 t tf at bt ct dt sr
  in  pap3 sr t' (rf t' a) b c d
-}

ap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp5 t tf at bt ct dt et sr
  in  pap4 sr t' (rf t' a) b c d e

ap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp6 t tf at bt ct dt et ft sr
  in  pap5 sr t' (rf t' a) b c d e f

ap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp7 t tf at bt ct dt et ft gt sr
  in  pap6 sr t' (rf t' a) b c d e f g

ap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp8 t tf at bt ct dt et ft gt ht sr
  in  pap7 sr t' (rf t' a) b c d e f g h

ap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                   f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp9 t tf at bt ct dt et ft gt ht it sr
  in  pap8 sr t' (rf t' a) b c d e f g h i


ap10 :: SR -> Trace 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i (Fun j r)))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i -> R j 
     -> R r 

ap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
  in  pap9 sr t' (rf t' a) b c d e f g h i j

ap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) = 
  let t' = if trusted t tf 
             then hide t
             else mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
  in  pap10 sr t' (rf t' a) b c d e f g h i j k

ap12 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                    k@(R _ kt) l@(R _ lt) =  
  let t' = if trusted t tf 
             then hide t
             else mkTAp12 t tf at bt ct dt et ft gt ht it jt kt lt sr
  in  pap11 sr t' (rf t' a) b c d e f g h i j k l


{- 
Combinators for n-ary applications in a projective context 
The difference to the ap_n combinators is that in the case
of trusting no hidden trace node has to be created,
because the skipped trace node has the same type as the 
parent redex.
-}

rap1 :: SR -> Trace -> R (Fun a r) -> R a -> R r

rap1 sr t (R rf tf) a@(R _ at) = 
  if trustedFun tf `tand` trustedFun t
    then rf t a
    else let t1 = mkTAp1 t tf at sr
	 in  t1 `myseq` rf t1 a


rap2 :: SR -> Trace -> R (Fun a (Fun b r)) -> R a -> R b -> R r

rap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  let t1 = if trustedFun tf `tand` trustedFun t 
             then t
             else  mkTAp2 t tf at bt sr
  in case (rf t1 a) of
       (R rf tf) ->
         if t1 `sameAs` tf 
           then rf t1 b
           else let t2 = mkTAp1 t1 tf bt sr
                in  t2 `myseq` rf t2 b

rap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then t
             else mkTAp3 t tf at bt ct sr
  in  case (rf t1 a) of
        (R rf tf) ->
          let t2 =  if t1 `sameAs` tf 
                      then t1
                      else mkTAp2 t1 tf bt ct sr
          in case (rf t2 b) of
               (R rf tf) ->
                 if t2 `sameAs` tf 
                   then rf t2 c
                   else let t3 = mkTAp1 t2 tf ct sr
                        in  t3 `myseq` rf t3 c

rap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  let t1 = if trustedFun tf `tand` trustedFun t
             then t
             else mkTAp4 t tf at bt ct dt sr
  in  case (rf t1 a) of
        (R rf tf) ->
          let t2 =  if t1 `sameAs` tf 
                      then t1
                      else mkTAp3 t1 tf bt ct dt sr
          in case (rf t2 b) of
               (R rf tf) ->
                 let t3 =  if t2 `sameAs` tf 
                             then t2
                             else mkTAp2 t1 tf ct dt sr
                 in case (rf t3 c) of
                      (R rf tf) ->
                        if t3 `sameAs` tf 
                          then rf t3 d
                          else let t4 = mkTAp1 t3 tf dt sr
                               in  t4 `myseq` rf t4 d

{- original:
rap1 :: SR -> Trace -> R (Fun a r) -> R a -> R r

rap1 sr t (R rf tf) a@(R _ at) = 
  if trusted t tf 
    then rf t a
    else let t' = mkTAp1 t tf at sr
	 in  t' `myseq` rf t' a


rap2 :: SR -> Trace -> R (Fun a (Fun b r)) -> R a -> R b -> R r

rap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
  if trusted t tf 
    then pap1 sr t (rf t a) b
    else let t' = mkTAp2 t tf at bt sr
         in  pap1 sr t' (rf t' a) b

rap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) = 
  if trusted t tf 
    then pap2 sr t (rf t a) b c
    else let t' = mkTAp3 t tf at bt ct sr
 	 in  pap2 sr t' (rf t' a) b c

rap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = 
  if trusted t tf 
    then pap3 sr t (rf t a) b c d
    else let t' = mkTAp4 t tf at bt ct dt sr
	  in pap3 sr t' (rf t' a) b c d

-}

rap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) = 
  if trusted t tf 
    then pap4 sr t (rf t a) b c d e
    else let t' = mkTAp5 t tf at bt ct dt et sr
	 in  pap4 sr t' (rf t' a) b c d e

rap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) = 
  if trusted t tf 
    then pap5 sr t (rf t a) b c d e f
    else let t' = mkTAp6 t tf at bt ct dt et ft sr
         in  pap5 sr t' (rf t' a) b c d e f

rap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) = 
  if trusted t tf 
    then pap6 sr t (rf t a) b c d e f g
    else let t' = mkTAp7 t tf at bt ct dt et ft gt sr
	 in  pap6 sr t' (rf t' a) b c d e f g

rap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
  if trusted t tf 
    then pap7 sr t (rf t a) b c d e f g h
    else let t' = mkTAp8 t tf at bt ct dt et ft gt ht sr
	 in  pap7 sr t' (rf t' a) b c d e f g h

rap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
  if trusted t tf 
    then pap8 sr t (rf t a) b c d e f g h i
    else let t' = mkTAp9 t tf at bt ct dt et ft gt ht it sr
	 in  pap8 sr t' (rf t' a) b c d e f g h i

rap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
  if trusted t tf 
    then pap9 sr t (rf t a) b c d e f g h i j
    else let t' = mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
	 in  pap9 sr t' (rf t' a) b c d e f g h i j

rap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) = 
  if trusted t tf 
    then pap10 sr t (rf t a) b c d e f g h i j k
    else let t' = mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
	 in  pap10 sr t' (rf t' a) b c d e f g h i j k

rap12 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) l@(R _ lt) =  
  if trusted t tf 
    then pap11 sr t (rf t a) b c d e f g h i j k l
    else let t' = mkTAp12 t tf at bt ct dt et ft gt ht it jt kt lt sr
	 in  pap11 sr t' (rf t' a) b c d e f g h i j k l


{- 
Combinators for n-ary application used by the combinators above.
Introduces a new application node if the function is a saturated
application.
-}
pap0 :: Trace -> R r -> R r

pap0 t e = e


pap1 :: SR -> Trace -> R (Trace -> R a -> R r) -> R a -> R r

pap1 sr t (R rf tf) a@(R _ at) =
  if t `sameAs` tf 
    then rf t a
    else let t' = mkTAp1 t tf at sr
         in  t' `myseq` rf t' a

pap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) =
  if t `sameAs` tf 
    then pap1 sr t (rf t a) b
    else let t' = mkTAp2 t tf at bt sr
         in  pap1 sr t' (rf t' a) b

pap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) =
  let t' = if t `sameAs` tf 
             then t
             else mkTAp3 t tf at bt ct sr
  in case (rf t' a) of
       (R rf tf) -> 
         let t'' = if t' `sameAs` tf
                     then t'
                     else mkTAp2 t' tf bt ct sr
         in case (rf t'' b) of
              (R rf tf) ->
                if t'' `sameAs` tf 
                  then rf t'' c
                  else let t''' = mkTAp1 t'' tf ct sr
                       in  t''' `myseq` rf t''' c
 
{- original:
pap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) =
  if t `sameAs` tf 
    then pap2 sr t (rf t a) b c
    else let t' = mkTAp3 t tf at bt ct sr
         in pap2 sr t' (rf t' a) b c
-}

pap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
  if t `sameAs` tf 
    then pap3 sr t (rf t a) b c d
    else let t' = mkTAp4 t tf at bt ct dt sr
         in  pap3 sr t' (rf t' a) b c d

pap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) =
  if t `sameAs` tf 
    then pap4 sr t (rf t a) b c d e
    else let t' = mkTAp5 t tf at bt ct dt et sr
         in  pap4 sr t' (rf t' a) b c d e

pap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) =
  if t `sameAs` tf 
    then pap5 sr t (rf t a) b c d e f
    else let t' = mkTAp6 t tf at bt ct dt et ft sr
         in  pap5 sr t' (rf t' a) b c d e f

pap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) =
  if t `sameAs` tf 
    then pap6 sr t (rf t a) b c d e f g
    else let t' = mkTAp7 t tf at bt ct dt et ft gt sr
         in  pap6 sr t' (rf t' a) b c d e f g

pap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) =
  if t `sameAs` tf 
    then pap7 sr t (rf t a) b c d e f g h
    else let t' = mkTAp8 t tf at bt ct dt et ft gt ht sr
         in  pap7 sr t' (rf t' a) b c d e f g h


pap9 :: SR -> Trace 
     -> R (Fun a (Fun b (Fun c (Fun d (Fun e (Fun f (Fun g (Fun h 
        (Fun i r))))))))) 
     -> R a -> R b -> R c -> R d -> R e -> R f -> R g -> R h -> R i 
     -> R r 

pap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                    f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) =
  if t `sameAs` tf 
    then pap8 sr t (rf t a) b c d e f g h i
    else let t' = mkTAp9 t tf at bt ct dt et ft gt ht it sr
         in  pap8 sr t' (rf t' a) b c d e f g h i

pap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) =
  if t `sameAs` tf 
    then pap9 sr t (rf t a) b c d e f g h i j
    else let t' = mkTAp10 t tf at bt ct dt et ft gt ht it jt sr
         in pap9 sr t' (rf t' a) b c d e f g h i j

pap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et)
                     f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt)
                     k@(R _ kt) =
  if t `sameAs` tf 
    then pap10 sr t (rf t a) b c d e f g h i j k
    else let t' = mkTAp11 t tf at bt ct dt et ft gt ht it jt kt sr
         in  pap10 sr t' (rf t' a) b c d e f g h i j k 



{-
Assure that a trace component of wrapped value exists by construction of Sat.
Directly used for Cafs
and used for fun_n, prim_n.
-}
lazySat :: R a -> Trace -> R a

lazySat x t = 
  let sat = mkTSatA t
  in mkR (mkTSatB sat `myseq` -- mark entering of evaluation
          case x of -- create trace for (unevaluated x/v)
            R v vt ->
              v `myseq` -- evaluate v and thus extend trace for v
              mkTSatC sat vt `myseq` -- set trace for evaluated v
              v) -- return value
       sat

{- without SatB and C:
lazySat x t = 
  let sat = mkTSatA t
  in mkR (
          case x of -- create trace for (unevaluated x/v)
            R v vt ->
              v) -- return value
       sat
-}

-- The following combinator is currently not used.
-- It should be used to not to loose information about pattern bindings.
-- (in transformation of DeclPat).

{- Add name of pattern as an indirection -}
patvar :: NmType -> R a -> SR -> Trace -> R a

patvar nm (R v vt) sr t = 
  mkR v (mkTInd (mkTNm t nm sr) vt)


{- Combintors for transforming n-ary functions. -}
fun0 :: NmType -> (Trace -> R r) -> SR -> Trace -> R r

fun0 nm rf sr t = 
  let t' = mkTNm t nm sr
  in t' `myseq` lazySat (enter nm t' (rf t)) t'  -- t here correct?


fun1 :: NmType -> (Trace -> R a -> R r) -> SR -> Trace 
     -> R (Trace -> R a -> R r)

fun1 nm rf sr t = 
  mkR (\t a -> let sat = mkTSatA t
               in mkR (mkTSatB sat `myseq` 
                        case (enter nm t (rf t a)) of 
                          R v vt ->
                            v `myseq` 
                            mkTSatC sat vt `myseq` 
                            v) 
                  sat)
      (mkTNm t nm sr)

fun2 nm rf sr t = 
  mkR (\t a ->
      R (\t b -> let sat = mkTSatA t
                 in mkR (mkTSatB sat `myseq` 
                          case (enter nm t (rf t a b)) of 
                            R v vt ->
                              v `myseq` 
                              mkTSatC sat vt `myseq` 
                              v) 
                    sat)
        t)
      (mkTNm t nm sr)

fun3 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c -> let sat = mkTSatA t
                   in mkR (mkTSatB sat `myseq` 
                            case (enter nm t (rf t a b c)) of 
                              R v vt ->
                                v `myseq` 
                                mkTSatC sat vt `myseq` 
                                v) 
                      sat)
          t)
        t)
      (mkTNm t nm sr)

fun4 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d -> let sat = mkTSatA t
                   in mkR (mkTSatB sat `myseq` 
                            case (enter nm t (rf t a b c d)) of 
                              R v vt ->
                                v `myseq` 
                                mkTSatC sat vt `myseq` 
                                v) 
                      sat)
            t)
          t)
        t)
      (mkTNm t nm sr)

fun5 nm rf sr t = 
  mkR (\t a ->
      R (\t b ->
        R (\t c ->
          R (\t d ->
            R (\t e -> lazySat (enter nm t (rf t a b c d e)) t)
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
              R (\t f -> lazySat (enter nm t (rf t a b c d e f)) t)
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
                R (\t g -> lazySat (enter nm t (rf t a b c d e f g)) t)
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
                  R (\t h -> lazySat (enter nm t (rf t a b c d e f g h)) t)
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
                    R (\t i -> lazySat (enter nm t (rf t a b c d e f g h i)) t)
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
                      R (\t j -> lazySat (enter nm t (rf t a b c d e f g 
                                                           h i j)) t)
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
                        R (\t k -> lazySat (enter nm t (rf t a b c d e f g 
                                                             h i j k)) t)
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
                          R (\t l -> lazySat (enter nm t (rf t a b c d e f g 
                                                               h i j k l)) t)
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


-- These four functions are dummies, introduced by the tracing compiler
-- and then eliminated again before code generation.
fromConInteger :: (Prelude.Num a) => SR -> Trace -> Integer -> R a
fromConInteger sr t x = mkR 1 mkTRoot

patFromConInteger :: (Prelude.Num a) => SR -> Trace -> Integer -> R a
patFromConInteger sr t x = mkR 1 mkTRoot

fromConRational :: (Prelude.Fractional a) => SR -> Trace -> Rational -> R a
fromConRational sr t x = mkR 1 mkTRoot

patFromConRational :: (Prelude.Fractional a) => SR -> Trace -> Rational -> R a
patFromConRational sr t x = mkR 1 mkTRoot
----

{- Used in module Case to translate overloaded numbers in patterns;
these appear in the prelude in SplitAt, Take, Drop 
-}
rPatBool :: R Bool -> Bool	
rPatBool (R v _) = v


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
conRational sr t b = mkR b (mkTNm t (mkNTRational b) sr)


{- Combinators for saturated n-ary applications of data constructors. -}
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


{-
Combinators for calling foreign functions:   prim_n
Definitions identical to those of fun_n, except for the use of primEnter
instead of enter and that the unwrapped arguments are passed.
(Maybe common abstraction should be defined?)
-}

prim0 :: NmCoerce r => NmType -> r -> SR -> Trace -> R r

prim0 nm rf sr t = 
  let tf = mkTNm t nm sr
  in lazySat (primEnter sr nm tf rf) tf  -- primEnter strict in tf


prim1 :: NmCoerce r => NmType -> (a -> r) -> SR -> Trace -> R (Fun a r)

prim1 nm rf sr t = 
  mkR (\t (R a at) -> let sat = mkTSatA t
                      in mkR (mkTSatB sat `myseq` 
                               case (primEnter sr nm t (rf a)) of 
                                 R v vt ->
                                   v `myseq` 
                                   mkTSatC sat vt `myseq` 
                                   v) 
                           sat)
    (mkTNm t nm sr)


prim2 :: NmCoerce r => 
         NmType -> (a -> b -> r) -> SR -> Trace -> R (Fun a (Fun b r))

prim2 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)-> let sat = mkTSatA t
                     in  mkR (mkTSatB sat `myseq` 
                               case (primEnter sr nm t (rf a b)) of 
                                 R v vt ->
                                   v `myseq` 
                                   mkTSatC sat vt `myseq` 
                                   v) 
                           sat)
      t)
    (mkTNm t nm sr)

prim3 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)-> let sat = mkTSatA t
                       in  mkR (mkTSatB sat `myseq` 
                                 case (primEnter sr nm t (rf a b c)) of 
                                   R v vt ->
                                     v `myseq` 
                                     mkTSatC sat vt `myseq` 
                                     v) 
                              sat)
        t)
      t)
    (mkTNm t nm sr)

prim4 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)-> let sat = mkTSatA t
                         in  mkR (mkTSatB sat `myseq` 
                                   case (primEnter sr nm t (rf a b c d)) of 
                                     R v vt ->
                                       v `myseq` 
                                       mkTSatC sat vt `myseq` 
                                       v) 
                               sat)
          t)
        t)
      t)
    (mkTNm t nm sr)

prim5 nm rf sr t = 
  mkR (\t (R a at)->
    R (\t (R b bt)->
      R (\t (R c ct)->
        R (\t (R d dt)->
          R (\t (R e et)-> lazySat (primEnter sr nm t (rf a b c d e)) t)
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
            R (\t (R f ft)-> lazySat (primEnter sr nm t (rf a b c d e f)) t)
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
                  lazySat (primEnter sr nm t (rf a b c d e f g)) t)
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
                    lazySat (primEnter sr nm t (rf a b c d e f g h)) t)
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
                      lazySat (primEnter sr nm t9 (rf a b c d e f g h i)) t9)
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
                        lazySat (primEnter sr nm t10 (rf a b c d e f 
                                                         g h i j)) t10)
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
                          lazySat (primEnter sr nm t11 (rf a b c d e f 
                                                           g h i j k)) t11)
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
                            lazySat (primEnter sr nm t12 (rf a b c d e f 
                                                             g h i j k l)) t12)
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

pa0 :: b -> (b -> Trace -> c) -> SR -> Trace -> NmType -> c

pa0 c cni sr t nm =
  cni c `myseqAp` (mkTNm t nm sr) 


pa1 :: (R a1 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType -> R a1 -> c

pa1 c cni sr t nm a1@(R _ t1) =
  cni (c a1) `myseqAp` mkTAp1 t (mkTNm t nm sr) t1 sr


pa2 :: (R a1 -> R a2 -> b) -> (b -> Trace -> c) -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> c

pa2 c cni sr t nm a1@(R _ t1) a2@(R _ t2) =
  cni (c a1 a2) `myseqAp` mkTAp2 t (mkTNm t nm sr) t1 t2 sr


pa3 :: (R a1 -> R a2 -> R a3 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace -> NmType
    -> R a1 -> R a2 -> R a3 -> c

pa3 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) =
  cni (c a1 a2 a3) `myseqAp` mkTAp3 t (mkTNm t nm sr) t1 t2 t3 sr


pa4 :: (R a1 -> R a2 -> R a3 -> R a4 -> b) -> (b -> Trace -> c) 
    -> SR -> Trace  -> NmType
    -> R a1 -> R a2 -> R a3 -> R a4 -> c

pa4 c cni sr t nm a1@(R _ t1) a2@(R _ t2) a3@(R _ t3) a4@(R _ t4) =
  cni (c a1 a2 a3 a4) `myseqAp` mkTAp4 t (mkTNm t nm sr) t1 t2 t3 t4 sr


{- Seems to be used for transformation of primitives, really true -}
_prim :: a
_prim = error "_prim"

