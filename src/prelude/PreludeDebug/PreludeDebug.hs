module Prelude where

--import PackedString(unpackPS, PackedString(..), packString)

sameAs :: a -> a -> Bool
x `sameAs` y = cPointerEquality (E x) (E y)

data _Value = _Evaluating | _Evaluated | _Closure

data E a = E a

data NmType =
     NTInt Int
   | NTChar Char
   | NTInteger Integer
   | NTRational Rational
   | NTFloat Float
   | NTDouble Double
   | NTId Int
   | NTConstr Int
   | NTTuple
   | NTFun
   | NTCase
   | NTLambda 
   | NTDummy   
   | NTCString Int
   | NTIf
   | NTGuard

-- Don't change the order!!!
data Trace =
     Ap Trace Traces SR
   | Nm Trace NmType SR
   | Ind Trace Trace
   | Root
   | Sat Trace Trace
   | Pruned
   | Hidden Trace

data SR = SR | SR2 Bool Int | SR3 Int 

data Traces = 
     TNil
   | TCons Trace Traces 

data R a = R a Trace

instance Eq a => Eq (R a) where
    R x _ == R y _ = x == y

instance Ord a => Ord (R a)

--instance (Show a) => Show (R a) where
--    showsPrec d (R a _) = showsPrec d a

fatal primitive 1 :: Trace -> a
--cContains primitive 1 :: E a -> [R b]
--cGetConstrNm primitive 1 :: a -> NmType
--cGetFunNm primitive 1 :: a -> NmType
cCheckEvaluation primitive 1 :: E a -> _Value
cPointerEquality primitive 2 :: E a -> E a -> Bool
cSeq primitive 2 :: a -> (E b) -> b
cEnter primitive 3 :: NmType -> Trace -> E a -> a
cInitializeDebugger primitive 1 :: E a -> a
trusted primitive 2 :: Trace -> Trace -> Bool
trust primitive 1 :: Trace -> Bool

enter :: NmType -> Trace -> a -> a
enter nm t e = cEnter nm t (E e)

getRedexes :: R a -> Trace
getRedexes (R _ t) = t

guard sr (R gv gt) e cont t = 
    if trust t then
        if gv then e t else cont t
    else
        let t' = Ap t (TCons (Nm t NTGuard sr) (TCons gt (TCons t TNil))) sr 
	in  if gv then e t' else cont t'

tif sr (R iv it) e1 e2 t = 
    if trust t then
        if iv then e1 t else e2 t
    else
        let t' = Ap t (TCons (Nm t NTIf sr) (TCons it (TCons t TNil))) sr 
	in  if iv then e1 t' else e2 t'

--contains :: a -> [R b]
--contains x = cContains (E x)

evaled :: a -> _Value
evaled x = cCheckEvaluation (E x)

initializeDebugger :: a -> a
initializeDebugger a = 
    cInitializeDebugger (E a)

myseq a b = cSeq a (E b)

rseq (R v _) b = cSeq v (E b)
--rseq (R v _) b = cSeq v b
--rseq a b = b

setOutputContext :: SR -> Trace -> R (Trace -> R (Trace -> R a -> R b) -> R (Trace -> R a -> R b))
setOutputContext sr t = R (\t xf@(R rv rt) -> R (\t w -> _tprim_setOutputContext rt (E (ap1 sr t xf w))) t) t

_tprim_setOutputContext primitive 2 :: Trace -> E (R a) -> R a

hidden h@(Hidden _) = h
hidden t            = Hidden t

ap1 sr t (R rf tf) a@(R _ at) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> rf t a
    else
        let t1 = Ap t (TCons tf (TCons at TNil)) sr
	in rf t1 a

ap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap1 sr t (rf t a) b
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt TNil))) sr
	in pap1 sr t1 (rf t1 a) b

ap3 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap2 sr t (rf t a) b c
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct TNil)))) sr
	in pap2 sr t1 (rf t1 a) b c

ap4 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap3 sr t (rf t a) b c d
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt TNil))))) sr
	in pap3 sr t1 (rf t1 a) b c d

ap5 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap4 sr t (rf t a) b c d e
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et TNil)))))) sr
	in pap4 sr t1 (rf t1 a) b c d e

ap6 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap5 sr t (rf t a) b c d e f
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft TNil))))))) sr
	in pap5 sr t1 (rf t1 a) b c d e f

ap7 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap6 sr t (rf t a) b c d e f g
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt TNil)))))))) sr
	in pap6 sr t1 (rf t1 a) b c d e f g

ap8 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap7 sr t (rf t a) b c d e f g h
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht TNil))))))))) sr
	in pap7 sr t1 (rf t1 a) b c d e f g h

ap9 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap8 sr t (rf t a) b c d e f g h i
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it TNil)))))))))) sr
	in pap8 sr t1 (rf t1 a) b c d e f g h i

ap10 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap9 sr t (rf t a) b c d e f g h i j
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt TNil))))))))))) sr
	in pap9 sr t1 (rf t1 a) b c d e f g h i j

ap11 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) = 
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap10 sr t (rf t a) b c d e f g h i j k
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt TNil)))))))))))) sr
	in pap10 sr t1 (rf t1 a) b c d e f g h i j k

ap12 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) =  
    if trusted t tf then
        case hidden t of
	    t@(Hidden _) -> pap11 sr t (rf t a) b c d e f g h i j k l
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt (TCons lt TNil))))))))))))) sr
	in pap11 sr t1 (rf t1 a) b c d e f g h i j k l

rap1 sr t (R rf tf) a@(R _ at) = 
    if trusted t tf then
        rf t a
    else
        let t1 = Ap t (TCons tf (TCons at TNil)) sr
	in rf t1 a

rap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) = 
    if trusted t tf then
	pap1 sr t (rf t a) b
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt TNil))) sr
	in pap1 sr t1 (rf t1 a) b

rap3 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) = 
    if trusted t tf then
	pap2 sr t (rf t a) b c
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct TNil)))) sr
	in pap2 sr t1 (rf t1 a) b c

rap4 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) = 
    if trusted t tf then
	pap3 sr t (rf t a) b c d
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt TNil))))) sr
	in pap3 sr t1 (rf t1 a) b c d

rap5 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et) = 
    if trusted t tf then
	pap4 sr t (rf t a) b c d e
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et TNil)))))) sr
	in pap4 sr t1 (rf t1 a) b c d e

rap6 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) = 
    if trusted t tf then
	pap5 sr t (rf t a) b c d e f
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft TNil))))))) sr
	in pap5 sr t1 (rf t1 a) b c d e f

rap7 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) = 
    if trusted t tf then
	pap6 sr t (rf t a) b c d e f g
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt TNil)))))))) sr
	in pap6 sr t1 (rf t1 a) b c d e f g

rap8 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) = 
    if trusted t tf then
	pap7 sr t (rf t a) b c d e f g h
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht TNil))))))))) sr
	in pap7 sr t1 (rf t1 a) b c d e f g h

rap9 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = 
    if trusted t tf then
	pap8 sr t (rf t a) b c d e f g h i
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it TNil)))))))))) sr
	in pap8 sr t1 (rf t1 a) b c d e f g h i

rap10 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = 
    if trusted t tf then
	pap9 sr t (rf t a) b c d e f g h i j
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt TNil))))))))))) sr
	in pap9 sr t1 (rf t1 a) b c d e f g h i j

rap11 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) = 
    if trusted t tf then
	pap10 sr t (rf t a) b c d e f g h i j k
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt TNil)))))))))))) sr
	in pap10 sr t1 (rf t1 a) b c d e f g h i j k

rap12 sr t (R rf tf) a@(R _ at) b@(R _ bt)  c@(R _ ct) d@(R _ dt) e@(R _ et)  f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) =  
    if trusted t tf then
	pap11 sr t (rf t a) b c d e f g h i j k l
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt (TCons lt TNil))))))))))))) sr
	in pap11 sr t1 (rf t1 a) b c d e f g h i j k l

pap0 t e = e

pap1 sr t (R rf tf) a@(R _ at) =
    if t `sameAs` tf then
        rf t a
    else
        let t1 = Ap t (TCons tf (TCons at TNil)) sr
        in rf t1 a

pap2 sr t (R rf tf) a@(R _ at) b@(R _ bt) =
    if t `sameAs` tf then
        pap1 sr t (rf t a) b
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt TNil))) sr
        in pap1 sr t1 (rf t1 a) b

pap3 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) =
    if t `sameAs` tf then
        pap2 sr t (rf t a) b c
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct TNil)))) sr
        in pap2 sr t1 (rf t1 a) b c

pap4 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) =
    if t `sameAs` tf then
        pap3 sr t (rf t a) b c d
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt TNil))))) sr
        in pap3 sr t1 (rf t1 a) b c d

pap5 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) =
    if t `sameAs` tf then
        pap4 sr t (rf t a) b c d e
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et TNil)))))) sr
        in pap4 sr t1 (rf t1 a) b c d e

pap6 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) =
    if t `sameAs` tf then
        pap5 sr t (rf t a) b c d e f
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft TNil))))))) sr
        in pap5 sr t1 (rf t1 a) b c d e f

pap7 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) =
    if t `sameAs` tf then
        pap6 sr t (rf t a) b c d e f g
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt TNil)))))))) sr
        in pap6 sr t1 (rf t1 a) b c d e f g

pap8 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) =
    if t `sameAs` tf then
        pap7 sr t (rf t a) b c d e f g h
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht TNil))))))))) sr
        in pap7 sr t1 (rf t1 a) b c d e f g h

pap9 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) =
    if t `sameAs` tf then
        pap8 sr t (rf t a) b c d e f g h i
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it TNil)))))))))) sr
        in pap8 sr t1 (rf t1 a) b c d e f g h i

pap10 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) =
    if t `sameAs` tf then
        pap9 sr t (rf t a) b c d e f g h i j
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt TNil))))))))))) sr
        in pap9 sr t1 (rf t1 a) b c d e f g h i j

pap11 sr t (R rf tf) a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) =
    if t `sameAs` tf then
        pap10 sr t (rf t a) b c d e f g h i j k
    else
        let t1 = Ap t (TCons tf (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt TNil)))))))))))) sr
        in pap10 sr t1 (rf t1 a) b c d e f g h i j k 

patvar nm (R v vt) sr t = 
    R v (Nm vt nm sr)

caf rv = \sr t -> rv 

{-
caf nm rv = 
    \sr t ->  
    let R r rt = rv
	t' = Nm t nm sr
    in R r (Sat t' rt)
-}

fun0 nm rf sr t = 
    let t' = Nm t nm sr
        R r rt = enter nm t' (rf t)
    in R r (Sat t' rt)
fun1 nm rf sr t = 
    R (\t a -> let (R r rt) = enter nm t (rf t a) in R r (Sat t rt)) (Nm t nm sr)
fun2 nm rf sr t = 
    R (\t a -> R (\t b -> let (R r rt) = enter nm t (rf t a b) in R r (Sat t rt)) t) (Nm t nm sr)
fun3 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> let (R r rt) = enter nm t (rf t a b c) in R r (Sat t rt)) t) t) (Nm t nm sr)
fun4 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> let (R r rt) = enter nm t (rf t a b c d) in R r (Sat t rt)) t) t) t) (Nm t nm sr)
fun5 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> let (R r rt) = enter nm t (rf t a b c d e) in R r (Sat t rt)) t) t) t) t) (Nm t nm sr)
fun6 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> let (R r rt) = enter nm t (rf t a b c d e f) in R r (Sat t rt)) t) t) t) t) t) (Nm t nm sr)
fun7 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> let (R r rt) = enter nm t (rf t a b c d e f g) in R r (Sat t rt)) t) t) t) t) t) t) (Nm t nm sr)
fun8 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> let (R r rt) = enter nm t (rf t a b c d e f g h) in R r (Sat t rt)) t) t) t) t) t) t) t) (Nm t nm sr)
fun9 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (\t i -> let (R r rt) = enter nm t (rf t a b c d e f g h i) in R r (Sat t rt)) t) t) t) t) t) t) t) t) (Nm t nm sr)
fun10 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (\t i -> R (\t j -> let (R r rt) = enter nm t (rf t a b c d e f g h i j) in R r (Sat t rt)) t) t) t) t) t) t) t) t) t) (Nm t nm sr)
fun11 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (\t i -> R (\t j -> R (\t k -> let (R r rt) = enter nm t (rf t a b c d e f g h i j k) in R r (Sat t rt)) t) t) t) t) t) t) t) t) t) t) (Nm t nm sr)
fun12 nm rf sr t = 
    R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (\t i -> R (\t j -> R (\t k -> R (\t l -> let (R r rt) = enter nm t (rf t a b c d e f g h i j k l) in R r (Sat t rt)) t) t) t) t) t) t) t) t) t) t) t) (Nm t nm sr)

{-
noIfTrace (Hidden _) = True
noIfTrace (Ap _ (TCons t _) _) = trustedFun t
noIfTrace (Nm _ nm _) = trustedName nm
noIfTrace (Sat t _) = trustedFun t
noIfTrace (Ind ct _) = trustedFun t
-}

indir :: Trace -> R a -> R a
indir t (R v t') = R v (Ind t t')

conInt :: SR -> Trace -> Int -> R Int
conInt sr t n = R n (Nm t (NTInt n) sr)

conChar :: SR -> Trace -> Char -> R Char
conChar sr t c = R c (Nm t (NTChar c) sr)

conInteger :: SR -> Trace -> Integer -> R Integer
conInteger sr t b = R b (Nm t (NTInteger b) sr)

fromConInteger :: (Prelude.Num a) => SR -> Trace -> Integer -> R a
fromConInteger sr t x = R 1 Root

patFromConInteger :: (Prelude.Num a) => SR -> Trace -> Integer -> R a
patFromConInteger sr t x = R 1 Root

conRational :: SR -> Trace -> Rational -> R Rational
conRational sr t b = R b (Nm t (NTRational b) sr)

conFloat :: SR -> Trace -> Float -> R Float
conFloat sr t b = R b (Nm t (NTFloat b) sr)

conDouble :: SR -> Trace -> Double -> R Double
conDouble sr t b = R b (Nm t (NTDouble b) sr)

con0 sr t cn nm = R cn (Nm t nm sr)

con1 sr t cn nm a@(R _ at) = R (cn a) (Ap t (TCons (Nm t nm sr) (TCons at TNil)) sr)

con2 sr t cn nm a@(R _ at) b@(R _ bt) = R (cn a b) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt TNil))) sr)

con3 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) = R (cn a b c) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct TNil)))) sr)

con4 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = R (cn a b c d) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt TNil))))) sr)

con5 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) = R (cn a b c d e) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et TNil)))))) sr)

con6 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) = R (cn a b c d e f) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft TNil))))))) sr)

con7 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) = R (cn a b c d e f g) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt TNil)))))))) sr)

con8 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) = R (cn a b c d e f g h) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht TNil))))))))) sr)

con9 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) = R (cn a b c d e f g h i) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it TNil)))))))))) sr)

con10 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) = R (cn a b c d e f g h i j) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt TNil))))))))))) sr)

con11 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) = R (cn a b c d e f g h i j k) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt TNil)))))))))))) sr)

con12 sr t cn nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) e@(R _ et) f@(R _ ft) g@(R _ gt) h@(R _ ht) i@(R _ it) j@(R _ jt) k@(R _ kt) l@(R _ lt) = R (cn a b c d e f g h i j k l) (Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt (TCons et (TCons ft (TCons gt (TCons ht (TCons it (TCons jt (TCons kt (TCons lt TNil))))))))))))) sr)

getArgRedexes ((R _ t) : rts) = myseq t (TCons t (getArgRedexes rts))
getArgRedexes [] = TNil

mkAp n t sr TNil = n
mkAp n t sr args = Ap t (TCons n args) sr

spine (TCons  _ xs) = spine xs
spine TNil = ()

value (R v _) = v

{-
datatype Trace
datatype NmType
datatype SR

con0, con
ap1, ..., apn
pap1, ..., papn
fun1, ..., funn
caf
-}

-- Dummy defs
cn1 rf t = myseq t (R (\t a -> R (rf a) t) t)
cn2 rf t = myseq t (R (\t a -> R (\t b -> R (rf a b ) t) t) t)
cn3 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (rf a b c) t) t) t) t)
cn4 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (rf a b c d) t) t) t) t) t)
cn5 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (rf a b c d e) t) t) t) t) t) t)
cn6 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (rf a b c d e f) t) t) t) t) t) t) t)
cn7 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (rf a b c d e f g) t) t) t) t) t) t) t) t)
cn8 rf t = myseq t (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (rf a b c d e f g h) t) t) t) t) t) t) t) t) t)
pa0 sr t nm = Nm t nm sr
--pa0 sr t nm = Ap t (TCons (Nm t nm sr) TNil) sr
pa1 sr t nm a@(R _ at) = Ap t (TCons (Nm t nm sr) (TCons at TNil)) sr
pa2 sr t nm a@(R _ at) b@(R _ bt) = Ap t (TCons (Nm t nm sr) (TCons at (TCons bt TNil))) sr
pa3 sr t nm a@(R _ at) b@(R _ bt) c@(R _ ct) = Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct TNil)))) sr
pa4 sr t nm a@(R _ at) b@(R _ bt) c@(R _ ct) d@(R _ dt) = Ap t (TCons (Nm t nm sr) (TCons at (TCons bt (TCons ct (TCons dt TNil))))) sr

c1 nm rf sr t = 
    (R (\t a -> R (rf a) t) (Nm t nm sr))

c2 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (rf a b) t) t) (Nm t nm sr))

c3 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (rf a b c) t) t) t) (Nm t nm sr))

c4 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (rf a b c d) t) t) t) t) (Nm t nm sr))

c5 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (rf a b c d e) t) t) t) t) t) (Nm t nm sr))

c6 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (rf a b c d e f) t) t) t) t) t) t) (Nm t nm sr))

c7 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (rf a b c d e f g) t) t) t) t) t) t) t) (Nm t nm sr))

c8 nm rf sr t = 
    --let fn = cGetConstrNm rf
    --in myseq fn 
    (R (\t a -> R (\t b -> R (\t c -> R (\t d -> R (\t e -> R (\t f -> R (\t g -> R (\t h -> R (rf a b c d e f g h) t) t) t) t) t) t) t) t) (Nm t nm sr))

--c1 = False
--c2 = False
--c3 = False
--c4 = False
--c5 = False
--c6 = False
--c7 = False
--c8 = False
c9 = False
c10 = False
c11 = False
c12 = False

--ap11 = False
--ap12 = False
--fun11 = False
--fun12 = False

_fromInteger = False
_fromRational = False
_error = error "_error"
data RList a = RCons | RNil
type RString = Bool
--class Num a where
--   fff :: a -> a
stringConst primitive 3 :: SR -> Trace -> a -> R String
--interact = False
dbgprint = False
_prim :: a
_prim = error "_prim"

_hide :: a -> a
_hide = error "_hide"

dummy :: Char
dummy = 'a'

{-
ap2 primitive 5 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> c)))))) -> ((R a) -> ((R b) -> c)))))

ap3 primitive 6 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> d))))))))) -> ((R a) -> ((R b) -> ((R c) -> d))))))

ap4 primitive 7 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> e)))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> e)))))))

ap5 primitive 8 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> f))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> f))))))))

ap6 primitive 9 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> (R (Trace -> ((R f) -> g)))))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> ((R f) -> g)))))))))

ap7 primitive 10 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> (R (Trace -> ((R f) -> (R (Trace -> ((R g) -> h))))))))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> ((R f) -> ((R g) -> h))))))))))

ap8 primitive 11 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> (R (Trace -> ((R f) -> (R (Trace -> ((R g) -> (R (Trace -> ((R h) -> i)))))))))))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> ((R f) -> ((R g) -> ((R h) -> i)))))))))))

ap9 primitive 12 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> (R (Trace -> ((R f) -> (R (Trace -> ((R g) -> (R (Trace -> ((R h) -> (R (Trace -> ((R i) -> j))))))))))))))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> ((R f) -> ((R g) -> ((R h) -> ((R i) -> j))))))))))))

ap10 primitive 13 ::(SR -> (Trace -> ((R (Trace -> ((R a) -> (R (Trace -> ((R b) -> (R (Trace -> ((R c) -> (R (Trace -> ((R d) -> (R (Trace -> ((R e) -> (R (Trace -> ((R f) -> (R (Trace -> ((R g) -> (R (Trace -> ((R h) -> (R (Trace -> ((R i) -> (R (Trace -> ((R j) -> k)))))))))))))))))))))))))))))) -> ((R a) -> ((R b) -> ((R c) -> ((R d) -> ((R e) -> ((R f) -> ((R g) -> ((R h) -> ((R i) -> ((R j) -> k)))))))))))))
-}