{- ---------------------------------------------------------------------------
Defines data type TokenId for names of all kinds of identifiers.
Also defines tokenIds for identifiers that are hardcoded into the compiler.
-}
module TokenId(module TokenId) where

import Extra(mix,isNhcOp,Pos(..),strPos)
import PackedString(PackedString, unpackPS, packString)


visible rtoken = Visible (packString rtoken)
qualify rmodule rtoken = Qualified (packString rmodule) (packString rtoken)


data TokenId =
     TupleId Int -- no distinction between the type and the value constructor?
   | Visible   PackedString	-- unqualified name
   | Qualified PackedString PackedString
     -- token for qualified name: module name, variable name
   | Qualified2 TokenId TokenId	
     -- token with: class token, type token for a dictionary?
   | Qualified3 TokenId TokenId TokenId
     -- token for method in instance: class token, type token, method token
   deriving (Eq,Ord)


instance Show TokenId where
  showsPrec d (TupleId s) = if s == 0
			    then showString "()"
	                    else showString "Prelude." . shows s
  showsPrec d (Visible n) = showString (reverse (unpackPS n))
  showsPrec d (Qualified m n ) = 
    showString (reverse (unpackPS m)) . showChar '.' . 
    showString (reverse (unpackPS  n))
  showsPrec d (Qualified2 t1 t2) = shows t1 . showChar '.' . shows t2
  showsPrec d (Qualified3 t1 t2 t3) = 
    shows t1 . showChar '.' . shows t2 . showChar '.' . shows t3


isTidOp (TupleId s) = False
isTidOp tid = 
  (isNhcOp . head . dropWhile (=='_') . reverse . unpackPS . extractV) tid

isTupleId (TupleId _)        = True
isTupleId (Qualified2 _ t)   = isTupleId t
isTupleId (Qualified3 _ _ t) = isTupleId t
isTupleId _                  = False

--notPrelude (Qualified tid n) = tid /= rpsDPrelude && tid /= rpsPrelude
notPrelude (Qualified tid n) = tid /= rpsPrelude
notPrelude (Qualified2 t1 t2) = notPrelude t1 && notPrelude t2 
notPrelude (Qualified3 t1 t2 t3) = notPrelude t1 && notPrelude t2 
notPrelude (TupleId _) = False


{- construct Qualified2 token from given two tokens -}
mkQual2 :: TokenId -> TokenId -> TokenId

mkQual2 cls cls_typ = Qualified2 cls cls_typ


{- construct Qualified3 token from given three tokens -}
mkQual3 :: TokenId -> TokenId -> TokenId -> TokenId

mkQual3 cls typ met = Qualified3 cls typ (dropM met)


{- -}
mkQualD :: PackedString -> TokenId -> TokenId

mkQualD rps v@(Visible n) = Qualified3 (Visible rps) t_underscore v
mkQualD rps   (Qualified m v) = Qualified3 (Visible m) t_underscore (Visible v)


{- if token is not qualified make it qualified with given module name -}
ensureM :: PackedString -> TokenId -> TokenId

ensureM tid (Visible n) = Qualified tid n
ensureM tid q = q


{- make token into qualified token with given module name -}
forceM :: PackedString -> TokenId -> TokenId

forceM m (Qualified _ n) = Qualified m n
forceM m (Visible n)     = Qualified m n
forceM m tid = tid


{- drop all qualification (module names) from token -}
dropM :: TokenId -> TokenId

dropM (Qualified tid n) = Visible n
dropM (Qualified2 t1 t2) = t2
dropM (Qualified3 t1 t2 t3) = t3
dropM v = v

{- get module name from token, correct for Visible? -}
extractM :: TokenId -> PackedString

extractM (Qualified tid n) = tid
extractM (Qualified2 t1 t2) = extractM t1
extractM (Qualified3 t1 t2 t3) = extractM t1
extractM v = rpsPrelude


{- get identifier name from token, without qualification -}
extractV :: TokenId -> PackedString

extractV (TupleId n) = packString ('(' : foldr (:) ")" (replicate n ','))
extractV (Visible v) = v
extractV (Qualified m v) =  v
extractV (Qualified2 t1 t2) = extractV t2
extractV (Qualified3 t1 t2 t3) = extractV t3


{- extend token by adding position to the identifier name -}
tidPos :: TokenId -> Pos -> TokenId

tidPos (TupleId s) pos = if s == 0 
		         then visImport ("():" ++ (strPos pos))
	                 else visImport (shows s (':' : strPos pos))
tidPos (Visible n)           pos = 
  Visible (packString (reverse (strPos pos) ++ ':' : unpackPS n))
tidPos (Qualified m n )      pos = 
  Qualified m (packString (reverse (strPos pos) ++ ':' : unpackPS n))
tidPos (Qualified2 t1 t2)    pos =
  Qualified2 t1 (tidPos t2 pos)
tidPos (Qualified3 t1 t2 t3) pos = 
  Qualified3 t1 t2 (tidPos t3 pos)


{- append given string to module name of qualified token -}
add2M :: String -> TokenId -> TokenId

add2M str (Qualified m v) =  
  Qualified (packString (reverse str ++ unpackPS m)) v

visImport = Visible . packString . reverse
qualImpPrel = Qualified rpsPrelude . packString . reverse
qualImpBin = Qualified rpsBinary  . packString . reverse
qualImpRat = Qualified rpsRatio   . packString . reverse
qualImpIx  = Qualified rpsIx      . packString . reverse
qualImpFFI = Qualified rpsFFI     . packString . reverse
qualImpPS  = Qualified rpsPS      . packString . reverse
qualImpIOE = Qualified rpsIOE     . packString . reverse

rpsPrelude      = (packString . reverse ) "Prelude"
rpsBinary       = (packString . reverse ) "Binary"
rpsRatio        = (packString . reverse ) "Ratio"
rpsIx           = (packString . reverse ) "Ix"
rpsFFI          = (packString . reverse ) "FFIBuiltin"
rpsPS           = (packString . reverse ) "PackedString"
rpsIOE          = (packString . reverse ) "IOExtras"


isUnit (TupleId 0) = True
isUnit _ = False


{- make token for tuple of given size -}
t_Tuple :: Int -> TokenId

t_Tuple  size   = TupleId size


tmain = Qualified (packString (reverse "Main")) (packString (reverse "main"))

tPrelude        = Visible rpsPrelude
t_underscore    = visImport  "_"
t_Bang		= visImport "!"
t_Assign	= visImport ":="
tprefix	 	= visImport "prefix"
tqualified 	= visImport "qualified"
thiding	 	= visImport "hiding"
tas	 	= visImport "as"
tinterface 	= visImport "interface"
tforall	 	= visImport "forall"
tdot	 	= visImport "."        
  -- an unqualified dot, used in types, e.g., "forall a . [a]"
tunboxed	= visImport "unboxed"
tprimitive	= visImport "primitive"
tMain           = visImport  "Main"
tUnknown u      = visImport  ("Unexported.Constr_"++show u)

t_gtgteq        = qualImpPrel  ">>="
t_gtgt	        = qualImpPrel  ">>"
tfail	        = qualImpPrel  "fail"
t_error  	= qualImpPrel  "error"
t_undef         = qualImpPrel  "undefined"
tfromInteger    = qualImpPrel  "fromInteger"
tNum            = qualImpPrel  "Num"
tIntegral       = qualImpPrel  "Integral"
tInt            = qualImpPrel  "Int"
tIntHash        = qualImpPrel  "Int#"


t_flip          = qualImpPrel  "flip"
tminus          = qualImpPrel  "-"
tident          = qualImpPrel  "id"
tnegate         = qualImpPrel  "negate"
tTrue           = qualImpPrel  "True"
tFalse          = qualImpPrel  "False"
tunknown        = visImport    "Unknown.variable"
terror          = qualImpPrel  "error"
tIO             = qualImpPrel  "IO"
tBool           = qualImpPrel  "Bool"
tFloatHash      = qualImpPrel  "Float#"
tFloat          = qualImpPrel  "Float"
tChar           = qualImpPrel  "Char"
t_List          = qualImpPrel  "[]"
t_Arrow         = qualImpPrel  "->"
t_Pair          = qualImpPrel  "(,"
tString         = qualImpPrel  "String"
t_filter        = qualImpPrel  "_filter"
t_foldr         = qualImpPrel  "_foldr"
t_Colon         = qualImpPrel  ":"
t_x             = qualImpPrel  "_x"
t_y             = qualImpPrel  "_y"
t_equalstring	= qualImpPrel  "_equalstring"
t_guardstring	= qualImpPrel  "_guardstring"
t_fail		= qualImpPrel  "_fail"
t_fatbar	= qualImpPrel  "_fatbar"
t_select	= qualImpPrel  "_select"
t_patbindupdate = qualImpPrel  "_patbindupdate"
t_callpatbindupdate = qualImpPrel  "_callpatbindupdate"
tDialogue       = qualImpPrel  "Dialogue"
t_apply1        = qualImpPrel  "_apply1"
t_apply2        = qualImpPrel  "_apply2"
t_apply3        = qualImpPrel  "_apply3"
t_apply4        = qualImpPrel  "_apply4"
t_used          = qualImpPrel  "used!"
tInteger        = qualImpPrel  "Integer"
tDouble         = qualImpPrel  "Double"
tDoubleHash     = qualImpPrel  "Double#"
tfromRational   = qualImpPrel  "fromRational"
t_equalinteger	= qualImpPrel  "_equalinteger"
t_guardinteger	= qualImpPrel  "_guardinteger"
t_nopos 	= qualImpPrel  "<no pos>"
t_fromEnum      = qualImpPrel  "_fromEnum"
t_toEnum        = qualImpPrel  "_toEnum"
--tEval           = qualImpPrel  "Eval"		-- Removed in Haskell 98
tEq             = qualImpPrel  "Eq"
tOrd            = qualImpPrel  "Ord"
tEnum           = qualImpPrel  "Enum"
tIx             = qualImpIx    "Ix"
tShow           = qualImpPrel  "Show"
tRead           = qualImpPrel  "Read"
t_andand        = qualImpPrel  "&&"
t_pipepipe      = qualImpPrel  "||"
tcompare	= qualImpPrel  "compare"
tLT		= qualImpPrel  "LT"
tEQ		= qualImpPrel  "EQ"
tGT		= qualImpPrel  "GT"
t_equalequal    = qualImpPrel  "=="
t_lessequal     = qualImpPrel  "<="
t_lessthan      = qualImpPrel  "<"
t_greater       = qualImpPrel  ">"

tseq 	        = qualImpPrel  "seq"

trange          = qualImpIx  "range"
tindex          = qualImpIx  "index"
tinRange        = qualImpIx  "inRange"
t_tupleRange    = qualImpIx  "_tupleRange"
t_tupleIndex    = qualImpIx  "_tupleIndex"
t_enumRange     = qualImpPrel  "_enumRange"
t_enumIndex     = qualImpPrel  "_enumIndex"
t_enumInRange   = qualImpPrel  "_enumInRange"

tfromEnum	= qualImpPrel  "fromEnum"
ttoEnum		= qualImpPrel  "toEnum"
tenumFrom       = qualImpPrel  "enumFrom"
tenumFromTo     = qualImpPrel  "enumFromTo"
tenumFromThen   = qualImpPrel  "enumFromThen"
tenumFromThenTo = qualImpPrel  "enumFromThenTo"
t_enumFromTo    = qualImpPrel  "_enumFromTo"
t_enumFromThenTo= qualImpPrel  "_enumFromThenTo"

tBounded	= qualImpPrel  "Bounded"
tminBound	= qualImpPrel  "minBound"
tmaxBound	= qualImpPrel  "maxBound"

t_append	= qualImpPrel  "++"
t_readCon0	= qualImpPrel  "_readCon0"
t_readConInfix	= qualImpPrel  "_readConInfix"
t_readCon	= qualImpPrel  "_readCon"
t_readConArg	= qualImpPrel  "_readConArg"
t_readField 	= qualImpPrel  "_readField"
t_readFinal 	= qualImpPrel  "_readFinal"

tshowsPrec      = qualImpPrel  "showsPrec"
tshowsType      = qualImpPrel  "showsType"
treadsPrec      = qualImpPrel  "readsPrec"
t_dot           = qualImpPrel  "."
tshowString     = qualImpPrel  "showString"
tshowChar       = qualImpPrel  "showChar"
tshowParen      = qualImpPrel  "showParen"
treadParen      = qualImpPrel  "readParen"
tFractional     = qualImpPrel  "Fractional"
tRational       = qualImpPrel  "Rational"	-- Changed in Haskell 98
tRatio          = qualImpRat  "Ratio"		-- Changed in Haskell 98
tRatioCon       = qualImpRat  "%"		-- Changed in Haskell 98
tPRIMITIVE      = visImport "PRIMITIVE"
tNEED           = visImport "NEED"
t_primitive     = visImport "primitive"
t_Lambda        = qualImpPrel  "\\"
t_eqInteger     = qualImpPrel  "_eqInteger"
t_eqDouble      = qualImpPrel  "_eqDouble"
t_eqFloat       = qualImpPrel  "_eqFloat"
t_otherwise	= qualImpPrel  "otherwise"

t_id            = qualImpPrel  "_id"   
  -- identity function that is not modified by the tracing transformation


{- Malcolm's additions from here on -}

{- class + instances of Binary -}
tBinary		= qualImpBin  "Binary"
t_put	        = qualImpBin  "put"
t_get           = qualImpBin  "get"
t_getF          = qualImpBin  "getF"
t_sizeOf        = qualImpBin  "sizeOf"
t_putBits       = qualImpBin  "putBits"
t_getBits       = qualImpBin  "getBits"
t_getBitsF      = qualImpBin  "getBitsF"
t_ltlt          = qualImpBin  "<<"
t_return        = qualImpPrel  "return"
t_plus          = qualImpPrel  "+"

{- (N+K) patterns -}
t_nplusk        = visImport   "+"
t_subtract      = qualImpPrel  "subtract"

{- FFI -}
t_foreign	= visImport   "foreign"
t_export	= visImport   "export"
t_ccall		= visImport   "ccall"
t_stdcall	= visImport   "stdcall"
t_unsafe	= visImport   "unsafe"
t_noproto	= visImport   "noproto"
t_cast		= visImport   "cast"
tForeign	= qualImpFFI  "ForeignObj"
tAddr		= qualImpFFI  "Addr"
tStablePtr	= qualImpFFI  "StablePtr"
tWord		= qualImpFFI  "Word"
tInt8		= qualImpFFI  "Int8"
tInt16		= qualImpFFI  "Int16"
tInt32		= qualImpFFI  "Int32"
tInt64		= qualImpFFI  "Int64"
tWord8		= qualImpFFI  "Word8"
tWord16		= qualImpFFI  "Word16"
tWord32		= qualImpFFI  "Word32"
tWord64		= qualImpFFI  "Word64"
tPackedString	= qualImpPS   "PackedString"
tunsafePerformIO= visImport   "unsafePerformIO"

{- more FFI -}
t_mkIOok n      = visImport   ("_mkIOok"++show (n::Int))
t_mkIOwf n      = visImport   ("_mkIOwf"++show (n::Int))

{- End TokenId -------------------------------------------------------------}
