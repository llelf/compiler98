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
     TupleId   Int
   | Visible   PackedString
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

extractV (Visible v) = v
extractV (Qualified m v) =  v
extractV (Qualified2 t1 t2) = extractV t2
extractV (Qualified3 t1 t2 t3) = extractV t3


{- extend token by adding position to the identifier name -}
tidPos :: TokenId -> Pos -> TokenId

tidPos (TupleId s) pos = if s == 0 
		         then visImpRev ("():" ++ (strPos pos))
	                 else visImpRev (shows s (':' : strPos pos))
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

visImpRev = Visible . packString . reverse
qualImpRev = Qualified rpsPrelude . packString . reverse
qualImpBin = Qualified rpsBinary  . packString . reverse
qualImpRat = Qualified rpsRatio   . packString . reverse
qualImpFFI = Qualified rpsFFI     . packString . reverse
qualImpPS  = Qualified rpsPS      . packString . reverse
--pqualImpRev = Qualified rpsPrelude . packString . reverse

rpsPrelude      = (packString . reverse ) "Prelude"
rpsBinary       = (packString . reverse ) "Binary"
rpsRatio        = (packString . reverse ) "Ratio"
--rpsDPrelude      = (packString . reverse ) "DPrelude"
rpsFFI          = (packString . reverse ) "FFIBuiltin"
rpsPS           = (packString . reverse ) "PackedString"


isUnit (TupleId 0) = True
isUnit _ = False


{- make token for tuple of given size -}
t_Tuple :: Int -> TokenId

t_Tuple  size   = TupleId size


tmain = Qualified (packString (reverse "Main")) (packString (reverse "main"))

tPrelude        = Visible rpsPrelude
t_underscore    = visImpRev  "_"
t_Bang		= visImpRev "!"
t_Assign	= visImpRev ":="
tprefix	 	= visImpRev "prefix"
tas	 	= visImpRev "as"
tforall	 	= visImpRev "forall"
tdot	 	= visImpRev "."        
  -- an unqualified dot, used in types, e.g., "forall a . [a]"
tunboxed	= visImpRev "unboxed"
tprimitive	= visImpRev "primitive"
tMain           = visImpRev  "Main"

t_gtgteq        = qualImpRev  ">>="
t_gtgt	        = qualImpRev  ">>"
t_zero	        = qualImpRev  "fail"
t_error  	= qualImpRev  "error"
t_undef         = qualImpRev  "undefined"
tfromInteger    = qualImpRev  "fromInteger"
tNum            = qualImpRev  "Num"
tIntegral       = qualImpRev  "Integral"
tInt            = qualImpRev  "Int"
tIntHash        = qualImpRev  "Int#"


tminus          = qualImpRev  "-"
tident          = qualImpRev  "id"
tnegate         = qualImpRev  "negate"
tTrue           = qualImpRev  "True"
tFalse          = qualImpRev  "False"
tunknown        = qualImpRev  "?unknown?"
terror          = qualImpRev  "error"
tIO             = qualImpRev  "IO"
tBool           = qualImpRev  "Bool"
tFloatHash      = qualImpRev  "Float#"
tFloat          = qualImpRev  "Float"
tChar           = qualImpRev  "Char"
t_List          = qualImpRev  "[]"
t_Arrow         = qualImpRev  "->"
t_Pair          = qualImpRev  "(,"
tString         = qualImpRev  "String"
t_filter        = qualImpRev  "_filter"
t_foldr         = qualImpRev  "_foldr"
t_Colon         = qualImpRev  ":"
t_x             = qualImpRev  "_x"
t_y             = qualImpRev  "_y"
t_equalstring	= qualImpRev  "_equalstring"
t_guardstring	= qualImpRev  "_guardstring"
t_fail		= qualImpRev  "_fail"
t_fatbar	= qualImpRev  "_fatbar"
t_select	= qualImpRev  "_select"
t_patbindupdate = qualImpRev  "_patbindupdate"
t_callpatbindupdate = qualImpRev  "_callpatbindupdate"
tDialogue       = qualImpRev  "Dialogue"
t_apply1        = qualImpRev  "_apply1"
t_apply2        = qualImpRev  "_apply2"
t_apply3        = qualImpRev  "_apply3"
t_apply4        = qualImpRev  "_apply4"
t_used          = qualImpRev  "used!"
tInteger        = qualImpRev  "Integer"
tDouble         = qualImpRev  "Double"
tDoubleHash     = qualImpRev  "Double#"
tfromRational   = qualImpRev  "fromRational"
t_equalinteger	= qualImpRev  "_equalinteger"
t_guardinteger	= qualImpRev  "_guardinteger"
t_nopos 	= qualImpRev  "<no pos>"
t_fromEnum      = qualImpRev  "_fromEnum"
t_toEnum        = qualImpRev  "_toEnum"
--tEval           = qualImpRev  "Eval"		-- Removed in Haskell 98
tEq             = qualImpRev  "Eq"
tOrd            = qualImpRev  "Ord"
tEnum           = qualImpRev  "Enum"
tIx             = qualImpRev  "Ix"
tShow           = qualImpRev  "Show"
tRead           = qualImpRev  "Read"
t_andand        = qualImpRev  "&&"
t_pipepipe      = qualImpRev  "||"
tcompare	= qualImpRev  "compare"
tLT		= qualImpRev  "LT"
tEQ		= qualImpRev  "EQ"
tGT		= qualImpRev  "GT"
t_equalequal    = qualImpRev  "=="
t_lessequal     = qualImpRev  "<="
t_lessthan      = qualImpRev  "<"
t_greater       = qualImpRev  ">"

tseq 	        = qualImpRev  "seq"

trange          = qualImpRev  "range"
tindex          = qualImpRev  "index"
tinRange        = qualImpRev  "inRange"
t_tupleRange    = qualImpRev  "_tupleRange"
t_tupleIndex    = qualImpRev  "_tupleIndex"
t_enumRange     = qualImpRev  "_enumRange"
t_enumIndex     = qualImpRev  "_enumIndex"
t_enumInRange   = qualImpRev  "_enumInRange"

tfromEnum	= qualImpRev  "fromEnum"
ttoEnum		= qualImpRev  "toEnum"
tenumFrom       = qualImpRev  "enumFrom"
tenumFromTo     = qualImpRev  "enumFromTo"
tenumFromThen   = qualImpRev  "enumFromThen"
tenumFromThenTo = qualImpRev  "enumFromThenTo"
t_enumFromTo    = qualImpRev  "_enumFromTo"
t_enumFromThenTo= qualImpRev  "_enumFromThenTo"

tBounded	= qualImpRev  "Bounded"
tminBound	= qualImpRev  "minBound"
tmaxBound	= qualImpRev  "maxBound"

t_append	= qualImpRev  "++"
t_readCon0	= qualImpRev  "_readCon0"
t_readConInfix	= qualImpRev  "_readConInfix"
t_readCon	= qualImpRev  "_readCon"
t_readConArg	= qualImpRev  "_readConArg"
t_readField 	= qualImpRev  "_readField"
t_readFinal 	= qualImpRev  "_readFinal"

tshowsPrec      = qualImpRev  "showsPrec"
tshowsType      = qualImpRev  "showsType"
treadsPrec      = qualImpRev  "readsPrec"
t_dot           = qualImpRev  "."
tshowString     = qualImpRev  "showString"
tshowChar       = qualImpRev  "showChar"
tshowParen      = qualImpRev  "showParen"
treadParen      = qualImpRev  "readParen"
tFractional     = qualImpRev  "Fractional"
tRational       = qualImpRev  "Rational"	-- Changed in Haskell 98
tRatio          = qualImpRat  "Ratio"		-- Changed in Haskell 98
tRatioCon       = qualImpRat  "%"		-- Changed in Haskell 98
tPRIMITIVE      = visImpRev "PRIMITIVE"
tNEED           = visImpRev "NEED"
t_primitive     = visImpRev "primitive"
t_Lambda        = qualImpRev  "\\"
t_eqInteger     = qualImpRev  "_eqInteger"
t_eqDouble      = qualImpRev  "_eqDouble"
t_eqFloat       = qualImpRev  "_eqFloat"
t_otherwise	= qualImpRev  "otherwise"

t_id            = qualImpRev  "_id"   
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
t_return        = qualImpRev  "return"
t_plus          = qualImpRev  "+"

{- n plus k patterns -}
t_nplusk        = visImpRev   "+"
t_subtract      = qualImpRev  "subtract"

{- ffi -}
t_foreign	= visImpRev   "foreign"
t_export	= visImpRev   "export"
t_ccall		= visImpRev   "ccall"
t_stdcall	= visImpRev   "stdcall"
t_unsafe	= visImpRev   "unsafe"
t_cast		= visImpRev   "cast"
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

{- End TokenId -------------------------------------------------------------}
