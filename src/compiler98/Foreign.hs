module Foreign
  ( Foreign, toForeign, strForeign
  , ForeignMemo, foreignMemo
  , ImpExp(..)
  ) where

import Maybe (fromJust)
import List (find)
import PackedString (PackedString,unpackPS)
import Syntax
import Info
import NT
import TokenId
import AssocTree
import Extra (mix,warning)
import GcodeLow (fun,foreignfun,fixStr)


data ImpExp  = Imported | Exported
data Foreign = Foreign	ImpExp	-- import or export?
			Bool	-- just a cast?
			Bool	-- generate C prototype?
			String	-- foreign function name
			TokenId -- Haskell function name
			Int	-- arity
			[Arg]	-- argument types
			Res	-- result type

instance Show ImpExp where
  showsPrec p Imported = showString "import"
  showsPrec p Exported = showString "export"

instance Show Foreign where
  showsPrec p (Foreign ie cast proto cname hname arity args res) =
    word "foreign" . space .
    (if cast then word "cast"
     else shows ie . space .  showChar '"' . word cname . showChar '"') .
    space .  shows hname . space . shows arity . showString " :: " .
    showString (mix " -> " (map show args)) .
    showString " -> " . shows res

data Arg = Int8  | Int16  | Int32  | Int64
         | Word8 | Word16 | Word32 | Word64
         | Float | Double | Char   | Bool | PackedString   | Int
         | Addr  | StablePtr | ForeignObj | Unknown String | Unit

instance Show Arg where
  showsPrec p Int8         = showString "FFI.Int8"
  showsPrec p Int16        = showString "FFI.Int16"
  showsPrec p Int32        = showString "FFI.Int32"
  showsPrec p Int64        = showString "FFI.Int64"
  showsPrec p Word8        = showString "FFI.Word8"
  showsPrec p Word16       = showString "FFI.Word16"
  showsPrec p Word32       = showString "FFI.Word32"
  showsPrec p Word64       = showString "FFI.Word64"
  showsPrec p Int          = showString "Prelude.Int"
  showsPrec p Float        = showString "Prelude.Float"
  showsPrec p Double       = showString "Prelude.Double"
  showsPrec p Char         = showString "Prelude.Char"
  showsPrec p Bool         = showString "Prelude.Bool"
  showsPrec p PackedString = showString "PackedString.PackedString"
  showsPrec p Addr         = showString "FFI.Addr"
  showsPrec p StablePtr    = showString "FFI.StablePtr"
  showsPrec p ForeignObj   = showString "FFI.ForeignObj"
  showsPrec p Unit         = showString "Prelude.()"
  showsPrec p (Unknown s)  = showString s

-- Note: as of 2000-10-18, the result can never have an IO type - Pure only.
-- (IO results are created by wrapping auxiliary _mkIOokN around a pure call.)
newtype Res = Pure Arg

instance Show Res where
    showsPrec p (Pure arg)       = shows arg

foreignname hname = showString foreignfun . fixStr (show hname)
localname hname   = showString fun . fixStr (show hname)
----


toForeign :: AssocTree Int Info -> ForeignMemo
              -> CallConv -> ImpExp -> String -> Int -> Int -> Foreign
toForeign symboltable memo (Other callconv) ie cname arity var =
    error ("Foreign calling convention \""++callconv++"\" not supported.")
toForeign symboltable memo callconv ie cname arity var =
    Foreign ie cast proto cname hname arity' args res
  where
    info = fromJust (lookupAT symboltable var)
    hname = tidI info
    (args,res) = searchType symboltable memo info
    cast  = (callconv==Cast)
    proto = (callconv/=Noproto)
    arity' = if arity==length args then arity
             else error ("foreign function: arity does not match: "++
                         "is "++show arity++
                         ", expected "++show (length args)++"\n")

searchType :: AssocTree Int Info -> ForeignMemo -> Info -> ([Arg],Res)
searchType st arrow info =
  let
    toList (NTcons c nts) | c==arrow  = let [a,b] = nts in a: toList b
    toList (NTstrict nt)  = toList nt
    toList nt             = [nt]

    toTid (NTcons c nts)  =
      case lookupAT st c of
        Just i | isRealData i -> let nm = tidI i in
                                 Pure (toArg nm)
               | otherwise    -> toTid (getNT (isRenamingFor st i))
    toTid (NTapp t1 t2)   = toTid t1
    toTid (NTstrict t)    = toTid t
    toTid t = Pure (Unknown (show t))  -- error ("Unrecognised NT: "++show t)
		-- (Pure Unknown) lets polymorphic heap-values across unmolested

    toArg t | t==tInt        = Int
            | t==tWord       = Word32
            | t==tBool       = Bool
            | t==tChar       = Char
            | t==tFloat      = Float
            | t==tDouble     = Double
            | t==tForeign    = ForeignObj
            | t==tStablePtr  = StablePtr
            | t==tAddr       = Addr
            | t==(t_Tuple 0) = Unit	-- no void args, but need void results
            | t==tInt8       = Int8
            | t==tInt16      = Int16
            | t==tInt32      = Int32
            | t==tInt64      = Int64
            | t==tWord8      = Word8
            | t==tWord16     = Word16
            | t==tWord32     = Word32
            | t==tWord64     = Word64
            | t==tPackedString  = PackedString
            | otherwise      =
                    warning ("foreign import/export has non-primitive type: "
                             ++show t++"\n") (Unknown (show t))

    getNT (NewType _ _ _ [nt]) = nt
    getNT (NewType _ _ _ (nt:_)) = nt
    getNT _                    = error ("Unable to retrieve newtype info.")

    dropRes args [res]             = (reverse args, res)
    dropRes args (x:xs)            = dropRes (dropPure x:args) xs

    dropPure (Pure arg) = arg

  in
    (dropRes [] . map toTid . toList . getNT . ntI) info

----
type ForeignMemo = Int

foreignMemo :: AssocTree Int Info -> ForeignMemo
foreignMemo st =
    findFirst (check t_Arrow . lookupAT st) [1..]
  where
    check tid (Just info) | cmpTid tid info  = Just (uniqueI info)
                          | otherwise        = Nothing
	-- If the ident doesn't exist after typecheck, it won't be used!
    check tid Nothing                        = Just 0

findFirst :: (a->Maybe b) -> [a] -> b
-- specification:   findFirst = head . catMaybes . map
findFirst f [] = error "findFirst failed"
findFirst f (x:xs) =
    case f x of
      Just b  -> b
      Nothing -> findFirst f xs

----

strForeign :: Foreign -> ShowS
strForeign f@(Foreign Imported cast proto cname hname arity args res) =
    nl . comment (shows f) . nl .
    (if proto then
        word "extern" . space . cResType res . space . word realcname .
          parens (listsep comma (map cTypename args)) . semi
     else id) .
    word "#ifdef PROFILE" . nl .
    word "static SInfo" . space . word profinfo . space . equals . space .
      opencurly . strquote (word modname) . comma .
                  strquote (shows hname) . comma .
                  strquote (shows res) .
      closecurly . semi .
    word "#endif" . nl .
    word "C_HEADER" . parens (foreignname hname) . space .
    opencurly . nl .
      indent . word "NodePtr nodeptr" . semi .
      cResDecl res .
      listsep semi (zipWith cArg args [1..]) . semi .
      foldr (.) id (zipWith cArgDefn args [1..]) . nl .
      (if cast then cCast arity res
       else if length args == 1 && noarg (head args)
       then cCall realcname 0 res
       else cCall realcname arity res) . nl .
      cFooter profinfo res .
    closecurly . nl
  where
    cArg a n = indent . cArgDecl a n
    realcname =
      case cname of
        "" -> (reverse . unpackPS . extractV) hname
        _  -> cname
    modname = (reverse . unpackPS . extractM) hname
    noarg Unit = True
    noarg _    = False
    profinfo = "pf_"++realcname

strForeign f@(Foreign Exported _ _ cname hname arity args res) =
    nl . comment (shows f) . nl .
    cCodeDecl realcname args res . space .
    opencurly . nl .
      --cResDecl res .
      hCall arity hname args .
      hResult res .
    closecurly . nl
  where
    realcname =
      case cname of
        "" -> (reverse . unpackPS . extractV) hname
        _  -> cname


---- foreign import ----

cArgDecl Unit n =
    comment (cTypename Unit . space . narg n)
cArgDecl arg n =
    cTypename arg . space . narg n

cArgDefn Unit n =
    id
cArgDefn arg n =
    indent . word "nodeptr = C_GETARG1" . parens (shows n) . semi .
    indent . word "IND_REMOVE(nodeptr)" . semi .
    indent . narg n . showString " = " .
    parens (cTypename arg) . cConvert arg . semi

cResDecl (Pure Unit) = id
cResDecl (Pure arg) =
    indent . cTypename arg . space . word "result" . semi

cCall cname arity res =
      indent . (case res of
                --  IOVoid      -> id
                --  Pure [Unit] -> id
                    Pure Unit   -> id
                    _           -> word "result = ") .
      word cname .  parens (listsep comma (map narg [1..arity])) . semi

cCast arity res =
    if arity /= 1 then
      error ("\"foreign import cast\" has wrong arity.")
    else
      indent .
      (case res of
        Pure  _      -> word "result = " . parens (cResType res) .
                                              parens (narg 1) . semi
      )

cFooter profinfo (Pure arg) =
    indent . word "nodeptr = " . hConvert arg (word "result") . semi .
    indent . word "INIT_PROFINFO(nodeptr,&" . word profinfo . word ")" . semi .
    indent . word "C_RETURN(nodeptr)" . semi
 

---- foreign export ----

cCodeDecl cname args res =
    cResType res . space . word cname . space .
      parens (listsep comma (zipWith cArgDecl args [1..]))

cResType (Pure  res )     = cTypename res

hCall arity hname args =
    indent . word "NodePtr nodeptr, vap, args" . squares (shows arity) . semi .
    indent . word "C_CHECK" .
        parens (parens (shows (arity+1) . word "+EXTRA") . word "*2") . semi .
    foldr (.) id (zipWith hArg1 args [1..]) .
    indent . word "vap = Hp" . semi .
    indent . word "*Hp = " . parens (word "Node") . 
        word "C_VAPTAG" . parens (localname hname) . semi .
    indent . word "Hp += 1+EXTRA" . semi .
    foldr (.) id (zipWith hArg2 args [1..]) .
    indent . word "nodeptr = evalExport(vap)" . semi .
    indent . word "IND_REMOVE(nodeptr)" . semi
  where 
    hArg1 arg n = indent . word "args" . squares (shows (n-1)) . space .
                  equals . space . hConvert arg (narg n) . semi
    hArg2 arg n = indent . word "*Hp++ = (Node)args" . squares (shows (n-1)) . semi

hResult (Pure Unit) = id	-- allow for IO ()
hResult res =
    indent . word "return" . space . cResult res . semi
  where
    cResult (Pure arg) = cConvert arg


---- shared between foreign import/export ----

cTypename :: Arg -> ShowS
cTypename Int          = word "HsInt"
cTypename Bool         = word "HsBool"
cTypename Int8         = word "HsInt8"
cTypename Int16        = word "HsInt16"
cTypename Int32        = word "HsInt32"
cTypename Int64        = word "HsInt64"
cTypename Word8        = word "HsWord8"
cTypename Word16       = word "HsWord16"
cTypename Word32       = word "HsWord32"
cTypename Word64       = word "HsWord64"
cTypename Float        = word "float"
cTypename Double       = word "double"
cTypename Char         = word "char"
cTypename PackedString = word "char*"
cTypename Addr         = word "void*"
cTypename StablePtr    = word "StablePtr"
cTypename ForeignObj   = word "void*"
cTypename Unit         = word "void"
cTypename (Unknown _)  = word "NodePtr"	-- for passing Haskell heap values

cConvert :: Arg -> ShowS
cConvert Int          = word "GET_INT_VALUE(nodeptr)"
cConvert Bool         = word "GET_BOOL_VALUE(nodeptr)"
cConvert Int8         = word "GET_8BIT_VALUE(nodeptr)"
cConvert Int16        = word "GET_16BIT_VALUE(nodeptr)"
cConvert Int32        = word "GET_32BIT_VALUE(nodeptr)"
cConvert Int64        = word "nhc_get_64bit_value(nodeptr)"
cConvert Word8        = word "GET_8BIT_VALUE(nodeptr)"
cConvert Word16       = word "GET_16BIT_VALUE(nodeptr)"
cConvert Word32       = word "GET_32BIT_VALUE(nodeptr)"
cConvert Word64       = word "nhc_get_64bit_value(nodeptr)"
cConvert Float        = word "get_float_value(nodeptr)"
cConvert Double       = word "get_double_value(nodeptr)"
cConvert Char         = word "GET_CHAR_VALUE(nodeptr)"
cConvert PackedString = word "nhc_getPackedString(nodeptr)"
cConvert Addr         = word "GET_INT_VALUE(nodeptr)"
cConvert StablePtr    = word "GET_INT_VALUE(nodeptr)"
cConvert ForeignObj   = word "derefForeignObj((ForeignObj*)GET_INT_VALUE(nodeptr))"
cConvert Unit         = word "0"
cConvert (Unknown _)  = word "nodeptr"

hConvert :: Arg -> ShowS -> ShowS
hConvert Int          s = word "nhc_mkInt" . parens s
hConvert Bool         s = word "nhc_mkBool" . parens s
hConvert Int8         s = word "nhc_mkInt8" . parens s
hConvert Int16        s = word "nhc_mkInt16" . parens s
hConvert Int32        s = word "nhc_mkInt32" . parens s
hConvert Int64        s = word "nhc_mkInt64" . parens s
hConvert Word8        s = word "nhc_mkWord8" . parens s
hConvert Word16       s = word "nhc_mkWord16" . parens s
hConvert Word32       s = word "nhc_mkWord32" . parens s
hConvert Word64       s = word "nhc_mkWord64" . parens s
hConvert Float        s = word "nhc_mkFloat" . parens s
hConvert Double       s = word "nhc_mkDouble" . parens s
hConvert Char         s = word "nhc_mkChar" . parens s
hConvert PackedString s = word "nhc_mkString" . parens (word "(char*)" . s)
hConvert Addr         s = word "nhc_mkInt" . parens (word "(int)" . s)
hConvert StablePtr    s = word "nhc_mkInt" . parens (word "(int)" . s)
{- Returning ForeignObj's to Haskell is usually illegal: -}
hConvert ForeignObj   s =
  warning ("foreign import/export should not return ForeignObj type.\n")
                          s
hConvert Unit         s = word "nhc_mkUnit()"
hConvert (Unknown _)  s = s	-- for passing Haskell heap values untouched

castConvert :: Arg -> ShowS -> ShowS
castConvert arg s = hConvert arg (parens (cTypename arg) . s)

openparen  = showChar '('
closeparen = showChar ')'
opencurly  = showChar '{'
closecurly = showChar '}'
parens s   = openparen . s . closeparen
strquote s = showChar '"' . s . showChar '"'
squares s  = showChar '[' . s . showChar ']'
semi       = showChar ';' . nl
nl         = showChar '\n'
space      = showChar ' '
equals     = showChar '='
word       = showString
comma      = showChar ','
dot        = showChar '.'
listsep s x= if length x > 0 then foldr1 (\l r-> l . s . r) x else id
indent     = space . space
narg n     = word "arg" . shows n
comment s  = word "/*" . space . s . space . word "*/"

