module GcodeLowC
  ( gcodeGather
  , gcodeCHeader
  ) where

#if defined(__HBC__) 
#define NATIVE
#elif defined(__NHC__)
#define NHCFLOAT
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 406
#define NATIVE
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 406
#define FLOAT
#endif

import Char

import Gcode
import GcodeLow (offsetSize
                ,shortNeedheap,shortNeedstack,shortPush,shortPop
                ,shortPushArg,shortZapArg,shortHeapCval,shortHeap
                ,fun,foreignfun,showId)
import Extra(strStr,splitIntegral,SplitIntegral(..))
import IntState(strIS,IntState,dummyIntState)
import EmitState
import Prim(strPrim)
import Machine
#if defined(NATIVE)
import Native
#elif defined(NHCFLOAT)
import NhcFloats
#elif defined(FLOAT)
import Floats
#endif

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

sfun  = showString fun
ffun  = showString foreignfun

----------------------------------------
gcodeCHeader = showString "#include \"newmacros.h\"\n#include \"runtime.h\"\n\n"

----------------------------------------

emitJump j i =
  emitByte (showString j)  >|>
  emitByte (shows l)  >|>
  emitByte (shows h)
 where
  (h,l) = divMod i 256

emitOp op =  emitByte (showString op)

emitOp1 op i =
  emitByte (showString op)  >|>
  emitByte (shows i)

emitOp2 op i j =
  emitByte (showString op)  >|>
  emitByte (shows i)        >|>
  emitByte (shows j)

emitOp12 op i =
  if i < 0 then
    case (-i) `divMod` 256 of
      (0,l) -> emitByte (showString op . showString "_N1") >|>
               emitByte (shows l)
      (h,l) -> emitByte (showString op . showString "_N2") >|>
               emitByte (shows l) >|>
               emitByte (shows h)
  else
    case i `divMod` 256 of
      (0,l) -> emitByte (showString op . showString "_P1") >|>
               emitByte (shows l)
      (h,l) -> emitByte (showString op . showString "_P2") >|>
               emitByte (shows l) >|>
               emitByte (shows h)


shortQ pred defgen opstr arg  =
  case pred arg of
    (True,argstr) -> emitOp (opstr ++ "_I" ++ argstr)
    _             -> defgen opstr arg

gcodeCDump state (ALIGN)         = emitAlign
gcodeCDump state (ALIGN_CONST)   = emitOp "ENDCODE" >|> emitAlignDouble
gcodeCDump state (NEEDHEAP i)    = shortQ shortNeedheap emitOp12 "NEEDHEAP" i
gcodeCDump state (NEEDSTACK i)   = shortQ shortNeedstack emitOp12 "NEEDSTACK" i
gcodeCDump state (LABEL i)       = defineLabel Local (showId state i)
gcodeCDump state (LOCAL s i)     = defineLabel Local
                                               (showString s . showId state i)
gcodeCDump state (GLOBAL s i)    = defineLabel Global
                                               (showString s . showId state i)
gcodeCDump state (JUMP  i)       = emitJump "JUMP" i
gcodeCDump state (JUMPFALSE i)   = emitJump "JUMPFALSE" i		-- DAVID
gcodeCDump state (PRIMITIVE)     = emitOp "PRIMITIVE"
gcodeCDump state (PRIM prim)     = emitOp (strPrim prim)
gcodeCDump state (NOP)	         = emitOp "NOP"
gcodeCDump state (MKIORETURN)    = emitOp "MKIORETURN"	-- MW

gcodeCDump state (TABLESWITCH size pad ls) =		-- DAVID
    emitOp1 "TABLESWITCH" size >|>
    someNops pad >|>
    someLabels ls
gcodeCDump state (LOOKUPSWITCH size pad tls def) =	-- DAVID
    emitOp1 "LOOKUPSWITCH" size >|>
    someNops pad >|>
    someLabels (concatMap (\(f,s) -> [f,s]) tls ++ [def])

gcodeCDump state (ZAP_ARG  i)     = shortQ shortZapArg emitOp1  "ZAP_ARG" i
gcodeCDump state (ZAP_STACK i)    = emitOp12  "ZAP_STACK" i

-- Stack
gcodeCDump state (PUSH_CADR  i)   = emitOp12 "PUSH_CADR" i
gcodeCDump state (PUSH_CVAL  i)   = emitOp12 "PUSH_CVAL" i
gcodeCDump state (PUSH_INT  i)    = emitOp12 "PUSH_INT" i
gcodeCDump state (PUSH_CHAR  i)   = emitOp12 "PUSH_CHAR" i
gcodeCDump state (PUSH_ARG  i)    = shortQ shortPushArg emitOp1  "PUSH_ARG" i
gcodeCDump state (PUSH_ZAP_ARG  i)= shortQ shortPushArg emitOp1  "PUSH_ZAP_ARG" i
gcodeCDump state (PUSH      i)    = shortQ shortPush emitOp12 "PUSH" i
gcodeCDump state (PUSH_HEAP)      = emitOp "PUSH_HEAP"
gcodeCDump state (POP       i)    = shortQ shortPop emitOp12 "POP" i
gcodeCDump state (SLIDE     i)    = emitOp12  "SLIDE" i
gcodeCDump state (UNPACK    i)    = emitOp1  "UNPACK" i

-- selector
gcodeCDump state (SELECTOR_EVAL)  = emitOp "SELECTOR_EVAL"
gcodeCDump state (SELECT     i)   = emitOp1 "SELECT" i

-- evaluation
gcodeCDump state (APPLY     i)    = emitOp1 "APPLY" i
gcodeCDump state (EVAL)           = emitOp "EVAL"
gcodeCDump state (RETURN)         = emitOp "RETURN"
gcodeCDump state (RETURN_EVAL)    = emitOp "RETURN_EVAL"

-- Heap
gcodeCDump state (HEAP_CADR  i)   = emitOp12 "HEAP_CADR" i
gcodeCDump state (HEAP_CVAL  i)   = shortQ shortHeapCval emitOp12 "HEAP_CVAL" i
gcodeCDump state (HEAP_INT  i)    = emitOp12 "HEAP_INT" i
gcodeCDump state (HEAP_CHAR  i)   = emitOp12 "HEAP_CHAR" i
gcodeCDump state (HEAP_ARG  i)    = emitOp1 "HEAP_ARG" i 
gcodeCDump state (HEAP_ARG_ARG i j)  = emitOp2 "HEAP_ARG_ARG" i j
gcodeCDump state (HEAP_ARG_ARG_RET_EVAL i j)  = emitOp2 "HEAP_ARG_ARG_RET_EVAL" i j
gcodeCDump state (HEAP      i)    = shortQ shortHeap emitOp12 "HEAP" i
gcodeCDump state (HEAP_OFF  i)    = emitOp12 "HEAP_OFF" i

gcodeCDump state (HEAP_CREATE)    = emitOp "HEAP_CREATE"
gcodeCDump state (HEAP_SPACE)     = emitOp "HEAP_SPACE"

gcodeCDump state (DATA_CREATE)    = emitWord (showString "0")
gcodeCDump state (DATA_CAPITEM a b) = emitByte (shows b) >|> emitByte (shows a)
gcodeCDump state (DATA_CONSTHEADER a b) = emitWord (showString "HW(" .
						    shows a . showChar ',' .
						    shows b . showString ")")
gcodeCDump state (DATA_W  i)      = emitWord (shows i)
gcodeCDump state (DATA_S  s)      = foldr (>|>) (emitByte (shows 0))
                                          (map (emitByte.shows.fromEnum) s)
#if defined(NATIVE)
gcodeCDump state (DATA_F  f)      = {-no need to test if floatIsDouble-}
                                    let bytes = showBytes f [] in
                                    foldr (>|>) id
                                        (map (emitByte.shows.fromEnum) bytes)
gcodeCDump state (DATA_D  d)      = let bytes = showBytes d [] in
                                    foldr (>|>) id
                                        (map (emitByte.shows.fromEnum) bytes)
#elif defined(NHCFLOAT)
gcodeCDump state (DATA_F  f)      = {-if floatIsDouble then
                                      let (h,l) = doubleToInts f in
                                      emitWord (shows h) >|> emitWord (shows l)
                                    else-}
                                      let i = floatToInt f in
                                      emitWord (shows i)
gcodeCDump state (DATA_D  d)      = let (h,l) = doubleToInts d in
                                    emitWord (shows h) >|> emitWord (shows l)
#elif defined(FLOAT)
gcodeCDump state (DATA_F  f)      = {-if floatIsDouble then
                                      let h = doubleToInt0 f
                                          l = doubleToInt1 f in
                                      emitWord (shows h) >|> emitWord (shows l)
                                    else-}
                                      let i = floatToInt f in
                                      emitWord (shows i)
gcodeCDump state (DATA_D  d)      = let h = doubleToInt0 d
                                        l = doubleToInt1 d in
                                    emitWord (shows h) >|> emitWord (shows l)
#endif
gcodeCDump state (DATA_NOP)       = id
gcodeCDump state (DATA_CLABEL i)  = useLabel (showCLabel state i)
gcodeCDump state (DATA_FLABEL i)  = useLabel (ffun . showId state i)
gcodeCDump state (DATA_GLB s 0)   = useLabel (showString s)
gcodeCDump state (DATA_GLB s i)   = useLabel (showString s . showId state i)
gcodeCDump state (DATA_VAP i)     = let lab = sfun . showId state i
                                    in
                                    mentionLabel lab >|>
                                    emitWord (showString "VAPTAG(" .
					      wrapUse lab .
					      showString ")")
gcodeCDump state (DATA_CAP  i s)  = let lab = sfun . showId state i
                                    in
                                    mentionLabel lab >|>
                                    emitWord (showString "CAPTAG(" .
					      wrapUse lab .
					      showChar ',' .  shows s .
					      showString ")")
gcodeCDump state (DATA_CON  s c)  = emitWord (showString "CONSTR(" .
					      shows c .  showChar ',' .
					      shows s .  showChar ',' .
					      showChar '0' .
					      showString ")")
gcodeCDump state (DATA_CONR s c)  = emitWord (showString "CONSTRR(" .
					      shows c .  showChar ',' .
					      shows s .  showChar ',' .
					      showChar '0' .
					      showString ")")
gcodeCDump state (DATA_CONT s c)  = emitWord (showString "CONSTRT(" .
					      shows c .  showChar ',' .
					      shows s .  showChar ',' .
					      showChar '0' .
					      showString ")")
gcodeCDump state (DATA_CONW s e)  = emitWord (showString "CONSTRW(" .
					      shows s .  showChar ',' .
			 		      shows e .
					      showString ")")
gcodeCDump state (DATA_CONP s e)  = emitWord (showString "CONSTRP(" .
					      shows s .  showChar ',' .
			 		      shows e .
					      showString ")")


someNops :: Int -> EmitState -> EmitState
someNops pad = foldr (>|>) id (take pad (repeat (emitOp "NOP")))

someLabels :: [ Int ] -> EmitState -> EmitState
someLabels cls =
  foldr (>|>) id (map (\l -> emitByte (showString "TOP(" . shows l .
                                       showString ")")   >|>
                             emitByte (showString "BOT(" . shows l .
                                       showString ")")
                      )
                      cls)

----------------------------------------
gcodeGather state es [] = es
gcodeGather state es list =
  (foldr (\a b-> gcodeCDump state a >|> b) emitAlign list) es

