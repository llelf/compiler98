module GcodeLowC
  ( gcodeGather
  , gcodeCHeader
  ) where

import Char

import Gcode
--import GcodeLow
import Extra(strStr,splitIntegral,SplitIntegral(..))
import IntState(strIS,IntState,dummyIntState)
import EmitState
import Prim(strPrim)
import Machine
#if defined(__HBC__) || defined(__GLASGOW_HASKELL__)
import Native
#elif defined(__NHC__)
import NhcFloats
#endif

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

fun  = showString "FN_"

offsetSize i = if i >=256 || i<= -256 then 2 else 1 

shortNeedheap :: Int -> (Bool,String)
shortNeedheap i = (i <= 32,"32")
shortNeedstack :: Int -> (Bool,String)
shortNeedstack i = (i <= 16,"16")
shortPush :: Int -> (Bool,String)
shortPush i = (i==1,"1")
shortPop :: Int -> (Bool,String)
shortPop i = (i==1,"1")
shortPushArg :: Int -> (Bool,String)
shortPushArg i = (1<= i && i <= 3, show i)
shortZapArg :: Int -> (Bool,String)
shortZapArg i = (1<= i && i <= 3, show i)
shortHeapCval :: Int -> (Bool,String)
shortHeapCval i = (i == -3 || (3<= i && i <= 5), if i == -3 then "N3" else show i)
shortHeap :: Int -> (Bool,String)
shortHeap i = (i==1 || i==2, show i)

gcodeSize (NEEDHEAP  i)  = if fst(shortNeedheap i) then 1 else 1 + offsetSize i
gcodeSize (NEEDSTACK  i) = if fst(shortNeedstack i) then 1 else 1 + offsetSize i
gcodeSize (LABEL i)      = 0
gcodeSize (LOCAL s i)    = 0
gcodeSize (GLOBAL s i)   = 0
gcodeSize (JUMP  i)      = 3
gcodeSize (JUMPFALSE i)  = 3		-- DAVID
gcodeSize (PRIMITIVE)    = 1

gcodeSize (PRIM prim) = 1

gcodeSize (NOP)	     = 1
gcodeSize (TABLESWITCH  size pad ls)      = 2 + pad + size * 2     -- DAVID
gcodeSize (LOOKUPSWITCH size pad tls def) = 2 + pad + size * 4 + 2 -- DAVID

{------- DAVID ------------
gcodeSize (MATCHCON) = 1
gcodeSize (MATCHINT) = 1
gcodeSize (JUMPS_T)  = 1
gcodeSize (JUMPTABLE l) = 2
gcodeSize (JUMPS_L)  = 1
gcodeSize (JUMPLENGTH s l) = 4
gcodeSize (JUMPLIST  v l) = 4
--------- DAVID ---------- -}

gcodeSize (ZAP_STACK  i)   = 1 + offsetSize i
gcodeSize (ZAP_ARG  i)   = if fst (shortZapArg i) then 1 else 2

-- Stack
gcodeSize (PUSH_CADR  i)   = 1 + offsetSize i
gcodeSize (PUSH_CVAL  i)   = 1 + offsetSize i
gcodeSize (PUSH_INT  i)    = 1 + offsetSize i
gcodeSize (PUSH_CHAR  i)   = 1 + offsetSize i
gcodeSize (PUSH_ARG  i)    = if fst (shortPushArg i) then 1 else 2
gcodeSize (PUSH      i)    = if fst (shortPush i) then 1 else 1 + offsetSize i
gcodeSize (PUSH_HEAP)      = 1
gcodeSize (POP       i)    = if fst (shortPop i) then 1 else 1 + offsetSize i 
gcodeSize (SLIDE     i)    = 1 + offsetSize i 
gcodeSize (UNPACK    i)    = 2

-- selector
gcodeSize (SELECTOR_EVAL)  = 1
gcodeSize (SELECT     i)   = 2

-- evaluation
gcodeSize (APPLY     i) = 2
gcodeSize (EVAL)        = 1
gcodeSize (RETURN)      = 1
gcodeSize (RETURN_EVAL) = 1

-- Heap
gcodeSize (HEAP_CADR  i)   = 1 + offsetSize i
gcodeSize (HEAP_CVAL  i)   = if fst (shortHeapCval i) then 1 else 1 + offsetSize i
gcodeSize (HEAP_INT  i)    = 1 + offsetSize i
gcodeSize (HEAP_CHAR  i)   = 1 + offsetSize i
gcodeSize (HEAP_ARG  i)    = 2
gcodeSize (HEAP      i)    = if fst (shortHeap i) then 1 else 1 + offsetSize i
gcodeSize (HEAP_OFF  i)    = 1 + offsetSize i

gcodeSize (HEAP_CREATE) = 1
gcodeSize (HEAP_SPACE) = 1

gcodeSize (DATA_CREATE)     = wsize
gcodeSize (DATA_CAPITEM a b ) = 2
gcodeSize (DATA_CONSTHEADER a b)   = wsize
gcodeSize (DATA_W  i)       = wsize
gcodeSize (DATA_F  f)       = if floatIsDouble then 8 else 4
gcodeSize (DATA_S  s)       = wsize
gcodeSize (DATA_D  d)       = 8
gcodeSize (DATA_NOP)        = 0
gcodeSize (DATA_CLABEL i)    = wsize
gcodeSize (DATA_GLB s i)    = wsize
gcodeSize (DATA_VAP i)      = wsize
gcodeSize (DATA_CAP  i s)   = wsize
gcodeSize (DATA_CON  s c)   = wsize
gcodeSize (DATA_CONR s c)   = wsize
gcodeSize (DATA_CONT s c)   = wsize
gcodeSize (DATA_CONW s e)   = wsize
gcodeSize (DATA_CONP s e)   = wsize


gcodeStack g = fst (gcodeNeed 0 g)

gcodeNeed :: Int -> Gcode -> (Int,Int) 
gcodeNeed extra (PUSH_CADR  i) = ( 1,0)
gcodeNeed extra (PUSH_CVAL  i) = ( 1,0)
gcodeNeed extra (PUSH_INT  i)  = ( 1,0)
gcodeNeed extra (PUSH_CHAR  i) = ( 1,0)
gcodeNeed extra (PUSH_ARG  i)  = ( 1,0)
-- gcodeNeed extra (PUSH      i)  = ( 1,0)
gcodeNeed extra (PUSH_HEAP)    = ( 1,0)
gcodeNeed extra (POP       i)  = (-i,0)
-- gcodeNeed extra (SLIDE     i)  = (-i,0)
-- gcodeNeed extra (UNPACK    i)  = (i-1,0)
-- gcodeNeed extra (SELECTOR_EVAL)= ( 1,0)
-- gcodeNeed extra (RETURN)      = (-1,0)
-- gcodeNeed extra (RETURN_EVAL) = (-1,0)

-- gcodeNeed extra (APPLY     i)  = (-i,10+i*(3+extra))   -- Not always correct (10 is a large application but they can be larger)
gcodeNeed extra (HEAP_CADR  i) = (0,1)
gcodeNeed extra (HEAP_CVAL  i) = (0,1)
gcodeNeed extra (HEAP_INT  i)  = (0,1)
gcodeNeed extra (HEAP_CHAR  i) = (0,1)
gcodeNeed extra (HEAP_ARG  i)  = (0,1)
gcodeNeed extra (HEAP      i)  = (0,1)
gcodeNeed extra (HEAP_OFF  i)  = (0,1)
gcodeNeed extra (HEAP_CREATE)  = (0,1)
gcodeNeed extra (HEAP_SPACE)   = (0,1)

gcodeNeed extra (NEEDSTACK i)  = (0,0)
gcodeNeed extra (ALIGN )        = (0,0)
gcodeNeed extra (ALIGN_CONST)   = (0,0)
gcodeNeed extra (DATA_CREATE)   = (0,0)
gcodeNeed extra (DATA_CAPITEM _ _)=(0,0)
gcodeNeed extra (DATA_CONSTHEADER _ _)   = (0,0)
gcodeNeed extra (DATA_W  _)     = (0,0)
gcodeNeed extra (DATA_S  _)     = (0,0)
gcodeNeed extra (DATA_F  _)     = (0,0)
gcodeNeed extra (DATA_D  _)     = (0,0)
gcodeNeed extra (DATA_NOP)      = (0,0)    -- does not generate anything, used after DATA_D to keep 1 DATA/WORD
gcodeNeed extra (DATA_CLABEL _) = (0,0)
gcodeNeed extra (DATA_GLB _ _)  = (0,0)
gcodeNeed extra (DATA_VAP _)    = (0,0)
gcodeNeed extra (DATA_CAP _ _)  = (0,0)
gcodeNeed extra (DATA_CON  _ _) = (0,0)
gcodeNeed extra (DATA_CONR _ _) = (0,0)
gcodeNeed extra (DATA_CONT _ _) = (0,0)
gcodeNeed extra (DATA_CONW _ _) = (0,0)
gcodeNeed extra (DATA_CONP _ _) = (0,0)
-- gcodeNeed extra MATCHCON        = (0,0)	-- DAVID
-- gcodeNeed extra MATCHINT        = (0,0)	-- DAVID
gcodeNeed extra g              = error ("gcodeNeed " ++ strGcode dummyIntState g) 

primStack prim = fst (primNeed 0 prim)

primNeed :: Int -> Prim -> (Int,Int)
primNeed extra (ADD	op) = (-1,opNeed extra op)
primNeed extra (SUB	op) = (-1,opNeed extra op)
primNeed extra (MUL	op) = (-1,opNeed extra op)
primNeed extra (ABS	op) = ( 0,opNeed extra op)
primNeed extra (SIGNUM  op) = ( 0,opNeed extra op)
primNeed extra (EXP	op) = ( 0,opNeed extra op)
primNeed extra (LOG	op) = ( 0,opNeed extra op)
primNeed extra (SQRT	op) = ( 0,opNeed extra op)
primNeed extra (SIN	op) = ( 0,opNeed extra op)
primNeed extra (COS	op) = ( 0,opNeed extra op)
primNeed extra (TAN	op) = ( 0,opNeed extra op)
primNeed extra (ASIN	op) = ( 0,opNeed extra op)
primNeed extra (ACOS	op) = ( 0,opNeed extra op)
primNeed extra (ATAN	op) = ( 0,opNeed extra op)
primNeed extra (SLASH	op) = (-1,opNeed extra op)
primNeed extra (CMP_EQ	op) = (-1,0)
primNeed extra (CMP_NE	op) = (-1,0)
primNeed extra (CMP_LT	op) = (-1,0)
primNeed extra (CMP_LE	op) = (-1,0)
primNeed extra (CMP_GT	op) = (-1,0)
primNeed extra (CMP_GE	op) = (-1,0)
primNeed extra (NEG	op) = ( 0,opNeed extra op)
primNeed extra (QUOT)     = (-1,2+extra)
primNeed extra (REM)      = (-1,2+extra)
primNeed extra (AND)      = (-1,0)
primNeed extra (OR)       = (-1,0)
primNeed extra (NOT)      = ( 0,0)
primNeed extra (ORD)      = ( 0,2+extra)
primNeed extra (CHR)      = ( 0,1+extra)
primNeed extra (SEQ)      = (-1,0)
primNeed extra (STRING)   = ( 0,3+2+2+3*extra)
primNeed extra (HGETC)   = (  0, 2+extra )
primNeed extra (HPUTC)   = ( -1, 2+extra )

opNeed :: Int -> PrimOp -> Int
opNeed extra OpWord   = 2+extra
opNeed extra OpFloat  = 2+extra
opNeed extra OpDouble = 3+extra


----------------------------------------
gcodeCHeader = showString "#include \"newmacros.h\"\n\n"

----------------------------------------
showId state i = fixStr (strIS state i)
 where
  fixStr [] = id
  fixStr (c:cs) =
   if isAlphanum c then
     showChar c . fixStr cs
   else 
     showChar '_' . shows (fromEnum c) . fixStr cs

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
#if defined(__HBC__) || defined(__GLASGOW_HASKELL__)
gcodeCDump state (DATA_F  f)      = {-no need to test if floatIsDouble-}
                                    let bytes = showBytes f [] in
                                    foldr (>|>) id
                                        (map (emitByte.shows.fromEnum) bytes)
gcodeCDump state (DATA_D  d)      = let bytes = showBytes d [] in
                                    foldr (>|>) id
                                        (map (emitByte.shows.fromEnum) bytes)
#elif defined(__NHC__)
gcodeCDump state (DATA_F  f)      = {-if floatIsDouble then
                                      let (h,l) = doubleToInts f in
                                      emitWord (shows h) >|> emitWord (shows l)
                                    else-}
                                      let i = floatToInt f in
                                      emitWord (shows i)
gcodeCDump state (DATA_D  d)      = let (h,l) = doubleToInts d in
                                    emitWord (shows h) >|> emitWord (shows l)
#endif
gcodeCDump state (DATA_NOP)       = id
gcodeCDump state (DATA_CLABEL i)  = useLabel (showCLabel state i)
gcodeCDump state (DATA_GLB s 0)   = useLabel (showString s)
gcodeCDump state (DATA_GLB s i)   = useLabel (showString s . showId state i)
gcodeCDump state (DATA_VAP i)     = let lab = fun . showId state i
                                    in
                                    mentionLabel lab >|>
                                    emitWord (showString "VAPTAG(" .
					      wrapUse lab .
					      showString ")")
gcodeCDump state (DATA_CAP  i s)  = let lab = fun . showId state i
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

