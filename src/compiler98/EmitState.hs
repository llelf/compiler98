{- ---------------------------------------------------------------------------
Mini-interpreter for pretty-printing bytecodes into C array declarations 
-}
module EmitState where

import Char (isLower)
import GcodeLow (foreignfun)
#if defined(__HASKELL98__)
import List (isPrefixOf)
#else
import IsPrefixOf
#endif

--  , {-type-} EmitState
--  , emitState
--  , startEmitState

-- accumulators:
--   (1) current absolute word offset
--   (2) current relative byte offset
--   (3) current incomplete word
--   (4) label defns
--   (5) bytecode in C

infixl >|>

data EmitState  = ES Int Int Incomplete [Label] ShowS
type Incomplete = (ShowS,ShowS,ShowS,ShowS)
data Label = Define GL String Int | Use String Int
data GL = Global | Local

eszero = showString "0"

empty :: Incomplete
empty = (eszero,eszero,eszero,eszero)

first :: ShowS -> Incomplete
first x = (x,eszero,eszero,eszero)

preSym :: ShowS
preSym = showString "startLabel"

startEmitState :: EmitState
startEmitState =
    ES 0 0 empty []
        (showString "\nstatic Node " . preSym . showString "[] = {\n ")

emitByte :: ShowS -> EmitState -> EmitState
emitByte a (ES n 0  word     labs code) = ES n 1 (first a) labs code
emitByte a (ES n 1 (w,x,y,z) labs code) = ES n 2 (w,a,y,z) labs code
emitByte a (ES n 2 (w,x,y,z) labs code) = ES n 3 (w,x,a,z) labs code
emitByte a (ES n 3 (w,x,y,z) labs code) = ES n 4 (w,x,y,a) labs code
emitByte a (ES n 4  word     labs code) =
    ES (n+1) 1 (first a) labs (outBytes word code)

emitWord :: ShowS -> EmitState -> EmitState
emitWord a (ES n 0 word labs code) =
    ES (n+1) 0 empty labs (outWord a code)
emitWord a (ES n b word labs code) =
    ES (n+2) 0 empty labs (outWord a (outBytes word code))

emitString :: String -> EmitState -> EmitState
emitString = foldr (>|>) (emitByte (shows 0)) . map (emitByte.shows.fromEnum)

emitAlign :: EmitState -> EmitState
emitAlign es@(ES n 0 word labs code) = es
emitAlign (ES n b word labs code) =
    ES (n+1) 0 empty labs (outBytes word code)

emitAlignDouble :: EmitState -> EmitState
emitAlignDouble es@(ES n 0 word labs code)
    | n`div`2 ==0   = es
    | otherwise     = ES (n+1) 0 word labs (outBytes empty code)
emitAlignDouble (ES n b word labs code) =
    emitAlignDouble (ES (n+1) 0 empty labs (outBytes word code))

defineLabel :: GL -> ShowS -> EmitState -> EmitState
defineLabel Local  sym (ES n b word labs code) =
    ES n b word (Define Local  (sym "") (n*4+b): labs)
        (code . showString "\t/* " . sym . showString ": (byte " .
         shows b . showString ") */\n ")
defineLabel Global sym (ES n 0 word labs code) =
    ES n 0 word (Define Global (sym "") (n*4): labs)
        (code . showString "};\nNode " . sym . showString "[] = {\n ")
defineLabel Global ss es = defineLabel Global ss (emitAlign es)

useLabel :: ShowS -> EmitState -> EmitState
useLabel sym (ES n b word labs code) =
    emitWord (wrapUse sym) (ES n b word (Use (sym "") (n*4+b): labs) code)

mentionLabel :: ShowS -> EmitState -> EmitState
mentionLabel sym (ES n b word labs code) =
    ES n b word (Use (sym "") (n*4+b): labs) code

wrapUse :: ShowS -> ShowS
wrapUse sym = showString "useLabel(" . sym . showChar ')'

outBytes :: Incomplete -> ShowS -> ShowS
outBytes (w,x,y,z) code =
    code .
    showString " bytes2word(" .
    w . showChar ',' .
    x . showChar ',' .
    y . showChar ',' .
    z . showString ")\n,"

outWord :: ShowS -> ShowS -> ShowS
outWord x code = code . showChar ' ' . x . showString "\n,"

(>|>) :: (a->a) -> (a->a) -> (a->a)
left >|> right = right . left

emitState :: EmitState -> String
emitState es =
  let (ES _ _ _ rlabs code) = emitAlign es
      isLocal (Define Local _ _) = True
      isLocal  _ = False
      isUse (Use _ _) = True
      isUse  _ = False
      isDefine (Define _ _ _) = True
      isDefine  _ = False
      labs = reverse rlabs
      locals  = filter isLocal labs
      defines = filter isDefine labs
      uses    = filter isUse labs
      externs = filter (\use-> notElemBy useAfterDef use defines) uses
      notElemBy :: (a->b->Bool) -> a -> [b] -> Bool
      notElemBy ok x = all (not . ok x)
      useAfterDef (Use sym use) (Define Local  sym' def) = (sym==sym')
      useAfterDef (Use sym use) (Define Global sym' def) = (sym==sym') &&
                                                           (use>=def)
      doLocal (Define Local sym def) = showString "#define " . showString sym .
                                       showString "\t((void*)" .
                                       preSym . showChar '+' . shows def .
                                       showString ")\n"
      doExtern (Use sym _)
	-- This is a dreadful hack for distinguishing primitives from bytecode!
        | isLower (head sym) = showString "extern void *" . showString sym .
                               showString "();\n"
	-- It is somewhat easier to distinguish foreign imports.
        | foreignfun `isPrefixOf` sym = showString "void " . showString sym .
                               showString "(void);\n"
	-- If nothing else, it must be bytecode.
        | otherwise          = showString "extern Node " . showString sym .
                               showString "[];\n"
      endcode = code . showString "};\n"
  in
  (foldr (.) (foldr (.) endcode (map doExtern externs)) (map doLocal locals)
  ) ""
{- End EmitState -------------------------------------------------------------}