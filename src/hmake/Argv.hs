module Argv(Goal(..),DecodedArgs(..),decode,stripGoal) where
import ListUtil(lconcatMap)
#if !defined(__HBC__)
import List (isPrefixOf)
import IO (hPutStrLn,stderr)
#else
import IsPrefixOf
import IO (hPutStr,hPutChar,stderr)
hPutStrLn h x = hPutStr h x >> hPutChar h '\n'
#endif

data Goal = Object  String String
          | Program String

instance Show Goal where
   showsPrec n (Object  name suf) = showString (name++'.':suf)
   showsPrec n (Program name)     = showString name

stripGoal (Object name suf) = name
stripGoal (Program name)    = name

-- The following is the start of an attempt to replace the 12-tuple
-- returned by "decode" with something more meaningful.  Apart from
-- anything else, nhc98 doesn't do 13-tuples, so this is needed if
-- you are to make any extensions to the type.
--
data DecodedArgs =
     Decoded 
	{ modules  :: [Goal]		-- specified target modules
	, pathSrc  :: [String]		-- paths to compilable sources
	, pathPrel :: [String]		-- paths to prelude .hi files
	, zdefs    :: [String]		-- cpp options beginning -Z
	, defs     :: [String]		-- cpp options beginning -D
	, ignoreHi :: [String]		-- .hi files to ignore
	, dflag    :: Bool		-- does compiler have a -d option?
	, quiet    :: Bool		-- option -q for quiet
	, keepPrel :: Bool		-- option -keepPrelude
	, isUnix   :: Bool		-- Unix or RiscOS (!)
	, debug    :: (String->IO ())	-- debugging printf function
	, ifnotopt :: ([String]->String->String)  -- conditional (option unset)
	, ifopt    :: ([String]->String->String)  -- conditional (option set)
	, goalDir  :: String		-- goal Directory for .o files
	, hiSuffix :: String		-- .hi / .T_hi
	, oSuffix  :: String		-- .o  / .T_o / .p_o / .t_o
	}

decode :: [String] -> DecodedArgs
decode progArgs =
  let d = Decoded {
      modules  = (map wrapGoal . filter (not . isflag)) progArgs
    , pathSrc  = (map tail . filter (\v -> head v == 'I')) flags ++
                 (map tail . filter (\v -> head v == 'i')) flags ++
                 if isopt "keepPrelude" then pathPrel d else []
    , pathPrel = (map tail . filter (\v -> head v == 'P')) flags
    , zdefs    = (map tail . filter (\v -> head v == 'Z')) flags
    , defs     = (map tail . filter (\v -> head v == 'D')) flags
    , ignoreHi = (map tail . filter (\v -> head v == 'N')) flags
    , dflag    = isopt "od"
    , quiet    = isopt "q"
    , keepPrel = isopt "keepPrelude"
    , isUnix   = True			-- not (isopt "RiscOS")
    , debug    = if isopt "watch" then (\s->hPutStrLn stderr s)
                 else (\s->return ())
    , ifnotopt = \opts s -> if not (or (map isopt opts)) then s else ""
    , ifopt    = \opts s -> if any isopt opts then s else ""
    , goalDir  = case filter (\v -> head v == 'd') flags of
                     []  -> ""
                     [x] -> tail x
                     _   -> error "Only one -dobjdir allowed!\n" 
    , hiSuffix = (withDefault "hi" (drop 10 . last)
                     . filter ("hi-suffix=" `isPrefixOf`)) flags
    , oSuffix  = (withDefault "o"  (drop  9 . last)
                     . filter ("o-suffix="  `isPrefixOf`)) flags
    }
  in d

 where
  flags = (map tail . filter isflag) progArgs

  isflag [] = False
  isflag (c : _) = c == '+' || c == '-'

  isopt opt = opt `elem` flags

  wrapGoal file =
      let (s,n) = break (=='.') (reverse file)
      in findFirst (\suf -> if s == reverse suf then
                                 Just (Object (reverse (tail n)) suf)
                            else Nothing)
                   (Program file)
                   suffixes
                   
  findFirst :: (a->Maybe b) -> b -> [a] -> b
  findFirst f z []     = z
  findFirst f z (x:xs) = case f x of
                            Just y  -> y
                            Nothing -> findFirst f z xs

  withDefault :: a -> ([a]->a) -> ([a]->a)
  withDefault s f [] = s
  withDefault s f xs = f xs

  suffixes = [ "lhs"	-- literate Haskell
             , "gc"	-- GreenCard
             , "hs"	-- Haskell
             , "o"	-- object file
             , "hi"	-- interface file
             , "T_o"	-- tracing object file
             , "p_o"	-- heap-profiling object file
             , "t_o"	-- time-profiling object file
             , "T_hi"	-- tracing interface file
             ]
