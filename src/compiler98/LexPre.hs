module LexPre(lexPre,PosTokenPre(..)) where

import Char

import Extra(snd3)
import Error(errorLC)
import Lex
import LexLow
import LexStr	-- includes: tab,lexChr
import PackedString(PackedString,packString,unpackPS)
import TokenId(TokenId,t_List,t_Arrow,t_Pair,t_Tuple)

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

type PosTokenPre = (PackedString,Int,Int,Lex)

lexPre :: Bool -> PackedString -> String -> [PosTokenPre]
lexPre u file l = iPreLex u file 1 1 l

------- local

iPreLex :: Bool -> PackedString -> Int -> Int -> String -> [PosTokenPre]
iPreLex u file r c []              = [(file,r,0,L_EOF)]
iPreLex u file r c ('\n':xs)       = iPreLex u file (r+1) 1 xs
iPreLex u file r c ('\^M':'\n':xs) = iPreLex u file (r+1) 1 xs	-- DOS line-end
iPreLex u file r c ('\^M':xs)      = iPreLex u file (r+1) 1 xs	-- Mac line-end
iPreLex u file r c (' ':xs)        = iPreLex u file r (c+1) xs
iPreLex u file r c ('\xa0':xs)     = iPreLex u file r (c+1) xs	-- &nbsp;
iPreLex u file r c ('\t':xs)       = iPreLex u file r (tab c) xs
iPreLex u file r c ('-':'-':xs)
  | null munch || isSpace nextchr || nextchr `elem` ",()[]{};\"'`"
     || isAlphanum nextchr = skipline (iPreLex u file (r+1) 1) xs
	where
		munch = dropWhile (=='-') xs
                nextchr = head munch
		skipline :: (String->[PosTokenPre]) -> String -> [PosTokenPre]
		skipline cont [] = cont []
		skipline cont ('\n':r) = cont r
		skipline cont (_:r) = skipline cont r
iPreLex u file r c ('{':'-':'#':xs) =
                case words xs of
                  ("LINE":lineno:newfile:"#-}":_) | all isDigit lineno ->
                          case lexInteger 10 0 lineno of
                            (_,newr,_) ->
                               iPreLex u (packString newfile)
                                         (fromInteger newr) 1
                                         (tail (dropWhile (/='\n') xs))
                  ("NEED":_) ->
                          (file,r,c,L_LANNOT) :iPreLex u file r (c+3) xs
                  ("#-}":_) ->
                          (file,r,c,L_LANNOT) :iPreLex u file r (c+3) xs
                  (name:_) | all isDigit name ->	-- e.g. fn arity
                          (file,r,c,L_LANNOT) :iPreLex u file r (c+3) xs
                  _ ->    skipcomment u file 0 r (c+3) xs
iPreLex u file r c ('#':'-':'}':xs) =
                (file,r,c,L_RANNOT): iPreLex u file r (c+3) xs

iPreLex u file r c ('{':'-':xs)     = skipcomment u file 0 r (c+2) xs
iPreLex u file r c ('(':xs) | isTupleId xs =
   case span (==',') xs of
     (commas,')':xs) -> 
 	case length commas of
	-- unit ()
	  0 -> (file,r,c,L_ACONID (t_Tuple 0)):  iPreLex u file r (c+3) xs
	-- (n+1)-tuple 
	  n -> (file,r,c,L_ACONID (t_Tuple (n+1))): iPreLex u file r (c+n+2) xs
 where
   isTupleId xs =
      case dropWhile (==',') xs of
	(')':_) -> True
	_ -> False

iPreLex u file r c ('(':'-':'>':')':xs) = (file,r,c,L_ACONID t_Arrow)
                                                   : iPreLex u file r (c+4) xs
iPreLex u file r c ('(':xs) = (file,r,c,L_LPAR)    : iPreLex u file r (c+1) xs
iPreLex u file r c (')':xs) = (file,r,c,L_RPAR)    : iPreLex u file r (c+1) xs
iPreLex u file r c (',':xs) = (file,r,c,L_COMMA)   : iPreLex u file r (c+1) xs
iPreLex u file r c ('{':xs) = (file,r,c,L_LCURL)   : iPreLex u file r (c+1) xs
iPreLex u file r c ('}':xs) = (file,r,c,L_RCURL)   : iPreLex u file r (c+1) xs
iPreLex u file r c ('[':']':xs) = (file,r,c,L_ACONID t_List)
                                                   : iPreLex u file r (c+2) xs
iPreLex u file r c ('[':xs) = (file,r,c,L_LBRACK)  : iPreLex u file r (c+1) xs
iPreLex u file r c (']':xs) = (file,r,c,L_RBRACK)  : iPreLex u file r (c+1) xs
iPreLex u file r c ('`':xs) = (file,r,c,L_BACKTICK): iPreLex u file r (c+1) xs
iPreLex u file r c (';':xs) = (file,r,c,L_SEMI)    : iPreLex u file r (c+1) xs
iPreLex u file r c ('#':xs) | c == 1 =
       case span ('\n' /=) xs of
         (line,xs) ->
            case words line of
              (line:file:_) | all isDigit line ->
                case lexInteger 10 0 line of
                  (_,r,_) ->
                      iPreLex u (packString file) (fromInteger r) 1 (tail xs)
              _ -> error ("Unknown preprocessor directive at line " ++ show r
                         ++ ( case show file of {
                                "\"\"" -> [];
                                file   -> " in file " ++ file } )
                         ++ "\n" ++ line ++ "\n")
iPreLex u file r c ('"':xs) = (file,r,c,L_STRING st): iPreLex u file r' c' xs'
	where (r',c',st,xs') = lexStr r (c+1) xs
iPreLex u file r c ('\'':xs)= (file,r,c,L_CHAR ch)  : iPreLex u file r' c' xs'
	where (r',c',ch,xs') = lexChr r (c+1) xs

iPreLex u file r c ('_':[]) = (file,r,c,L_Underscore): iPreLex u file r (c+1) []
iPreLex u file r c xxs@('_':xs@(x:_)) =
	if isAlpha x 
	then case lexId u r c xxs of
	        (r,c',lex,xs) -> (file,r,c,lex) : iPreLex u file r c' xs
	else
	   (file,r,c,L_Underscore) :  iPreLex u file r (c+1) xs
iPreLex u file r c (xs@(x:s))=
	if isLexId x
	then case lexId u r c xs of
	        (r,c',lex,xs) -> (file,r,c,lex) : iPreLex u file r c' xs
	else if isDigit x
	then case lexNum r c xs of
	        (r,c',lex,xs) -> (file,r,c,lex) : iPreLex u file r c' xs
        else
          (file,r,c,L_ERROR x) :  iPreLex u file r c s




-- Auxiliary used by more than one clause of iPreLex (originally a local defn)
skipcomment :: Bool -> PackedString ->Int ->Int ->Int ->String ->[PosTokenPre]
skipcomment u file n r c xs = skip n r c xs
  where
    skip :: Int -> Int -> Int -> String -> [PosTokenPre]
    skip n r c []           = iPreLex u file r c []
    skip n r c ('-':'}':xs) = if n > 0 
				 then skip (n-1) r (c+2) xs
				 else iPreLex u file r (c+2) xs
    skip n r c ('{':'-':xs) = skip (n+1) r   (c+2) xs
    skip n r c ('\n':xs)    = skip n    (r+1)    1 xs
    skip n r c ('\t':xs)    = skip n     r (tab c) xs
    skip n r c (_:xs)       = skip n     r   (c+1) xs
