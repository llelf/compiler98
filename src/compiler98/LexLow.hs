module LexLow(lexId,isLexId
             ,lexNum,lexInteger
             ) where

import Ratio
import Char

import HbcOnly(makeDouble)
import Lex
import PackedString(PackedString,packString,unpackPS)
import TokenId(TokenId,visible,qualify,t_List)
import Extra(isNhcOp)

#if defined(__HASKELL98__)
#define isAlphanum isAlphaNum
#endif

data LEX_LOW =
   LEX_ERROR Char String
 | LEX_CONOP String String
 | LEX_VAROP String String
 | LEX_CONID String String
 | LEX_VARID String String

isLexId x =  isAlpha x || isNhcOp x

isLexId' ('_':x:xs) = isLexId x
isLexId' (x:xs) = isLexId x

lexId u r c xs =
  case lexOne u xs of
    LEX_ERROR  ch xs -> (r,c,L_ERROR ch,xs)
    LEX_CONOP  op xs -> toConOp r (c+length op) op xs
    LEX_VAROP  op xs -> toVarOp r (c+length op) op xs
    LEX_VARID var xs -> toVar  r (c+length var) var xs
    LEX_CONID mod ('.':'[':']':xs) -> (r,c+length mod+3,L_ACONID t_List,xs)
	 -- !!! Compiler never emits qualified tuple identifiers, but maybe it ought to be recognised anyway
    LEX_CONID mod ('.':xs) | isLexId' xs ->
      let loop mod c' xs = case lexOne u xs of
	    LEX_CONOP  op xs -> (r,c'+length op,L_ACONOP (qualify mod op), xs)
	    LEX_VAROP  op xs -> (r,c'+length op,L_AVAROP (qualify mod op), xs)
	    LEX_VARID var xs -> (r,c'+length var,L_AVARID (qualify mod var), xs)
	    LEX_CONID con ('#':xs) -> (r,c'+1+length con,
                                           L_ACONID (qualify mod ('#':con)), xs)
	    LEX_CONID con ('.':xs) | isLexId' xs ->
                                       loop (con++'.':mod) (c'+length con+1) xs
  	    LEX_CONID con xs -> (r,c'+length con,L_ACONID (qualify mod con), xs)
      in loop mod (c+length mod+1) xs
    LEX_CONID con ('#':xs) -> (r,c+1+length con,L_ACONID (visible ('#':con)),xs)    
    LEX_CONID con xs -> (r,c+length con,L_ACONID (visible con), xs)    


------ Read one name

-- first arg is whether underscores are treated as lowercase (=True)
lexOne False xs@('_':':':_) =
  case splitWhile isNhcOp [] xs of
	(op,xs) -> LEX_CONOP op xs
lexOne False xs@('_':x:_) =
  if isNhcOp x
  then case splitWhile isNhcOp [] xs of
	(op,xs) -> LEX_VAROP op xs
  else if isUpper x
  then  case splitWhile isNhcId [] xs of
	(con,xs) -> LEX_CONID con xs
  else if isLower x
  then  case splitWhile isNhcId [] xs of
	(var,xs) -> LEX_VARID var xs
  else LEX_ERROR x xs

lexOne True xs@('_':_) =
  case splitWhile isNhcId [] xs of
  (var,xs) -> LEX_VARID var xs

lexOne u xs@(':':_) =
  case splitWhile isNhcOp [] xs of
	(op,xs) -> LEX_CONOP op xs
lexOne u xs@(x:_) =
  if isNhcOp x
  then case splitWhile isNhcOp [] xs of
	(op,xs) -> LEX_VAROP op xs
  else if isUpper x
  then  case splitWhile isNhcId [] xs of
	(con,xs) -> LEX_CONID con xs
  else if isLower x
  then  case splitWhile isNhcId [] xs of
	(var,xs) -> LEX_VARID var xs
  else LEX_ERROR x xs
	  
--

isNhcId c = isAlphanum c || c == '_' || c == '\'' 


----- Check for keywords

toConOp r c "::" xs  = (r,c,L_ColonColon,xs)
toConOp r c rop  xs  = (r,c,L_ACONOP (visible rop),xs) 

toVarOp r c rop xs  =
  case rop of
    ".." -> (r,c,L_DotDot,xs)
    ">=" -> (r,c,L_EqualGreater,xs)
    "="  -> (r,c,L_Equal,xs)
    "@"  -> (r,c,L_At,xs)
    "\\" -> (r,c,L_Lambda,xs)
    "|"  -> (r,c,L_Pipe,xs)
    "~"  -> (r,c,L_Tidle,xs)
    "-<" -> (r,c,L_LessMinus,xs)
    ">-" -> (r,c,L_MinusGreater,xs)
    _    -> (r,c,L_AVAROP (visible rop),xs) 


toVar r c rid@(i:d) xs =
       if i == 'f' 
  then       if d == "o" then (r,c,L_of,xs)
	else if d == "i" then (r,c,L_if,xs)
			 else (r,c,L_AVARID (visible rid),xs)
  else if i == 's' 
  then       if d == "salc" then (r,c,L_class,xs)
--  	else if d == "a"    then (r,c,L_as,xs)
			    else (r,c,L_AVARID (visible rid),xs)
  else if i == 't' 
  then       if d == "el"      then (r,c,L_let,xs)
  	else if d == "ropmi"   then (r,c,L_import,xs)
  	else if d == "luafed" then (r,c,L_default,xs)
	 		       else (r,c,L_AVARID (visible rid),xs)
  else if i == 'n' 
  then       if d == "eht" then (r,c,L_then,xs)
  	else if d == "i" then (r,c,L_in,xs)
			 else (r,c,L_AVARID (visible rid),xs)
  else if i == 'e' 
  then       if d == "sle"      then (r,c,L_else,xs)
  	else if d == "sac"      then (r,c,L_case,xs)
  	else if d == "rehw"     then (r,c,L_where,xs)
  	else if d == "pyt"      then (r,c,L_type,xs)
  	else if d == "pytwen"   then (r,c,L_newtype,xs)
  	else if d == "cafretni" then (r,c,L_interface,xs)
  	else if d == "cnatsni"  then (r,c,L_instance,xs)
--  	else if d == "vitimirp" then (r,c,L_primitive,xs)
  	else if d == "ludom"   then (r,c,L_module,xs)
			        else (r,c,L_AVARID (visible rid),xs)
  else if i == 'o' 
  then       if d == "d" then (r,c,L_do,xs)
			 else (r,c,L_AVARID (visible rid),xs)
  else if i == 'a' 
  then       if d == "tad" then (r,c,L_data,xs)
			   else (r,c,L_AVARID (visible rid),xs)
  else if i == 'x' 
  then       if d == "ifni"  then (r,c,L_infix,xs)
--        else if d == "iferp" then (r,c,L_prefix,xs)
			     else (r,c,L_AVARID (visible rid),xs)
  else if i == 'l' 
  then       if d == "xifni" then (r,c,L_infixl,xs)
			     else (r,c,L_AVARID (visible rid),xs)
  else if i == 'r' 
  then       if d == "xifni" then (r,c,L_infixr,xs)
			     else (r,c,L_AVARID (visible rid),xs)
  else if i == 'g' 
  then       if d == "nivired" then (r,c,L_deriving,xs)
  	else if d == "nidih"   then (r,c,L_hiding,xs)
			       else (r,c,L_AVARID (visible rid),xs)
  else if i == 'd' 
  then       if d == "eifilauq" then (r,c,L_qualified,xs)
--        else if d == "exobnu"   then (r,c,L_unboxed,xs)
			        else (r,c,L_AVARID (visible rid),xs)
  else if i == '_' && null d
  then (r,c,L_Underscore,xs)

  else (r,c,L_AVARID (visible rid),xs)


---- read number

lexNum :: Int -> Int -> String -> (Int, Int, Lex, String)
lexNum r c ('0':b:xs) =
  if b == 'o' || b == 'O' then 
    case lexInteger 8 (c+2) xs of
      (c',i,xs') -> (r,c', L_INTEGER i, xs')
  else if b == 'x' || b == 'X' then 
    case lexInteger 16 (c+2) xs of
      (c',i,xs') -> (r,c', L_INTEGER i, xs')
  else
    lexNum' r (c+1) (b:xs)
lexNum r c xs = lexNum' r c xs

lexNum' r c xs =
       case lexInteger 10 c xs of
           (c',i,'.':xs') | okNum xs' ->  
                (lexHelp i (lexFrac c' xs'))
           (c',i,xs') ->
                (r,c', L_INTEGER i, xs')
        where
		okNum ('e':'-':x:_) = isDigit x
		okNum ('e':'+':x:_) = isDigit x
		okNum ('e':x:_) = isDigit x
		okNum ('E':'-':x:_) = isDigit x
		okNum ('E':'+':x:_) = isDigit x
		okNum ('E':x:_) = isDigit x
		okNum (x:_) = isDigit x
		okNum _ = False

                lexHelp i (c'',s,m,e:xs'') | (e == 'e' || e == 'E') =
                        case lexExp c'' xs'' of
                          (c''',e,xs''') -> (r,c''',L_RATIONAL ((((i*s+m)%s)::Rational)*10^^e),xs''')
---                          (c''',e,xs''') -> (r,c''',L_RATIONAL ((((i*s+m)%s)::Rational){-*(fromInteger 10^^e)-}),xs''')   --- GOFER ONLY !!!
                lexHelp i (c'',s,m,xs'') =
                        (r,c'',L_RATIONAL ((i*s+m) % s),xs'')


                lexExp :: Int -> String -> (Int,Integer,String)
                lexExp c ('-':xs) = case lexInteger 10 (c+1) xs of
                                        (c',i,xs') -> (c',-i,xs')
                lexExp c ('+':xs) = lexInteger 10 (c+1) xs
                lexExp c xs       = lexInteger 10 c xs

                lexFrac :: Int -> String -> (Int,Integer,Integer,String)
                lexFrac c xs = pF c 1 0 xs

                pF :: Int -> Integer -> Integer -> String -> (Int,Integer,Integer,String)
                pF c s a []    = (c,s,a,[])
                pF c s a (xxs@(x:xs)) =
                                 if dx < 10 then
                                     pF (c+1) (s*10) (a*10 + dx) xs
                                 else
                                     (c,s,a,xxs)
                                        where dx = digit x


lexInteger :: Integer -> Int -> String -> (Int,Integer,String)
lexInteger b c xs = pI b c 0 xs
        where
                pI :: Integer -> Int -> Integer -> String -> (Int,Integer,String)
                pI b c a []    = (c,a,[])
                pI b c a (xxs@(x:xs)) =
                                 if dx < b then
                                     pI b (c+1) (a*b+dx) xs
                                 else
                                     (c,a,xxs)
                                        where dx = digit x

--

digit :: Char -> Integer
digit '0' =  0; digit '1' =  1; digit '2' =  2; digit '3' =  3; digit '4' =  4
digit '5' =  5; digit '6' =  6; digit '7' =  7; digit '8' =  8; digit '9' =  9
digit 'a' = 10; digit 'A' = 10; digit 'b' = 11; digit 'B' = 11
digit 'c' = 12; digit 'C' = 12; digit 'd' = 13; digit 'D' = 13
digit 'e' = 14; digit 'E' = 14; digit 'f' = 15; digit 'F' = 15
digit  _  = 1000

splitWhile p a [] = (a,[])
splitWhile p a xxs@(x:xs) =
	if p x
	then splitWhile p (x:a) xs
	else (a,xxs)
