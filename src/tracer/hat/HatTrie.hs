module HatTrie (
 insertTrie,
 linearizeExpr,
 getTrieNodes,
 showTrie,showTrieList,
 insertTrieList,
 stringLex,stringLinExpr,showLinList,lmoFun,compareExpr
)

where

import HatTrace
import HatExpression
import Maybe
import Char(isAlpha,isAlphaNum,isDigit)

data LinExpr = LAppl | LConst | LConstr String | LIdent String |
	       LSATA | LSATB | LHidden | LCase | LLambda | -- HatNode | 
	       LInt Int | LInteger Integer | LChar Char | LRational Rational |
               LFloat Float | LDouble Double | LString String | LIf | LGuard |
	       LContainer | --HatNode |
               LFirstArg | LLastArg | LNodeAdr HatNode -- deriving Show

dropLast (_:[]) = []
dropLast (r:l) = r:(dropLast l)
dropLast [] = []

isLAppl LAppl = True
isLAppl _ = False

linearizeExpr :: HatExpression -> [LinExpr]
linearizeExpr e = (linearizeExpr' e)++[LNodeAdr (ref e)]

linearizeExpr' :: HatExpression -> [LinExpr]
linearizeExpr' (HatApplication _ _ fun args _) = 
    let linargs = (foldl (++) [] (map linearizeExpr' args));
	funexpr = (linearizeExpr' fun) in
	 if (isLAppl (head funexpr)) then 
          ((dropLast funexpr)++linargs++[LLastArg]) -- drop last (LLastArg)
          else
          ((LAppl:funexpr))++
	   (LFirstArg:(linargs++[LLastArg]))
linearizeExpr' (HatConstant _ _ fun _) =
    --LConst:  -- no need for LConst: always an LIdent/LConstr or LInt-LString following!
    (linearizeExpr' fun)
linearizeExpr' (HatIdentifier _ name _) = [LIdent name]
linearizeExpr' (HatConstructor _ name _) = [LConstr name]
linearizeExpr' (HatSAT_A _ _) = [LSATA]
linearizeExpr' (HatSAT_B _ _) = [LSATB]
linearizeExpr' (HatHidden _ _) = [LHidden]
linearizeExpr' (HatProj _ _ value) = (linearizeExpr' value)
linearizeExpr' (HatCase _) = [LCase]
linearizeExpr' (HatLambda _) = [LLambda]
linearizeExpr' (HatInt _ i) = [LInt i]
linearizeExpr' (HatInteger _ i) = [LInteger i]
linearizeExpr' (HatChar _ c) = [LChar c]
linearizeExpr' (HatRational _ r) = [LRational r]
linearizeExpr' (HatFloat _ f) = [LFloat f]
linearizeExpr' (HatDouble _ d) = [LDouble d]
linearizeExpr' (HatString _ s) = [LString s]
linearizeExpr' (HatIf _) = [LIf]
linearizeExpr' (HatGuard _) = [LGuard]
linearizeExpr' (HatContainer node) = [LContainer] -- node
linearizeExpr' (HatNone _) = error "Not complete"


data Trie = TAppl [Trie] | TConst [Trie] | TConstr String [Trie] |
	    TIdent String [Trie] | TSATA [Trie] | TSATB [Trie] |
	    THidden [Trie] | TCase [Trie] | TLambda [Trie] | -- HatNode |
	    TInt Int [Trie] | TInteger Integer [Trie] | TChar Char [Trie] |
            TRational Rational [Trie] | TFloat Float [Trie] |
            TDouble Double [Trie] | TString String [Trie] | TIf [Trie] |
            TGuard [Trie] | TContainer [Trie] | --HatNode |
            TFirstArg [Trie] | TLastArg [Trie] | TNodeAdr HatNode -- deriving Show

insertTrie :: [Trie] -> [LinExpr] -> [Trie]
insertTrie tries [] = tries
insertTrie [] (LAppl:r) = [TAppl (insertTrie [] r)]
insertTrie [] (LConst:r) = [TConst (insertTrie [] r)]
insertTrie [] ((LConstr s):r) = [TConstr s (insertTrie [] r)]
insertTrie [] ((LIdent s):r) = [TIdent s (insertTrie [] r)]
insertTrie [] (LSATA:r) = [TSATA (insertTrie [] r)]
insertTrie [] (LSATB:r) = [TSATB (insertTrie [] r)]
insertTrie [] (LHidden:r) = [THidden (insertTrie [] r)]
insertTrie [] (LCase:r) = [TCase (insertTrie [] r)]
insertTrie [] (LLambda:r) = [TLambda (insertTrie [] r)]
insertTrie [] (LInt i:r) = [TInt i (insertTrie [] r)]
insertTrie [] (LInteger i:r) = [TInteger i (insertTrie [] r)]
insertTrie [] (LChar c:r) = [TChar c (insertTrie [] r)]
insertTrie [] (LRational rat:r) = [TRational rat (insertTrie [] r)]
insertTrie [] (LFloat f:r) = [TFloat f (insertTrie [] r)]
insertTrie [] (LDouble d:r) = [TDouble d (insertTrie [] r)]
insertTrie [] (LString s:r) = [TString s (insertTrie [] r)]
insertTrie [] (LIf:r) = [TIf (insertTrie [] r)]
insertTrie [] (LGuard:r) = [TGuard (insertTrie [] r)]
insertTrie [] (LContainer:r) = [TContainer (insertTrie [] r)]
insertTrie [] (LLastArg:r) = [TLastArg (insertTrie [] r)]
insertTrie [] (LFirstArg:r) = [TFirstArg (insertTrie [] r)]
insertTrie [] (LNodeAdr n:[]) = [TNodeAdr n]

insertTrie ((TAppl trie):a) (LAppl:r) = ((TAppl (insertTrie trie r)):a)
insertTrie ((TConst trie):a) (LConst:r) = ((TConst (insertTrie trie r)):a)
insertTrie ((TConstr s1 trie):a) x@((LConstr s2):r) = 
 if (s1==s2) then ((TConstr s1 (insertTrie trie r)):a) else
  (TConstr s1 trie):(insertTrie a x) 
insertTrie ((TIdent s1 trie):a) x@((LIdent s2):r) =
 if (s1==s2) then ((TIdent s1 (insertTrie trie r)):a) else
  (TIdent s1 trie):(insertTrie a x) 
insertTrie ((TSATA trie):a) (LSATA:r) = ((TSATA (insertTrie trie r)):a)
insertTrie ((TSATB trie):a) (LSATB:r) = ((TSATB (insertTrie trie r)):a)
insertTrie ((THidden trie):a) (LHidden:r) = (THidden (insertTrie trie r):a)
insertTrie ((TCase trie):a) (LCase:r) = ((TCase (insertTrie trie r)):a)
insertTrie ((TLambda trie):a) (LLambda:r) = ((TLambda (insertTrie trie r)):a)
insertTrie ((TInt i1 trie):a) x@(LInt i2:r) = 
 if (i1==i2) then ((TInt i1 (insertTrie trie r)):a) else
   (TInt i1 trie):(insertTrie a x)
insertTrie ((TInteger i1 trie):a) x@(LInteger i2:r) = 
 if (i1==i2) then ((TInteger i1 (insertTrie trie r)):a) else
   (TInteger i1 trie):(insertTrie a x)
insertTrie ((TChar c1 trie):a) x@(LChar c2:r) = 
 if (c1==c2) then ((TChar c1 (insertTrie trie r)):a) else
   (TChar c1 trie):(insertTrie a x)
insertTrie ((TRational rat1 trie):a) x@(LRational rat2:r) = 
 if (rat1==rat2) then ((TRational rat1 (insertTrie trie r)):a) else
   (TRational rat1 trie):(insertTrie a x)
insertTrie ((TFloat f1 trie):a) x@(LFloat f2:r) =
 if (f1==f2) then ((TFloat f1 (insertTrie trie r)):a) else
   (TFloat f1 trie):(insertTrie a x)
insertTrie ((TDouble d1 trie):a) x@(LDouble d2:r) =
 if (d1==d2) then ((TDouble d1 (insertTrie trie r)):a) else
   (TDouble d1 trie):(insertTrie a x)
insertTrie ((TString s1 trie):a) x@(LString s2:r) =
 if (s1==s2) then ((TString s1 (insertTrie trie r)):a) else
   (TString s1 trie):(insertTrie a x)
insertTrie ((TIf trie):a) (LIf:r) = ((TIf (insertTrie trie r)):a)
insertTrie ((TGuard trie):a) (LGuard:r) = ((TGuard (insertTrie trie r)):a)
insertTrie ((TContainer trie):a) (LContainer:r) = ((TContainer (insertTrie trie r)):a)
insertTrie ((TFirstArg trie):a) (LFirstArg:r) = ((TFirstArg (insertTrie trie r)):a)
insertTrie ((TLastArg trie):a) (LLastArg:r) = ((TLastArg (insertTrie trie r)):a)
insertTrie x@((TNodeAdr adr1):_) (LNodeAdr adr2:[]) = x
insertTrie (x:a) e = x:(insertTrie a e)

-- data DummyHatExpression = 
--     HatApplication {ref::HatNode,parent::HatExpression,
-- 				     fun::HatExpression,args::[HatExpression],
-- 				     res::HatExpression} |
-- 		     HatConstant {ref::HatNode,parent::HatExpression,
-- 				  fun::HatExpression,res::HatExpression} |
--      		     HatConstructor {ref::HatNode,name::String,infixType::HatInfixType} |
-- 		     HatIdentifier {ref::HatNode,name::String,infixType::HatInfixType} |
-- 		     HatSAT_A {ref::HatNode,trace::HatExpression} |
--                      HatSAT_B {ref::HatNode,trace::HatExpression} |
--                      HatSAT_C {ref::HatNode,trace::HatExpression} |
-- 	             HatHidden {ref::HatNode,parent::HatExpression} |
-- 	             HatProj {ref::HatNode,parent::HatExpression,
-- 			      formerParent::HatExpression} |
--                      HatCase {ref::HatNode} |
-- 	             HatLambda {ref::HatNode} |
-- 	             HatInt {ref::HatNode,valueInt::Int} |
--                      HatChar {ref::HatNode,valueChar::Char} |
--                      HatInteger {ref::HatNode,valueInteger::Integer} |
--                      HatRational {ref::HatNode,valueRational::Rational} |
--                      HatFloat {ref::HatNode,valueFloat::Float} |
-- 	             HatDouble {ref::HatNode,valueDouble::Double} |
--                      HatString {ref::HatNode,valueString::String} |
-- 	             HatIf {ref::HatNode} |
--                      HatGuard {ref::HatNode} |
--                      HatContainer {ref::HatNode} |
--                      HatNone {ref::HatNode}
showLin (LAppl) = "LAppl"
showLin (LConst) = "LConst"
showLin (LConstr s) = "LConstr "++s
showLin (LIdent s) = "LIdent "++s
showLin (LSATA) = "LSATA"
showLin (LSATB) = "LSATB"
showLin (LHidden) = "LHidden"
showLin (LCase) = "LCase"
showLin (LLambda) = "LLambda"
showLin (LInt i) = "LInt "++(show i)
showLin (LInteger i) = "LInteger "++(show i)
showLin (LChar c) = "LChar "++(show c)
showLin (LRational r) = "LRational "++(show r)
showLin (LFloat f) = "LFloat "++(show f)
showLin (LDouble d) = "LDouble "++(show d)
showLin (LString s) = "LString "++s
showLin (LIf) = "LIf"
showLin (LGuard) = "LGuard"
showLin (LContainer) = "LContainer"
showLin (LFirstArg) = "LFirstArg"
showLin (LLastArg) = "LLastArg"
showLin (LNodeAdr node) = "LNodeArg "++(showHatNode node)

showLinList l = "["++(foldl (\x y->(x++", "++y)) [] (map showLin l))++"]"

showTrieList l = "["++(foldl (\x y->(x++", "++y)) [] (map showTrie l))++"]"

showTrie (TAppl l) = "TAppl "++(showTrieList l)
showTrie (TConst l) = "TConst "++(showTrieList l)
showTrie (TConstr s l) = "TConstr "++s++" "++(showTrieList l)
showTrie (TIdent s l) = "TIdent "++s++" "++(showTrieList l)
showTrie (TSATA l) = "TSATA "++(showTrieList l)
showTrie (TSATB l) = "TSATB "++(showTrieList l)
showTrie (THidden l) = "THidden "++(showTrieList l)
showTrie (TCase l) = "TCase "++(showTrieList l)
showTrie (TLambda l) = "TLambda "++(showTrieList l)
showTrie (TInt i l) = "TInt "++(show i)++" "++(showTrieList l)
showTrie (TInteger i l) = "TInteger "++(show i)++" "++(showTrieList l)
showTrie (TChar c l) = "TChar "++(show c)++" "++(showTrieList l)
showTrie (TRational r l) = "TRational "++(show r)++(showTrieList l)
showTrie (TFloat f l) = "TFloat "++(show f)++" "++(showTrieList l)
showTrie (TDouble d l) = "TDouble "++(show d)++" "++(showTrieList l)
showTrie (TString s l) = "TString "++s++" "++(showTrieList l)
showTrie (TIf l) = "TIf "++(showTrieList l)
showTrie (TGuard l) = "TGuard "++(showTrieList l)
showTrie (TContainer l) = "TContainer "++(showTrieList l)
showTrie (TFirstArg l) = "TFirstArg "++(showTrieList l)
showTrie (TNodeAdr node) = "TNodeArg "++(showHatNode node)

-- insertTrieList :: [Trie] -> [[LinExpr]] -> IO([Trie])
-- insertTrieList trie [] = putStrLn ((showTrieList trie)++"\n\n") >> return trie
-- insertTrieList trie (exp:exps) =
--        let s = insertTrie trie exp in
--         (putStrLn ((showTrieList s)++"\n\n")) >>
--           do 
--             v <- (insertTrieList s exps)
--             return v

insertTrieList :: [Trie] -> [([LinExpr],[LinExpr])] -> [Trie]
insertTrieList trie [] = trie
insertTrieList trie ((exp,_):exps) = (insertTrieList (insertTrie trie exp) exps)

getTrieNodes :: [Trie] -> [HatNode]
getTrieNodes t = getTrieNodes' [] t

getTrieNodes' :: [HatNode] -> [Trie] -> [HatNode]
getTrieNodes' s ((TAppl t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TConst t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TConstr _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TIdent _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TSATA t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TSATB t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((THidden t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TCase t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TLambda t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TInt _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TInteger _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TChar _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TRational _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TFloat _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TDouble _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TString _ t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TIf t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TGuard t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TContainer t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TLastArg t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TFirstArg t):r) =  (getTrieNodes' (getTrieNodes' s r) t)
getTrieNodes' s ((TNodeAdr n):r) =  (n:(getTrieNodes' s r))
getTrieNodes' s [] = s

-- main = do
--          hattrace <- openTrace "simple"
-- 	 let observed = (map lazyExpression (observe hattrace "mymap" "" 0));
--              lins = (map linearizeExpr observed) in
--             showReductionList observed >>
--             putStrLn (showLinList (head lins)) >>
--             -- putStrLn (show (
-- 	    do
--               e <- (insertTrieList [] lins)
--               putStrLn ""
-- 	      showReductionList (map lazyExpression (getTrieNodes e))
--          putStrLn ""

compareExpr (LAppl:r1) (LAppl:r2) = compareExpr r1 r2
compareExpr (LConst:r1) (LConst:r2) = compareExpr r1 r2
compareExpr (LConstr s1:r1) (LConstr s2:r2) | s1 == s2 = (compareExpr r1 r2)
                                            | otherwise = False
compareExpr (LIdent s1:r1) (LIdent s2:r2)   | s1 == s2 = (compareExpr r1 r2)
                                            | otherwise = False
compareExpr (LConstr s1:r1) (LIdent s2:r2)  | s1 == s2 = (compareExpr r1 r2)
                                            | otherwise = False
compareExpr (LIdent s1:r1) (LConstr s2:r2)  | s1 == s2 = (compareExpr r1 r2)
                                            | otherwise = False

compareExpr (LSATB:r1) (LSATB:r2) = compareExpr r1 r2
compareExpr (r1) (LSATA:r2) = 
  let r = (dropArgument r1) in
   if (isNothing r) then False else compareExpr (fromJust r) r2
compareExpr (LHidden:r1) (LHidden:r2) = compareExpr r1 r2
compareExpr (LCase:r1) (LCase:r2) = compareExpr r1 r2
compareExpr (LLambda:r1) (LLambda:r2) = False

compareExpr (LInt i:r1) (v:r2) | (toRational i)==(numValue v) 
				   = compareExpr r1 r2
                               | otherwise = False
compareExpr (LInteger i:r1) (v:r2)  | (toRational i)==(numValue v)
					= compareExpr r1 r2
                                    | otherwise = False
compareExpr (LRational r:r1) (v:r2) | r==(numValue v) = compareExpr r1 r2
                                    | otherwise = False
compareExpr (LFloat f:r1) (v:r2)  | (toRational f)==(numValue v) 
				      = compareExpr r1 r2
                                  | otherwise = False
compareExpr (LDouble d:r1) (v:r2) | (toRational d)==(numValue v) 
				      = compareExpr r1 r2
                                  | otherwise = False

compareExpr (LChar c1:r1) (LChar c2:r2) | c1==c2 = compareExpr r1 r2
                                        | otherwise = False
compareExpr (LString s1:r1) (LString s2:r2) | s1==s2 = compareExpr r1 r2
                                            | otherwise = False
compareExpr (LIf:r1) (LIf:r2) = compareExpr r1 r2
compareExpr (LGuard:r1) (LGuard:r2) = compareExpr r1 r2
compareExpr (LContainer:_) (LContainer:_) = False
compareExpr (LFirstArg:r1) (LFirstArg:r2) = compareExpr r1 r2
compareExpr (LLastArg:r1) (LLastArg:r2) = compareExpr r1 r2
compareExpr (LNodeAdr _:r1) (LNodeAdr _:r2) = True
compareExpr (LNodeAdr _:[]) [] = True
compareExpr [] [] = True
compareExpr a b = False -- error ("\n\n\nERROR ERROR False: "++(showLinList a)++", "++(showLinList b))

dropArgument l = dropArgument' l 0 0 -- drop one argument
 where
  dropArgument' (LLastArg:r) i dropped = 
      if (i>0) then dropArgument' r (i-1) 1 else
        if (dropped==0) then Nothing else (Just (LLastArg:r))
  dropArgument' (LAppl:r) i _ = dropArgument' r (i+1) 1-- skip application within argument!
  dropArgument' (_:r) i _ | i==0 = (Just r)
			  | otherwise = dropArgument' r i 1
--  dropArgument' (LNodeAdr _:_) i = Nothing
  dropArgument' [] i _ | i==0 = (Just [])
                       | otherwise = Nothing

-- numValue :: LinExpr -> Num a
numValue (LRational r) = r
numValue (LInt i) = (toRational i)
numValue (LInteger i) = (toRational i)
numValue (LFloat f) = (toRational f)
numValue (LDouble d) = (toRational d)

data ReadMode = NoMode | AlphaMode | SpecialMode | StringMode deriving Eq

stringLex :: String -> [String]
stringLex [] = []
stringLex s =
 let (l,r) = (oneLex NoMode s) in
  if (l/=[]) then l:(stringLex r) else (stringLex r)
 where
  oneLex :: ReadMode -> String->(String,String)
  oneLex _ [] = ([],[])
  oneLex readMode (c:r) | (readMode==StringMode)
                          = if (c=='"') then (c:[],r) else
                            let (l,r2) = oneLex StringMode r in (c:l,r2)
                        | c==' ' = ([],r)
                        | (readMode==NoMode)&&(c=='"')
			  = let (l,r2) = oneLex StringMode r in (c:l,r2)
                        | (readMode==NoMode)&&(c=='\'')
                          = let cs = take 2 r in
                             if (length cs==2)&&(head (tail cs)=='\'') then
                                (c:cs,drop 2 r)
                             else error "Bad character expression!"
                        | (readMode==NoMode)&&(c=='_')&&((take 2 r)=="|_") = ("_|_",(drop 2 r))
                        | (((isAlphaNum c)||(c=='_')||((c=='.')&&(readMode==AlphaMode)))&&
			   (readMode `elem` [NoMode,AlphaMode]))
			  = let (l,r2) = oneLex AlphaMode r in (c:l,r2)
                        | ((c `elem` ['+','-','*','/','!','&','|','=','<','>',':'])&&
			  (readMode `elem` [NoMode,SpecialMode]))
			    = let (l,r2) = oneLex SpecialMode r in (c:l,r2)
                        | (readMode==NoMode)&&(c=='[')&&((take 1 r)=="]") = ("[]",(drop 1 r))
			| c `elem` ['(',')','[',']',',']
			  = if (readMode==NoMode) then (c:[],r) else ([],c:r)
			| otherwise = if (readMode==NoMode) then (c:[],r) else ([],c:r)

lhs :: [String] -> [String]
lhs = takeWhile (\x -> x/="=")

rhs :: [String] -> [String]
rhs l = let r = dropWhile (\x -> x/="=") l in (if (null r) then [] else (tail r))

stringLinExpr :: [String] -> ([LinExpr],[LinExpr])
stringLinExpr [] = ([],[])
stringLinExpr s =
  let l = (lhs s);
      r = (rhs s) in
      (topAppl l,topAppl r)
  where
    topAppl l = if ((length l)>1)&&(((head l) `elem` ["(","["])==False) then
		(LAppl:(lin' [0] [] l))++[LLastArg] else (lin' [] [] l)
    lin' funs brackets ("(":r) = LAppl:(lin' (((length brackets)+1):funs)
					(('(',1):brackets) r)
    lin' funs (('(',_):brackets) (")":r) =
       if (funs/=[])&&((head funs)==(length brackets)) then
	  LLastArg:LFirstArg:(lin' (drop 1 funs) brackets r)
        else
          LLastArg:(lin' funs brackets r)
    lin' funs _ ("(":r) = error "Parenthesis mismatch!"
    lin' funs brackets ("[":r) =
	       LAppl:LConstr ":":LFirstArg:(lin' funs (('[',1):brackets) r)
    lin' funs (('[',c):brackets) ("]":r) = 
	       (LConstr "[]":(replicate c LLastArg))++(lin' funs brackets r)
    lin' funs _ ("]":_) = error "Parenthesis mismatch!"
    lin' funs (('[',c):brackets) (",":r) =
	       LAppl:LConstr ":":LFirstArg:(lin' funs (('[',c+1):brackets) r)
    lin' funs brackets (('\'':c:'\'':[]):r) = LChar c:(lin' funs brackets r)
    lin' funs brackets (('"':s):r) = (makeString s)++(lin' funs brackets r)
    lin' funs brackets (s:r) =
      if (funs/=[])&&((head funs)==(length brackets)) then
       (token' s):LFirstArg:(lin' (drop 1 funs) brackets r) else
       (token' s):(lin' funs brackets r)
    lin' funs brackets [] =
      if (null brackets) then [] else error "Unbalanced parenthesis!"
    token' "_|_" = LSATB
    token' "_" = LSATA
    token' all@(c:_) =
      if (isDigit c) then
	 -- (LIdent all)
         (LRational (toRational (convertToRational all)))
       else (LConstr all)
    makeString [] = LConstr "[]":[]
    makeString "\"" = LConstr "[]":[]
    makeString (c:r) = (LAppl:LConstr ":":LFirstArg:LChar c:(makeString r))++(LLastArg:[])
   
    

convertToRational :: String -> Double
convertToRational s = (read s)

lmoFun :: [LinExpr] -> String
lmoFun [] = []
lmoFun (LConstr s:_) = s
lmoFun (_:r) = lmoFun r


