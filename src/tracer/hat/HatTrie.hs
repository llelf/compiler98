module HatTrie (
 insertTrie,
 linearizeExpr,linearizeEquation,
 getTrieNodes,
 showTrie,showTrieList,
 insertTrieList,
 stringLex,stringLinExpr,showLinList,lmoFun,compareExpr,
 LinExpr,Trie
)

where

import HatTrace
import HatExpression
import Maybe
import Char(isAlpha,isAlphaNum,isDigit)

data LinExprElement = LAppl | LConstr String | LIdent String |
	       LSATA | LSATB | LHidden | LCase | LLambda |
	       LInt Int | LInteger Integer | LChar Char | LRational Rational |
               LFloat Float | LDouble Double | LString String | LIf | LGuard |
	       LContainer |
               LFirstArg | LLastArg | LRHS | LNodeAdr HatNode | LNone deriving Show
type LinExpr = [LinExprElement]

data TrieElement = TAppl Trie | TConstr String Trie |
	    TIdent String Trie | TSATA Trie | TSATB Trie |
	    THidden Trie | TCase Trie | TLambda Trie |
	    TInt Int Trie | TInteger Integer Trie | TChar Char Trie |
            TRational Rational Trie | TFloat Float Trie |
            TDouble Double Trie | TString String Trie | TIf Trie |
            TGuard Trie | TContainer Trie |
            TFirstArg Trie | TLastArg Trie | TRHS Trie |
	    TNodeAdr HatNode | TNone Trie
type Trie = [TrieElement]


dropLast :: [a] -> [a]
dropLast (_:[]) = []
dropLast (r:l) = r:(dropLast l)
dropLast [] = []

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- convert an equation to a linear representation: a list of constructors
linearizeEquation :: HatExpression -> LinExpr
linearizeEquation appl = (linearizeExpr' appl)++(LRHS:(linearizeExpr' (res appl)))++
			 [LNodeAdr (ref appl)]

-- convert an expression to a linear representation
linearizeExpr :: HatExpression -> LinExpr
linearizeExpr e = (linearizeExpr' e)++[LNodeAdr (ref e)]

linearizeExpr' :: HatExpression -> LinExpr
linearizeExpr' (HatApplication _ _ fun args _) = 
    let linargs = (foldl (++) [] (map linearizeExpr' args)); -- convert arguments
	funexpr = (linearizeExpr' fun) in                    -- convert function
	 if (isLAppl' (head funexpr)) then                   -- flat representation
          ((dropLast funexpr)++linargs++[LLastArg])    -- get rid of LLastArg in funexpr
          else -- function is not an application: enclose arguments with First-/LastArg
          ((LAppl:funexpr))++(LFirstArg:(linargs++[LLastArg]))
     where isLAppl' LAppl = True
	   isLAppl' _ = False

linearizeExpr' (HatConstant _ _ fun _) = (linearizeExpr' fun)
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
linearizeExpr' (HatNone node) = [LNone]
linearizeExpr' _ = error "linearizeExpr': unknown constructor"


---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- insert a linearized expression into a trie
--  - deal with more/less general equations
--  - deal with unevalauted subexpressions within lhs or rhs correctly!


insertTrie :: Trie -> LinExpr -> Trie
insertTrie t l = insertTrie' False t l

insertTrie' :: Bool -> Trie -> LinExpr -> Trie

insertTrie' _ tries [] = tries -- LinExpr empty: trie unmodified

-- if trie is empty, simply add the new elements
insertTrie' rhs [] (LAppl:r) = [TAppl (insertTrie' rhs [] r)]
insertTrie' rhs [] ((LConstr s):r) = [TConstr s (insertTrie' rhs [] r)]
insertTrie' rhs [] ((LIdent s):r) = [TIdent s (insertTrie' rhs [] r)]
insertTrie' rhs [] (LSATA:r) = [TSATA (insertTrie' rhs [] r)]
insertTrie' rhs [] (LSATB:r) = [TSATB (insertTrie' rhs [] r)]
insertTrie' rhs [] (LHidden:r) = [THidden (insertTrie' rhs [] r)]
insertTrie' rhs [] (LCase:r) = [TCase (insertTrie' rhs [] r)]
insertTrie' rhs [] (LLambda:r) = [TLambda (insertTrie' rhs [] r)]
insertTrie' rhs [] (LInt i:r) = [TInt i (insertTrie' rhs [] r)]
insertTrie' rhs [] (LInteger i:r) = [TInteger i (insertTrie' rhs [] r)]
insertTrie' rhs [] (LChar c:r) = [TChar c (insertTrie' rhs [] r)]
insertTrie' rhs [] (LRational rat:r) = [TRational rat (insertTrie' rhs [] r)]
insertTrie' rhs [] (LFloat f:r) = [TFloat f (insertTrie' rhs [] r)]
insertTrie' rhs [] (LDouble d:r) = [TDouble d (insertTrie' rhs [] r)]
insertTrie' rhs [] (LString s:r) = [TString s (insertTrie' rhs [] r)]
insertTrie' rhs [] (LIf:r) = [TIf (insertTrie' rhs [] r)]
insertTrie' rhs [] (LGuard:r) = [TGuard (insertTrie' rhs [] r)]
insertTrie' rhs [] (LContainer:r) = [TContainer (insertTrie' rhs [] r)]
insertTrie' rhs [] (LLastArg:r) = [TLastArg (insertTrie' rhs [] r)]
insertTrie' rhs [] (LFirstArg:r) = [TFirstArg (insertTrie' rhs [] r)]
insertTrie' rhs [] (LRHS:r) = [TRHS (insertTrie' True [] r)]
insertTrie' rhs [] (LNodeAdr n:[]) = [TNodeAdr n]
insertTrie' rhs [] (LNone:r) = [TNone (insertTrie' rhs [] r)]

-- a new address is ignored, if it matches the position of an old address:
--  the equations have been the same so far! (or even less general)
insertTrie' rhs x@((TNodeAdr _):_) (LNodeAdr _:[]) = x

-- if a SATA is found in the Trie or in the LinExpr, use mostGeneralTrie to
--  deal with more/less general equations correctly
insertTrie' rhs trie@((TSATA _):_) linexpr =
    mostGeneralTrie rhs trie linexpr
insertTrie' rhs trie linexpr@(LSATA:_) =
    mostGeneralTrie rhs trie linexpr

-- if RHS constructor is found (rhs of equation follows) set rhs mode to true!
insertTrie' _ ((TRHS t1):a) (LRHS:b) = (TRHS (insertTrie' True t1 b):a)

-- if element in Trie matches the one in LinExpr: add rest in its Trie
insertTrie' rhs (e1:a) (e2:b) | (sameType e1 e2) = 
				  ((typesConstr e1) (insertTrie' rhs (typesTrie e1) b):a)

-- trie and linExpr are different: check other possibilities in the trie
insertTrie' rhs (x:a) e = x:(insertTrie' rhs a e)


-- state for comparison
data CompareState = NoState | MoreGeneral | LessGeneral | UncompState deriving Eq

-- deal with more- or less general equations, when inserting into the trie

mostGeneralTrie :: Bool -> Trie -> LinExpr -> Trie

mostGeneralTrie rhs trielist linexpr@(LSATA:l) =
-- SATA in new expression: drop one argument within trie and search ALL subtries!
--  while searching: clear all less general equations within the trie!
 let (state,ntrie) = dropTrieArgument rhs trielist l in
   if ((state==NoState)||(state==LessGeneral)) then trielist else
      (TSATA (insertTrie' rhs [] l)):ntrie

mostGeneralTrie rhs trielist@((TSATA t):r) linexpr =
-- SATA in trie: if rhs==True, new expression might be more general (so clear all
--   less general equations in the trie
--   if rhs==False: check, whether it's a different equation or whether it's just
--   less general than the ones already in the trie
 let (state,trie) = compareTrie rhs NoState trielist linexpr in
   if state==LessGeneral then
       trielist  -- the new equation is less general: return the original trie
     else 
      (TSATA t):(insertTrie' rhs r linexpr)
      -- it seems more general: so add the new equation to the Trie

-- skip one argument within trie and check ALL its subtries
dropTrieArgument :: Bool -> Trie -> LinExpr -> (CompareState,Trie)
dropTrieArgument rhs l expr = dropOne' rhs l 0 0 expr

-- drop first argument of every element in this trie and check its subtries
dropOne' _ [] _ _ _ = (UncompState,[]) -- nothing to drop left! Uncomparable!
dropOne' rhs (a:tlist) i dropped expr =
    let (fstate,first) =  -- drop argument of first trie
	    dropTrieArgument' rhs a i dropped expr;
        (rstate,rest)  =  -- drop first arguments of all remaining tries
	    dropOne' rhs tlist i dropped expr in
		       (compState fstate rstate,first++rest)

dropTrieArgument' rhs all@(TRHS t) appldepth dropped expr
    | (appldepth>0)||(dropped==0) = 
  -- RHS reached, but still have to drop an argument! => impossible
	(UncompState,[all])

dropTrieArgument' rhs all@(TLastArg t) i dropped expr = 
      if (i>0) then -- still within an application, need to drop more elements
        let (nstate,ntrie) = dropOne' rhs t (i-1) 1 expr in
         if (null ntrie) then -- nothing left in this subtrie!
		(nstate,[]) 
	  else (nstate,[TLastArg ntrie]) -- return remaining subtrie
       else
        if (dropped==0) then  -- nothing dropped yet! => can't continue here...
	       (UncompState,[all])
         else  -- ok, compare and filter the remaining expression
	 compareTrie rhs (if rhs then LessGeneral else MoreGeneral) [(TLastArg t)] expr

dropTrieArgument' rhs (TAppl t) i _ expr =
    -- dropping application: remember to drop all its arguments! (inc application depth)
    let (nstate,ntrie) = dropOne' rhs t (i+1) 1 expr in
     if (null ntrie) then (nstate,[]) -- nothing left: return nothing
	else (nstate,[TAppl ntrie]) -- return Application with remaining subtrie

dropTrieArgument' rhs e appldepth _ expr
 | appldepth==0 = let (nstate,ntrie) = -- compare and filter its subtries
			  (compareTrie rhs 
			   (if rhs then LessGeneral else MoreGeneral) 
			   (typesTrie e) expr) in
   if (null ntrie) then (nstate,[])  -- no subtries left: return nothing
      else (nstate,[((typesConstr e) ntrie)]) -- else: return node with its subtries
 | otherwise = -- appldepth not 0, so keep on dropping elements
     dropOne' rhs (typesTrie e) appldepth 1 expr

-- compareTrie's states: LessGeneral: new element is bigger (= less general) (so far) than
--                         the ones in the trie
--                       MoreGeneral: new element is smaller (= more general) (so far)
--                         than the ones in the trie
--                       UncompState: new element is not comparable to trie, neither
--                         less general nor more general
--                       NoState: No comparison so far

-- compare states and return a combination of both
compState :: CompareState -> CompareState -> CompareState
compState MoreGeneral _ = MoreGeneral
compState _ MoreGeneral = MoreGeneral
compState LessGeneral _ = LessGeneral
compState _ LessGeneral = LessGeneral
compState NoState _ = NoState
compState _ NoState = NoState
compState _ _        = UncompState

-- compare a new linexpr to a trie: remove all less general elements in trie,
-- and return the resulting state afterwards: new element can be MoreGeneral,
-- LessGeneral or Uncomparable
compareTrie :: Bool -> CompareState -> Trie -> LinExpr -> (CompareState,Trie)
compareTrie _ state [] [] = (UncompState,[]) -- no match found with any element in trie
compareTrie rhs state [] (LNodeAdr _:[]) =
    -- NoState=Equal so far (treat as LessGeneral)
    (if state==NoState then LessGeneral else state,[])
compareTrie rhs _     [] _  = (UncompState,[])
compareTrie rhs _     t  [] = (UncompState,t)
compareTrie rhs state all@(e1:t) linexp@(e2:r) | (sameType e1 e2) =
  -- same element here: check with this element's subtrie
  let trie = typesTrie e1; -- get element's subtrie
      nrhs = (rhs || (isRHS' e2)); -- are on rhs, if was rhs or this element is RHS
      (nstate,newt) = -- check subtrie against rest of expression
	     compareTrie nrhs state trie r in
      if ((nstate==LessGeneral)||(nstate==NoState)) then
	     (state,all)  -- new element was LessGeneral or equal: finished, return old
	 else  -- no: new element was MoreGeneral or Uncomparable: check rest of trie!
	    let (s2,t2) = compareTrie rhs state t linexp in
			  ((compState nstate s2), -- combine states
			   if (null newt) then t2  -- nothing left in first subtrie
			   else (((typesConstr e1) newt):t2)) -- add first subtrie
    where
     isRHS' LRHS = True
     isRHS' _ = False

compareTrie rhs state all@(TSATA trie:t) linexp
    | ((rhs==False)&&(state /= MoreGeneral))||  
    -- SATA found: ok, if on lhs and MoreGeneral so far
    -- or if on rhs and LessGeneral so far
      ((rhs==True)&&(state /= LessGeneral)) =
      let l = dropArgument linexp in  -- drop one argument of linexpr
     if (isNothing l) then -- nothing in linexpr?
       let (nstate,newt) = compareTrie rhs state t linexp in -- compare with other tries
	 (nstate,(TSATA trie):newt)  -- return first element and filtered rest
      else
        let (nstate,newt) = compareTrie rhs (if rhs then MoreGeneral else LessGeneral)
			    trie (fromJust l) in  -- compare with remaining expression
          if (nstate==LessGeneral) then (LessGeneral,all) else  -- LessGeneral: finished!
	    let (state2,newt) = compareTrie rhs state t linexp in
				-- compare with remaining tries
				(compState nstate state2,(TSATA trie):newt)

compareTrie rhs state all linexp@(LSATA:l)
    | ((rhs==False)&&(state /= LessGeneral))|| -- ok, if on lhs and LessGeneral so far
      ((rhs==True)&&(state /=MoreGeneral)) =   -- or on rhs and MoreGeneral so far
  let (nstate,ntrie) = dropTrieArgument rhs all l in 
  -- drop one argument in trie and compare
   if (nstate==LessGeneral) then (LessGeneral,all) else  -- if lessGeneral: finished
      (nstate,ntrie)  -- else return the filtered result

compareTrie rhs state (t:trie) linexpr =  -- ok, first elements are uncomparable
  let (nstate,newt) = compareTrie rhs state trie linexpr in -- compare with others
    (compState UncompState nstate,t:newt) -- return first element and filtered rest

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- insert a list of LinExpressions into a Trie, return resulting trie
insertTrieList :: Trie -> [LinExpr] -> Trie
insertTrieList trie [] = trie
insertTrieList trie (exp:exps) = (insertTrieList (insertTrie trie exp) exps)


-- get all node addresses stored within the trie
getTrieNodes :: Trie -> [HatNode]
getTrieNodes t = getTrieNodes' [] t
 where
  getTrieNodes' :: [HatNode] -> Trie -> [HatNode]
  getTrieNodes' s ((TNodeAdr n):r) =  (n:(getTrieNodes' s r))
  getTrieNodes' s (e:r) = (getTrieNodes' (getTrieNodes' s r) (typesTrie e))
  getTrieNodes' s [] = s

---------------------------------------------------------------------------
---------------------------------------------------------------------------

-- compare two LinExpressions: if first expressions matches the pattern of
-- the second, return true else false
compareExpr :: LinExpr -> LinExpr -> Bool
compareExpr (LAppl:r1) (LAppl:r2) = compareExpr r1 r2
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
compareExpr (LNone:r1) (LNone:r2) = True
compareExpr (LInt i:r1) (v:r2) | (sameValue (toRational i) v) 
				   = compareExpr r1 r2
                               | otherwise = False
compareExpr (LInteger i:r1) (v:r2)  | (sameValue (toRational i) v)
					= compareExpr r1 r2
                                    | otherwise = False
compareExpr (LRational r:r1) (v:r2) | (sameValue r v) = compareExpr r1 r2
                                    | otherwise = False
compareExpr (LFloat f:r1) (v:r2)  | (sameValue (toRational f) v) 
				      = compareExpr r1 r2
                                  | otherwise = False
compareExpr (LDouble d:r1) (v:r2) | (sameValue (toRational d) v) 
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
compareExpr _ (LLastArg:[]) = True
compareExpr (LRHS:_) [] = True
compareExpr (LRHS:r1) (LRHS:r2) = compareExpr r1 r2
compareExpr r1 (LRHS:r2) =
    let rhs = (dropWhile findRHS' r1) in
	if (null rhs) then False else
           compareExpr rhs (tail rhs)
    where findRHS' LRHS = True
          findRHS' _ = False

compareExpr [] [] = True
compareExpr a b = False -- error ("\n\n\nERROR ERROR False: "++(showLinList a)++", "++(showLinList b))

dropArgument :: LinExpr -> Maybe LinExpr
dropArgument l = dropArgument' l 0 0 -- drop one argument
 where
  dropArgument' all@(LRHS:_) i dropped =
      if (i>0)||(dropped==0) then Nothing else (Just all)
  dropArgument' all@(LLastArg:r) i dropped = 
      if (i>0) then dropArgument' r (i-1) 1 else
        if (dropped==0) then Nothing else (Just all)
  dropArgument' (LAppl:r) i _ = dropArgument' r (i+1) 1-- skip application within argument!
  dropArgument' (_:r) i _ | i==0 = (Just r)
			  | otherwise = dropArgument' r i 1
  dropArgument' [] i _ | i==0 = (Just [])
                       | otherwise = Nothing

sameValue :: Rational -> LinExprElement -> Bool
sameValue r1 (LRational r) = r1==r
sameValue r1 (LInt i)      = r1==(toRational i)
sameValue r1 (LInteger i)  = r1==(toRational i)
sameValue r1 (LFloat f)    = r1==(toRational f)
sameValue r1 (LDouble d)   = r1==(toRational d)
sameValue _ _              = False

---------------------------------------------------------------------------
---------------------------------------------------------------------------

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

stringLinExpr :: [String] -> LinExpr -- (LinExpr,LinExpr)
stringLinExpr [] = [] -- ([],[])
stringLinExpr s =
  let l = (lhs s);
      r = (rhs s) in
      if (null r) then topAppl l else
	 (topAppl l)++(LRHS:(topAppl r))
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
         (LRational (toRational (convertToRational all)))
       else (LConstr all)
    makeString [] = LConstr "[]":[]
    makeString "\"" = LConstr "[]":[]
    makeString (c:r) = (LAppl:LConstr ":":LFirstArg:LChar c:(makeString r))++(LLastArg:[])



convertToRational :: String -> Double
convertToRational s = (read s)

lmoFun :: LinExpr -> String
lmoFun [] = []
lmoFun (LConstr s:_) = s
lmoFun (_:r) = lmoFun r

---------------------------------------------------------------------------
---------------------------------------------------------------------------

sameType :: TrieElement -> LinExprElement -> Bool
sameType (TAppl _)  LAppl  = True
sameType (TConstr s1 _) (LConstr s2) = s1==s2
sameType (TIdent s1 _) (LIdent s2) = s1==s2
sameType (TSATA _) LSATA = True
sameType (TSATB _) LSATB = True
sameType (THidden _) LHidden = True
sameType (TCase _)   LCase = True
sameType (TLambda _) LLambda = True
sameType (TInt i1 _) (LInt i2) = i1==i2
sameType (TInteger i1 _) (LInteger i2) = i1==i2
sameType (TChar c1 _) (LChar c2) = c1==c2
sameType (TRational r1 _) (LRational r2) = r1==r2
sameType (TFloat f1 _) (LFloat f2) = f1==f2
sameType (TDouble d1 _) (LDouble d2) = d1==d2
sameType (TString s1 _) (LString s2) = s1==s2
sameType (TIf _) LIf = True
sameType (TGuard _) LGuard = True
sameType (TContainer _) LContainer = True
sameType (TFirstArg _) LFirstArg = True
sameType (TLastArg _) LLastArg = True
sameType (TRHS _) LRHS = True
sameType (TNodeAdr _) (LNodeAdr _) = True
sameType (TNone _) LNone = True
sameType _ _ = False


typesTrie :: TrieElement -> Trie
typesTrie (TAppl t)   = t
typesTrie (TConstr _ t) = t
typesTrie (TIdent _ t)  = t
typesTrie (TSATA t)   = t
typesTrie (TSATB t)     = t
typesTrie (THidden t)   = t
typesTrie (TCase t)     = t
typesTrie (TLambda t)   = t
typesTrie (TInt _ t)    = t
typesTrie (TInteger _ t)= t
typesTrie (TChar _ t)   = t
typesTrie (TRational _ t)= t
typesTrie (TFloat _ t)  = t
typesTrie (TDouble _ t) = t
typesTrie (TString _ t) = t
typesTrie (TIf t)       = t
typesTrie (TGuard t)    = t
typesTrie (TContainer t)= t
typesTrie (TFirstArg t) = t
typesTrie (TLastArg t)  = t
typesTrie (TRHS t)      = t
typesTrie (TNone t)     = t
typesTrie _             = []

typesConstr :: TrieElement -> (Trie -> TrieElement)
typesConstr  (TAppl _)   = TAppl
typesConstr (TConstr s _) = TConstr s
typesConstr (TIdent s _)  = TIdent s
typesConstr (TSATA _)   = TSATA
typesConstr (TSATB _)     = TSATB
typesConstr (THidden _)   = THidden
typesConstr (TCase _)     = TCase
typesConstr (TLambda _)   = TLambda
typesConstr (TInt i _)    = TInt i
typesConstr (TInteger i _)= TInteger i
typesConstr (TChar c _)   = TChar c
typesConstr (TRational r _)= TRational r
typesConstr (TFloat f _)  = TFloat f
typesConstr (TDouble d _) = TDouble d
typesConstr (TString s _) = TString s
typesConstr (TIf _)       = TIf
typesConstr (TGuard _)    = TGuard
typesConstr (TContainer _)= TContainer
typesConstr (TFirstArg _) = TFirstArg
typesConstr (TRHS _) = TRHS
typesConstr (TLastArg _)  = TLastArg
typesConstr (TNone _)   = TNone

---------------------------------------------------------------------------
---------------------------------------------------------------------------

showLin :: LinExprElement -> String
showLin (LAppl) = "LAppl"
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
showLin (LRHS) = "LRHS"
showLin (LNone) = "LNone"
showLin (LNodeAdr node) = "LNodeArg "++(show node)

showLinList l = "["++(foldl (\x y->(x++", "++y)) [] (map showLin l))++"]"

showTrieList l = "["++(foldl (\x y->(x++", "++y)) [] (map showTrie l))++"]"

showTrie (TAppl l) = "TAppl "++(showTrieList l)
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
showTrie (TRHS l) = "TRHS "++(showTrieList l)
showTrie (TNodeAdr node) = "TNodeArg "++(show node)
showTrie (TNone l) = "TNone "++(showTrieList l)
