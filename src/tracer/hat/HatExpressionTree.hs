module HatExpressionTree (
-- entire expression held in memory. Multiple access to any part of the expression
-- will only result in one access to the file.

 HatExpressionTree(
		   HatApplication,HatConstant,HatConstructor,HatIdentifier,HatSAT_A,
		   HatSAT_B,HatHidden,HatProj,HatCase,HatLambda,HatInt,HatChar,
		   HatInteger,HatDummy,
		   HatRational,HatFloat,HatDouble,HatString,HatIf,HatGuard,HatContainer,
		   HatContainer,HatNone),
 toHatExpressionTree,
 toHatLimit,
 fromHatLimit,
 prettyPrint,
 printExpression,
 printReduction,
 printReductionList
)
where

import HatTrace
import Maybe
import Char(isAlphaNum)


#ifdef __NHC__
default (Int,Double,HatExpressionTree)
#endif

data HatExpressionTree = 
                     HatApplication {ref::HatNode,
				     parent::HatExpressionTree,
				     fun::HatExpressionTree,
				     args::[HatExpressionTree],
				     res::HatExpressionTree}       |
		     HatConstant    {ref::HatNode,
				     parent::HatExpressionTree,
				     fun::HatExpressionTree,
				     res::HatExpressionTree}       |
     		     HatConstructor {ref::HatNode,
				     name::String,
				     infixType::HatInfixType}      |
		     HatIdentifier  {ref::HatNode,
				     name::String,
				     infixType::HatInfixType}      |
		     HatSAT_A     {ref::HatNode,
				   projValue::HatExpressionTree}      |
                     HatSAT_B     {ref::HatNode,
			           projValue::HatExpressionTree}      |
                     HatSAT_C     {ref::HatNode,
				   projValue::HatExpressionTree}      |
	             HatHidden    {ref::HatNode,
				   parent::HatExpressionTree}      |
	             HatProj      {ref::HatNode,
				   parent::HatExpressionTree,
				   projValue::HatExpressionTree}   |
                     HatCase      {ref::HatNode}            |
	             HatLambda    {ref::HatNode}            |
	             HatInt       {ref::HatNode,
				   valueInt::Int}           |
                     HatChar      {ref::HatNode,
				   valueChar::Char}         |
                     HatInteger   {ref::HatNode,
				   valueInteger::Integer}   |
                     HatRational  {ref::HatNode,
				   valueRational::Rational} |
                     HatFloat     {ref::HatNode,
				   valueFloat::Float}       |
	             HatDouble    {ref::HatNode,
				   valueDouble::Double}     |
                     HatString    {ref::HatNode,
				   valueString::String}     |
	             HatIf        {ref::HatNode}            |
                     HatGuard     {ref::HatNode}            |
                     HatContainer {ref::HatNode}            |
		     HatDummy     {ref::HatNode}            |
                     HatNone      {ref::HatNode}
		     deriving Show

instance HatRep HatExpressionTree where
  toHatNode (HatNone _) = hatInvalidNode
  toHatNode a      = ref a
  toUndefined a    = HatNone (toHatNode a)
  hatParent        = parent
  hatApplFun       = fun
  hatApplArgs      = args
  hatInfix         = infixType
  hatResult        = res
  hatName          = name
  hatProjValue     = projValue
  hatValueInt      = valueInt
  hatValueChar     = valueChar
  hatValueInteger  = valueInteger
  hatValueRational = valueRational
  hatValueFloat    = valueFloat
  hatValueDouble   = valueDouble
  hatValueString   = valueString
  hatNodeType      = toHatNodeType

toHatNodeType :: HatExpressionTree -> HatNodeType
toHatNodeType (HatApplication _ _ _ _ _) = HatApplNode
toHatNodeType (HatConstructor _ _ _)     = HatConstrNode
toHatNodeType (HatIdentifier _ _ _)      = HatIdentNode
toHatNodeType (HatConstant _ _ _ _)      = HatConstantNode
toHatNodeType (HatSAT_A _ _)             = HatSAT_ANode
toHatNodeType (HatSAT_B _ _)             = HatSAT_BNode
toHatNodeType (HatSAT_C _ _)             = HatSAT_CNode
toHatNodeType (HatHidden _ _)            = HatHiddenNode
toHatNodeType (HatProj _ _ _)            = HatProjNode
toHatNodeType (HatCase _)                = HatCaseNode
toHatNodeType (HatLambda _)              = HatLambdaNode
toHatNodeType (HatInt _ _)               = HatIntNode
toHatNodeType (HatChar _ _)              = HatCharNode
toHatNodeType (HatInteger _ _)           = HatIntegerNode
toHatNodeType (HatRational _ _)          = HatRationalNode
toHatNodeType (HatFloat _ _)             = HatFloatNode
toHatNodeType (HatDouble _ _)            = HatDoubleNode
toHatNodeType (HatString _ _)            = HatCStringNode
toHatNodeType (HatIf _)                  = HatIfNode
toHatNodeType (HatGuard _)               = HatGuardNode
toHatNodeType (HatContainer _)           = HatContainerNode
toHatNodeType (HatDummy _)               = HatDummyNode
toHatNodeType (HatNone _)                = HatInvalidNode

toHatExpressionTree :: HatRep a => Int -> a -> HatExpressionTree
toHatExpressionTree precision node = _toHatExpressionTree precision (toHatNode node)

_toHatExpressionTree :: Int -> HatNode -> HatExpressionTree
_toHatExpressionTree precision node = toHatExpression' precision (hatNodeType node)
 where toHatExpression' :: Int -> HatNodeType -> HatExpressionTree
       toHatExpression' 0 _ = (HatNone node)
       toHatExpression' precision HatApplNode = 
	   let parent = (hatParent node);
	       newprec = (precision-1) in 
	       (HatApplication node 
		(if (isInvalidNode parent) then (HatNone node) else
		 (_toHatExpressionTree newprec parent))
                (_toHatExpressionTree newprec (hatApplFun node))
                (map (_toHatExpressionTree newprec) (hatApplArgs node))
		(let r = (hatResult node) in 
		 if (isInvalidNode r) then (HatNone node) else
		 _toHatExpressionTree newprec r))
       toHatExpression' _ HatConstrNode = (HatConstructor node 
					  (hatName node)
					  (hatInfix node))
       toHatExpression' _ HatIdentNode = (HatIdentifier node
					 (hatName node)
					 (hatInfix node))
       toHatExpression' prec HatConstantNode =  
	   let parent = (hatParent node);
	       newprec = prec-1 in
	       (HatConstant node
		(if (isInvalidNode parent) then (HatNone node) else
		 (_toHatExpressionTree newprec parent))
		(_toHatExpressionTree newprec (hatApplFun node))
		(let r = (hatResult node) in 
		 if (isInvalidNode r) then (HatNone node) else
		 _toHatExpressionTree newprec r))
       toHatExpression' prec HatSAT_ANode = HatSAT_A node (_toHatExpressionTree (prec-1)
						(hatProjValue node))
       toHatExpression' prec HatSAT_BNode = HatSAT_B node (_toHatExpressionTree (prec-1)
						(hatProjValue node))
       toHatExpression' prec HatSAT_CNode = (_toHatExpressionTree (prec-1)
						(hatProjValue node))
       toHatExpression' prec HatHiddenNode = HatHidden node (_toHatExpressionTree (prec-1)
						(hatParent node))
       toHatExpression' prec HatProjNode =  HatProj node (_toHatExpressionTree (prec-1)
							 (hatParent node))
					   (_toHatExpressionTree (prec-1) 
					       (hatProjValue node))
       toHatExpression' _ HatCaseNode = HatCase node
       toHatExpression' _ HatLambdaNode = HatLambda node
       toHatExpression' _ HatIntNode = HatInt node (hatValueInt node)
       toHatExpression' _ HatCharNode = HatChar node (hatValueChar node)
       toHatExpression' _ HatIntegerNode = HatInteger node (hatValueInteger node)
       toHatExpression' _ HatRationalNode = HatRational node (hatValueRational node)
       toHatExpression' _ HatFloatNode = HatFloat node (hatValueFloat node)
       toHatExpression' _ HatDoubleNode = HatDouble node (hatValueDouble node)
       toHatExpression' _ HatCStringNode = HatString node (hatValueString node)
       toHatExpression' _ HatIfNode = HatIf node
       toHatExpression' _ HatGuardNode = HatGuard node
       toHatExpression' _ HatContainerNode = HatContainer node
       toHatExpression' _ HatDummyNode = HatDummy node
       toHatExpression' _ HatInvalidNode = HatNone node


spacer stra "" = stra
spacer "" strb = strb
spacer stra strb@(':':_) = stra ++ strb
spacer stra strb = if (last stra)==':' then stra++strb else stra ++ " " ++ strb


ppStringExpr :: HatRep a => Int -> a -> Maybe String
ppStringExpr 0 _ = Nothing
ppStringExpr precision expr =
 let exptype = (hatNodeType expr);
     appfun = (hatApplFun expr) in
 if (exptype==HatProjNode) then (ppStringExpr precision (hatProjValue expr)) else
 if (exptype/=HatConstantNode)&&(exptype/=HatApplNode) then Nothing else
   if ((hatNodeType appfun)==HatInvalidNode) then Nothing else
    let funsym = (prettyPrint 2 False appfun) in
    if funsym==":" then
      let arguments = (hatApplArgs expr) in
       if (length arguments)/=2 then Nothing else
         let f = (head arguments) in
          if (hatNodeType f)==HatConstantNode then
            pp' (hatApplFun f) (head (tail arguments))
           else Nothing
     else 
      if funsym=="[]" then (Just "") else Nothing
 where 
       pp' :: HatRep b => b -> b -> Maybe String
       pp' exp s = if (hatNodeType exp)==HatCharNode then
		    let c = hatValueChar exp;
			r = (ppStringExpr (precision-1) s) in
		     if (r==Nothing) then Nothing
		      else Just (ppchar c (fromJust r))
                    else
		     Nothing
       ppchar c r = let chr = (show c) in
                     if (length chr)==3 then (head (tail chr)):r
                      else (head (tail chr)):(head (tail (tail chr))):r


fst3 :: (a,b,c)  -> a
fst3 (a,_,_)     = a
thrd3 :: (a,b,c) -> c
thrd3 (_,_,c)    = c

-- prettyPrinting by Haskell

prettyPrint :: HatRep a => Int -> Bool -> a -> String
prettyPrint i verbose expr = fst3 (pPrint [] i verbose HatNoInfix expr)

pPrint :: HatRep a => [HatNode] -> Int -> Bool -> HatInfixType -> a ->
          (String,HatInfixType,[HatNode])
pPrint previousnodes precision verbose topInfix expr
 | precision == 0 = ("<CUT>",HatNoInfix,[])
 | otherwise =
     let hn = (toHatNode expr) in
	 if (hn `elem` previousnodes) then ("a",HatNoInfix,[hn]) else
	    let r@(s,i,cs)=prettyPrint' (hn:previousnodes) (hatNodeType expr) in
              if (hn `elem` cs) then 
		 ("(a where a = "++s++")",HatNoInfix,cs)
	       else r
   where
    prettyPrint' pnodes HatApplNode =
         let s = (ppStringExpr precision expr) in
          if (isJust s) then
             if (fromJust s)=="" then ("[]",HatNoInfix,[]) else
		    ("\""++(fromJust s)++"\"",HatNoInfix,[])
          else
           if (precision>0) then
            let (funsym,infixP,cycles) = (pPrint
					  pnodes
					  (precision-1)
					  verbose
					  topInfix
					  (hatApplFun expr)) in
             if funsym == "<CUT>" then
		    (funsym,infixP,[])
              else
               let args = (hatApplArgs expr) in
                if (funsym == "IO")&&((length args)==1) then
	          -- show HIDDEN argument of IO (otherwise only IO <HIDDEN> was shown)
		 let f = (head args);
                     p = (hatParent f);
		     arg = if ((hatNodeType f)==HatHiddenNode)&&(isValidNode p) then p
			   else f;
		     (s,_,cycles2) = pPrint pnodes (precision-1) verbose infixP arg
		     in
		      (brackets infixP topInfix (spacer funsym s),infixP,cycles2)
                else
		 let pps = map (pPrint pnodes (precision-1) verbose infixP) args in
		 (brackets infixP topInfix (foldl (spacer) "" (swapListInfix infixP
				 funsym (map fst3 pps))),
	          infixP,
		  (cycles++(foldl (++) [] (map thrd3 pps))))
            else ("<CUT>",HatNoInfix,[])
    prettyPrint' pnodes HatConstantNode = (pPrint pnodes precision verbose topInfix
					   (hatApplFun expr))
    prettyPrint' _ HatHiddenNode    = ("<Hidden>",HatNoInfix,[])
    prettyPrint' pnodes HatSAT_ANode= if (verbose) then
				        (pPrint pnodes precision verbose topInfix
					 (hatProjValue expr))  
				       else ("_",HatNoInfix,[])
    prettyPrint' _ HatSAT_BNode     = ("_|_",HatNoInfix,[])
    prettyPrint' _ HatConstrNode    = ((hatName expr),(hatInfix expr),[])
    prettyPrint' _ HatIdentNode     = ((hatName expr),(hatInfix expr),[])
    prettyPrint' _ HatIntNode       = (show (hatValueInt expr),     HatNoInfix,[])
    prettyPrint' _ HatCharNode      = (show (hatValueChar expr),    HatNoInfix,[])
    prettyPrint' _ HatIntegerNode   = (show (hatValueInteger expr), HatNoInfix,[])
    prettyPrint' _ HatRationalNode  = (show (hatValueRational expr),HatNoInfix,[])
    prettyPrint' _ HatFloatNode     = (show (hatValueFloat expr),   HatNoInfix,[])
    prettyPrint' _ HatDoubleNode    = (show (hatValueDouble expr),  HatNoInfix,[])
    prettyPrint' _ HatCStringNode   = (ppString (hatValueString expr),HatNoInfix,[])
    prettyPrint' _ HatIfNode        = ("IF",HatNoInfix,[])
    prettyPrint' _ HatGuardNode     = ("GUARD",HatNoInfix,[])
    prettyPrint' _ HatContainerNode = ("CONTAINER",HatNoInfix,[])
    prettyPrint' _ HatLambdaNode    = ("LAMBDA",HatNoInfix,[])
    prettyPrint' _ HatDummyNode     = ("DUMMY",HatNoInfix,[])
    prettyPrint' _ HatCaseNode      = ("CASE",HatNoInfix,[])
    prettyPrint' pnodes HatProjNode = (pPrint pnodes (precision-1) verbose topInfix
				       (hatProjValue expr))
    prettyPrint' _ HatInvalidNode   = ("<CUT>",HatNoInfix,[])
    prettyPrint' _ x = ("{ERROR in prettyPrint: "++(show x)++"}",HatNoInfix,[])
    ppString s = ('\'':s)++"'"
    swapListInfix HatNoInfix f x = (f:x)
    swapListInfix _ f [] = f:[]
    swapListInfix _ f (x:r) = (x:(infixquote f):r)
    infixquote s@(c:_) = if (isAlphaNum c) then '`':(s++"`") else s
    infixquote s = s
    brackets HatNoInfix _ l = '(':(l++")")
    brackets _ HatNoInfix l = '(':(l++")")
    brackets (HatInfixR p) (HatInfixR q) l = if (p>=q) then l else '(':(l++")")
    brackets (HatInfixL p) (HatInfixL q) l = if (p>q) then l else '(':(l++")")
    brackets (HatInfixR p) (HatInfixL q) l = if (p>=q) then l else '(':(l++")")
    brackets (HatInfixL p) (HatInfixR q) l = if (p>=q) then l else '(':(l++")")
    brackets (HatInfixR p) (HatInfix q) l = if (p>=q) then l else '(':(l++")")
    brackets (HatInfixL p) (HatInfix q) l = if (p>=q) then l else '(':(l++")")
    brackets (HatInfix p) (HatInfixL q) l  = if (p>=q) then l else '(':(l++")")
    brackets (HatInfix p) (HatInfixR q) l  = if (p>=q) then l else '(':(l++")")
    brackets (HatInfix p) (HatInfix q) l   = if (p>=q) then l else '(':(l++")")
    brackets  _ _ l = '(':(l++")")


printExpression :: HatRep a => Bool -> a -> IO ()
printExpression verbose expression =
    putStr --(unbracket 
	    (prettyPrint 100 verbose expression) --)
    where unbracket ('(':r) = cutlast r
          unbracket r = r
          cutlast (')':[]) = []
          cutlast [] = []
          cutlast (c:r) = c:(cutlast r)

printReduction :: HatRep a => Bool -> a -> IO ()
printReduction verbose expression =
  printExpression verbose expression >>
  let exptype = (hatNodeType expression) in
    if ((exptype==HatApplNode)||(exptype==HatConstantNode)) then
      putStr " = " >>
      (printExpression verbose (hatResult expression))
    else
     return ()

printReductionList :: HatRep a => Bool -> [a] -> IO ()
printReductionList _ [] = return ()
printReductionList verbose (x:list) =
    do
    printReduction verbose x
    putStrLn ""
    printReductionList verbose list


data HatRep a => HatLimit a = Limit Int a deriving Show

toHatLimit :: HatRep a => Int -> a -> HatLimit a
toHatLimit 0 a = Limit 0 (toUndefined a)
toHatLimit limitation n = Limit limitation n

fromHatLimit :: HatRep a => HatLimit a -> a
fromHatLimit (Limit _ n) = n

instance HatRep a => HatRep (HatLimit a) where
  toHatNode (Limit p n)        = toHatNode n
  toUndefined (Limit p n)      = Limit 0 (toUndefined n)
  hatParent (Limit p n)        = (toHatLimit (p-1) (hatParent n))
  hatApplFun (Limit p n)       = (toHatLimit (p-1) (hatApplFun n))
  hatApplArgs (Limit p n)      = (map (toHatLimit (p-1))) (hatApplArgs n)
  hatInfix (Limit p n)         = hatInfix n
  hatResult (Limit p n)        = toHatLimit (p-1) (hatResult n)
  hatName (Limit p n)          = hatName n
  hatProjValue (Limit p n)     = toHatLimit (p-1) (hatProjValue n)
  hatValueInt (Limit p n)      = hatValueInt n
  hatValueChar (Limit p n)     = hatValueChar n
  hatValueInteger (Limit p n)  = hatValueInteger n
  hatValueRational (Limit p n) = hatValueRational n
  hatValueFloat (Limit p n)    = hatValueFloat n
  hatValueDouble (Limit p n)   = hatValueDouble n
  hatValueString (Limit p n)   = hatValueString n
  hatNodeType (Limit p n)      = hatNodeType n

