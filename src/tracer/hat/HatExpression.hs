module HatExpression (
 HatExpression(HatApplication,HatConstant,HatConstructor,HatIdentifier,HatSAT_A,
	      HatSAT_B,HatHidden,HatProj,HatCase,HatLambda,HatInt,HatChar,HatInteger,
	      HatRational,HatFloat,HatDouble,HatString,HatIf,HatGuard,HatContainer,
	      HatContainer,HatNone),
 hatExpressionType,
 lazyExpression,
 showExpression,
 showReduction,
 showReductionList
)

where

import HatTrace
import Maybe
import Char(isAlphaNum)


data HatExpression = HatApplication {ref::HatNode,parent::HatExpression,
				     fun::HatExpression,args::[HatExpression],
				     res::HatExpression} |
		     HatConstant {ref::HatNode,parent::HatExpression,
				  fun::HatExpression,res::HatExpression} |
     		     HatConstructor {ref::HatNode,name::String,infixType::HatInfixType} |
		     HatIdentifier {ref::HatNode,name::String,infixType::HatInfixType} |
		     HatSAT_A {ref::HatNode,trace::HatExpression} |
                     HatSAT_B {ref::HatNode,trace::HatExpression} |
                     HatSAT_C {ref::HatNode,trace::HatExpression} |
	             HatHidden {ref::HatNode,parent::HatExpression} |
	             HatProj {ref::HatNode,parent::HatExpression,
			      formerParent::HatExpression} |
                     HatCase {ref::HatNode} |
	             HatLambda {ref::HatNode} |
	             HatInt {ref::HatNode,valueInt::Int} |
                     HatChar {ref::HatNode,valueChar::Char} |
                     HatInteger {ref::HatNode,valueInteger::Integer} |
                     HatRational {ref::HatNode,valueRational::Rational} |
                     HatFloat {ref::HatNode,valueFloat::Float} |
	             HatDouble {ref::HatNode,valueDouble::Double} |
                     HatString {ref::HatNode,valueString::String} |
	             HatIf {ref::HatNode} |
                     HatGuard {ref::HatNode} |
                     HatContainer {ref::HatNode} |
                     HatNone {ref::HatNode}


hatExpressionType :: HatExpression -> HatNodeType
hatExpressionType (HatApplication _ _ _ _ _) = HatApplNode
hatExpressionType (HatConstructor _ _ _) = HatConstrNode
hatExpressionType (HatIdentifier _ _ _) = HatIdentNode
hatExpressionType (HatConstant _ _ _ _) = HatConstantNode
hatExpressionType (HatSAT_A _ _) = HatSAT_ANode
hatExpressionType (HatSAT_B _ _) = HatSAT_BNode
hatExpressionType (HatSAT_C _ _) = HatSAT_CNode
hatExpressionType (HatHidden _ _) = HatHiddenNode
hatExpressionType (HatProj _ _ _) = HatProjNode
hatExpressionType (HatCase _) = HatCaseNode
hatExpressionType (HatLambda _) = HatLambdaNode
hatExpressionType (HatInt _ _) = HatIntNode
hatExpressionType (HatChar _ _) = HatCharNode
hatExpressionType (HatInteger _ _) = HatIntegerNode
hatExpressionType (HatRational _ _) = HatRationalNode
hatExpressionType (HatFloat _ _) = HatFloatNode
hatExpressionType (HatDouble _ _) = HatDoubleNode
hatExpressionType (HatString _ _) = HatCStringNode
hatExpressionType (HatIf _) = HatIfNode
hatExpressionType (HatGuard _) = HatGuardNode
hatExpressionType (HatContainer _) = HatContainerNode
hatExpressionType (HatNone _) = HatUnknownNode

lazyExpression :: Int -> HatNode -> HatExpression


lazyExpression precision node = lazyExpression' precision (hatNodeType node)
 where lazyExpression' :: Int -> HatNodeType -> HatExpression
       lazyExpression' 0 _ = (HatNone node)
       lazyExpression' precision HatApplNode = 
	   let parent = (hatParent node);
	       newprec = (precision-1) in 
	       (HatApplication node 
		(if (isNothing(parent)) then (HatNone node) else
		 (lazyExpression newprec (fromJust parent)))
                (lazyExpression newprec (fromJust (hatApplFun node)))
                (map (lazyExpression newprec) (hatApplArgs node))
		(let r = (hatResult node) in 
		 if (isNothing r) then (HatNone node) else
		 lazyExpression newprec (fromJust r)))
       lazyExpression' _ HatConstrNode = HatConstructor node (fromJust (hatName node)) (hatApplInfix node)
       lazyExpression' _ HatIdentNode = HatIdentifier node (fromJust (hatName node)) (hatApplInfix node)
       lazyExpression' prec HatConstantNode =  
	   let parent = (hatParent node);
	       newprec = prec-1 in
	       (HatConstant node
		(if (isNothing(parent)) then (HatNone node) else
		 (lazyExpression newprec (fromJust parent)))
		(lazyExpression newprec (fromJust (hatApplFun node)))
		(let r = (hatResult node) in 
		 if (isNothing r) then (HatNone node) else
		 lazyExpression newprec (fromJust r)))
       lazyExpression' prec HatSAT_ANode = HatSAT_A node (lazyExpression (prec-1)
						(fromJust (hatParent node)))
       lazyExpression' prec HatSAT_BNode = HatSAT_B node (lazyExpression (prec-1)
						(fromJust (hatParent node)))
       lazyExpression' prec HatSAT_CNode = (lazyExpression (prec-1)
						(fromJust (hatParent node)))
       lazyExpression' prec HatHiddenNode = HatHidden node (lazyExpression (prec-1)
						(fromJust (hatParent node)))
       lazyExpression' prec HatProjNode =  HatProj node (lazyExpression (prec-1)
					       (fromJust (hatParent node)))
				              (lazyExpression (prec-1) 
					       (fromJust (hatProjRef node)))
       lazyExpression' _ HatCaseNode = HatCase node
       lazyExpression' _ HatLambdaNode = HatLambda node
       lazyExpression' _ HatIntNode = HatInt node (hatInt (hatValue node))
       lazyExpression' _ HatCharNode = HatChar node (hatChar (hatValue node))
       lazyExpression' _ HatIntegerNode = HatInteger node (hatInteger (hatValue node))
       lazyExpression' _ HatRationalNode = HatRational node (hatRational (hatValue node))
       lazyExpression' _ HatFloatNode = HatFloat  node (hatFloat (hatValue node))
       lazyExpression' _ HatDoubleNode = HatDouble node (hatDouble (hatValue node))
       lazyExpression' _ HatCStringNode = HatString node (hatCString (hatValue node))
       lazyExpression' _ HatIfNode = HatIf node
       lazyExpression' _ HatGuardNode = HatGuard node
       lazyExpression' _ HatContainerNode = HatContainer node
       lazyExpression' _ HatUnknownNode = HatNone node

hatInt (HatIntValue i) = i
hatChar (HatCharValue c) = c
hatInteger (HatIntegerValue i) = i
hatRational (HatRationalValue r) = r
hatFloat (HatFloatValue f) = f
hatDouble (HatDoubleValue d) = d
hatCString (HatStringValue s) = s


spacer stra "" = stra
spacer "" strb = strb
spacer stra strb@(':':_) = stra ++ strb
spacer stra strb = if (last stra)==':' then stra++strb else stra ++ " " ++ strb


ppStringExpr :: Int -> HatExpression -> Maybe String
ppStringExpr 0 _ = Nothing
ppStringExpr precision expr =
 let appfun = (fun expr) in
   if (hatExpressionType(appfun)==HatUnknownNode) then Nothing
--     if (hatNodeType expr)==HatConstructor then
--	if (hatName expr)==(Just "[]") then (Just "") else Nothing
--     else Nothing
   else
    let funsym = (prettyPrint 2 appfun) in
    if funsym==":" then
      let arguments = (args expr) in
       if (length arguments)/=2 then Nothing else
         let f = (head arguments) in
          if (hatExpressionType f)==HatConstantNode then
           pp' (fun f) (head (tail arguments))
          else Nothing
    else 
      if funsym=="[]" then (Just "") else Nothing
 where pp' :: HatExpression -> HatExpression -> Maybe String
       pp' (HatChar ref c) s = 
         let r = (ppStringExpr (precision-1) s) in
           if (r==Nothing) then Nothing else Just (ppchar c (fromJust r))
       pp' _ _ = Nothing
       ppchar c r = let chr = (show c) in
                   if (length chr)==3 then (head (tail chr)):r
                   else (head(tail chr)):(head (tail (tail chr))):r

prettyPrint :: Int -> HatExpression -> String
prettyPrint i expr = fst (pPrint i expr (HatInfix 0))

pPrint :: Int -> HatExpression -> HatInfixType -> (String,HatInfixType)
pPrint precision expr topInfix
 | precision == 0 = ("<CUT>",HatNoInfix)
 | otherwise = prettyPrint' (hatExpressionType expr)
   where
    prettyPrint' HatApplNode = 
     let resultType = (hatExpressionType (res expr)) in
      if (resultType==HatSAT_BNode) then ("_|_",HatNoInfix) else
       if (resultType==HatSAT_ANode) then ("_",HatNoInfix) else
         let s = (ppStringExpr precision expr) in 
          if (isJust s) then
             if (fromJust s)=="" then ("[]",HatNoInfix) else ("\""++(fromJust s)++"\"",HatNoInfix)
          else
           if (precision>1) then
            let (funsym,infixP) = (pPrint (precision-1)
				   (fun expr) topInfix) in
             if funsym == "<CUT>" then (funsym,infixP) else
               (brackets infixP topInfix (foldl (spacer) "" (swapListInfix infixP
				   funsym (map (\x->fst (pPrint (precision-1) x infixP))
					 (args expr)))),
	        infixP)
           else ("<CUT>",HatNoInfix)
    prettyPrint' HatConstantNode = pPrint precision (fun expr) topInfix
    prettyPrint' HatHiddenNode = ("<Hidden>",HatNoInfix)
    prettyPrint' HatSAT_ANode = ("_",HatNoInfix)
    prettyPrint' HatSAT_BNode = ("_|_",HatNoInfix)
    prettyPrint' HatConstrNode = ((name expr),(infixType expr))
    prettyPrint' HatIdentNode = ((name expr),(infixType expr))
    prettyPrint' HatIntNode = (show (valueInt expr),HatNoInfix)
    prettyPrint' HatCharNode = (show (valueChar expr),HatNoInfix)
    prettyPrint' HatIntegerNode = (show (valueInteger expr),HatNoInfix)
    prettyPrint' HatRationalNode = (show (valueRational expr),HatNoInfix)
    prettyPrint' HatFloatNode = (show (valueFloat expr),HatNoInfix)
    prettyPrint' HatDoubleNode = (show (valueDouble expr),HatNoInfix)
    prettyPrint' HatCStringNode = (ppString (valueString expr),HatNoInfix)
    prettyPrint' HatIfNode = ("IF",HatNoInfix)
    prettyPrint' HatGuardNode = ("GUARD",HatNoInfix)
    prettyPrint' HatContainerNode = ("CONTAINER",HatNoInfix)
    prettyPrint' HatProjNode = pPrint (precision-1) (formerParent expr) topInfix
    prettyPrint' x = ("{ERROR in prettyPrint: "++(show x)++"}",HatNoInfix)
    ppString s = ('\'':s)++"'"
    swapListInfix _ f [] = f:[]
    swapListInfix HatNoInfix f x = (f:x)
    swapListInfix i f (x:r) = (x:(infixquote f):r)
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

--showHatExprList :: [HatExpression] -> [(Int,Int)]
--showHatExprList [] = []
--showHatExprList (((f,node),_):xs) = (((showForeignObjAddr f),node):(showHatExprList xs))

showExpression :: HatExpression -> IO ()
showExpression hatExpression =
    putStr (unbracket (prettyPrint 100 hatExpression))
    where unbracket ('(':r) = cutlast r
          unbracket r = r
          cutlast (')':[]) = []
          cutlast [] = []
          cutlast (c:r) = c:(cutlast r)

showReduction :: HatExpression -> IO ()
showReduction hatNode =
  showExpression hatNode >>
  let exptype = (hatExpressionType hatNode) in
    if ((exptype==HatApplNode)||(exptype==HatConstantNode)) then
     let result = (res hatNode) in
        putStr " = " >>
        (showExpression result) >>
        putStrLn ""
    else
     return ()

showReductionList :: [HatExpression] -> IO ()
showReductionList [] = return ()
showReductionList (x:list) =
    do
    showReduction x
    showReductionList list



