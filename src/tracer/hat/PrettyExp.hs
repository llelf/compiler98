-- Pretty printing of an ART expression.
-- Function for converting an instance of class HatRep into a Doc
-- for pretty printing.
module PrettyExp(hatRep2Doc,showExpression,showReduction) where

import HatTrace(HatRep(..),HatNodeType(..),HatNode,isValidNode
               ,HatInfixType(HatInfix,HatInfixL,HatInfixR,HatNoInfix))
import HatExpressionTree(HatLimit,toHatLimit)
import PrettyLib(Doc,text,(<>),delimiter,fdelimiter,nil,group,parens
                ,groupNest,pretty)
import Char(isAlpha)
import List (unzip3)


data SExp = 
  SApp [SExp] | -- n-ary application of at least 2 expressions
  SId String HatInfixType |
  SLiteral String |
  SLambda | 
  SIf SExp | SCase SExp | SGuard SExp |
  SInvalid | -- subexpression that was cut off (to limit depth)
  SUnevaluated |
  SBottom |
  SCycle String SExp  -- cyclic expression to be shown as `id where id = ..'

-- conversion function
-- if first boolean is True, then cycles are located and expressed as SCycles
-- if False, there is the danger of obtaining an infinite expression
-- if second boolean is True, then unevaluated arguments appear in result,
-- otherwise they are represented by SUnevaluated.
hatRep2SExp :: HatRep a => Bool -> Bool -> a -> SExp
hatRep2SExp cyc uneval expObj = case go cyc uneval [] expObj of (e,_,_) -> e  

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

-- worker for definition above
go :: HatRep a => Bool -> Bool -> [(HatNode,String)] -> a 
   -- look for cycles?  transform unevaluted args?
   --  nodes above with variable name for `where'
   -> (SExp,[HatNode],[String]) 
   -- (expression,nodes that start cycle
   -- ,occurring variable names (except those for cycles))
go cyc uneval nodesAbove expObj = case hatNodeType expObj of
  HatApplNode -> 
    let expNode = toHatNode expObj
        partCycles :: [HatNode]
        partCycles = funCycles ++ concat argsCycles
        partVars :: [String]
        partVars = funVars ++ concat argsVars
        isCycle = cyc && expNode `elem` partCycles
        var :: String
        var = head . filter (not . (`elem` partVars)) .
              map (("cyc"++) . show) $ [1..] 
        newNodesAbove :: [(HatNode,String)]
        newNodesAbove = (expNode,var) : nodesAbove
        -- (fun,funCycles,funVars) = ... not accepted by nhc98
        fun = fst3 z
        funCycles = snd3 z
        funVars = thd3 z
        z = go cyc uneval newNodesAbove (hatApplFun expObj) 
        args = fst3 zs
        argsCycles = snd3 zs
        argsVars = thd3 zs
        -- (args,argsCycles,argsVars) = ... not accepted by nhc98
        zs = unzip3 $ map (go cyc uneval newNodesAbove) (hatApplArgs expObj)
        sexp = case fun of
          SIf SLambda -> 
            if length args == 2 
              then SIf (head args)
              else error "hatRep2Doc: invalid If node"
          SCase SLambda -> 
            if length args == 2 
              then SCase (head args)
              else error "hatRep2Doc: invalid Case node"
          SGuard SLambda ->
            if length args == 2 
              then SGuard (head args)
              else error "hatRep2Doc: invalid Guard node"
          SApp args1 -> SApp (args1++args)  -- combine applications
          _ -> SApp (fun:args)
    in case lookup expNode nodesAbove of
         Just var -> (SId var HatNoInfix,[expNode],[]) -- `lower' end of cycle
         Nothing ->
           case sexp of
             SApp [SId "IO" fixity,_] -> 
               -- get hidden argument of IO, otherwise only get IO <hidden>
               let arg = head (hatApplArgs expObj)
                   par = hatParent arg
                   newArg = if hatNodeType arg == HatHiddenNode && 
                              isValidNode par
                              then par else arg
                   z = go cyc uneval newNodesAbove newArg
               in (SApp [SId "IO" fixity,fst3 z],snd3 z,thd3 z)
             _ -> -- normal case 
               (if isCycle then SCycle var sexp else sexp,partCycles,partVars)
  HatConstantNode -> go cyc uneval nodesAbove (hatApplFun expObj)
  HatSAT_ANode -> if uneval 
                    then go cyc uneval nodesAbove (hatProjValue expObj)
                    else simple $ SUnevaluated
  HatSAT_BNode -> simple $ SBottom
  HatSAT_CNode -> error "hatRep2Doc: hatSAT_CNode" -- should never appear
  HatHiddenNode -> simple $ SLiteral "<hidden>"
  HatDummyNode -> simple $ SLiteral "(<dummy>)"
  HatProjNode -> go cyc uneval nodesAbove (hatProjValue expObj)
  HatConstrNode -> let var = hatName expObj 
                   in (SId var (hatInfix expObj),[],[])
  HatIdentNode -> let var = hatName expObj 
                  in (SId var (hatInfix expObj),[],[var])
  HatCaseNode -> simple $ SCase SLambda  -- hack for recognition as app fun
  HatIfNode -> simple $ SIf SLambda -- ""
  HatGuardNode -> simple $ SGuard SLambda -- ""
  HatLambdaNode -> simple $ SLambda
  HatIntNode -> simpleLitShow . hatValueInt $ expObj
  HatCharNode -> simpleLitShow . hatValueChar $ expObj
  HatIntegerNode -> simpleLitShow . hatValueInteger $ expObj
  HatRationalNode -> simpleLitShow . hatValueRational $ expObj
  HatFloatNode -> simpleLitShow . hatValueFloat $ expObj
  HatDoubleNode -> simpleLitShow . hatValueDouble $ expObj
  HatCStringNode -> simpleLitShow . hatValueString $ expObj
  HatContainerNode -> simple $ SLiteral "(?)"
  HatInvalidNode -> simple $ SInvalid
  HatModuleNode -> error "hatRep2Doc: HatModuleNode"
  where
  simple e = (e,[],[])
  simpleLitShow = simple . SLiteral . show

indentation :: Int
indentation = 2

atomic :: SExp -> Bool
atomic (SApp ((SId (',':xs) _):args)) = length xs + 2 == length args
atomic (SApp _) = False
atomic (SIf _) = False
atomic (SCase _) = False
atomic (SGuard _) = False
atomic (SCycle _ _) = False
atomic _ = True

-- useful document combinator
(<->) :: Doc -> Doc -> Doc
d1 <-> d2 = d1 <> delimiter " " <> d2 

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> fdelimiter " " <> d2

isOpSym :: Char -> Bool
isOpSym c = not (isAlpha c || c == '[') 

funDoc :: String -> Doc
funDoc var = (if isOpSym $ head var then parens else id) $ text var 

opDoc :: String -> Doc
opDoc var =  text ((if isAlpha $ head var then ('`' :) . (++ "'") else id) var)

-- put parenthesis around non-atomic expression
argDoc :: SExp -> Doc
argDoc exp = (if atomic exp then id else parens) $ sExp2Doc exp

-- handle argument of infix operator of given priority and test for equal
-- priority
infixArgDoc :: Int -> (HatInfixType -> Bool) -> SExp -> Doc
infixArgDoc pri assocTest exp@(SApp [SId var fixity,e1,e2]) = 
  (if comp (priority fixity) pri (assocTest fixity) then parens else id) $ 
    sExp2Doc exp  
  where
  comp p1 p2 test = case compare p1 p2 of
                      LT -> True
                      EQ -> test
                      GT -> False 
infixArgDoc _ _ exp = sExp2Doc exp

priority (HatInfix p) = p
priority (HatInfixL p) = p
priority (HatInfixR p) = p
priority HatNoInfix = 10

notInfixL (HatInfixL _) = False
notInfixL _ = True

notInfixR (HatInfixR _) = False
notInfixR _ = True

-- a central function
sExp2Doc :: SExp -> Doc 
sExp2Doc (SApp ((SId (',':xs) _):args)) | length xs + 2 == length args =
  -- print tuple properly
  group 
    (text "(" <> foldr1 (<*>) (map sExp2Doc args)) <> text ")"
  where
  (<*>) :: Doc -> Doc -> Doc
  d1 <*> d2 = d1 <> fdelimiter "" <> text "," <> d2
sExp2Doc (SApp [SId var (HatInfix pri),e1,e2]) =
  groupNest indentation 
    (infixArgDoc pri (const True) e1 <+> opDoc var <+> 
      infixArgDoc pri (const True) e2)
sExp2Doc (SApp [SId var (HatInfixL pri),e1,e2]) =
  groupNest indentation
    (infixArgDoc pri notInfixL e1 <+> opDoc var <+> 
      infixArgDoc pri (const True) e2)
sExp2Doc (SApp [SId var (HatInfixR pri),e1,e2]) =
  groupNest indentation
    (infixArgDoc pri (const True) e1 <+> opDoc var <+> 
      infixArgDoc pri notInfixR e2)
sExp2Doc (SApp [SId var fixity,e]) | fixity /= HatNoInfix =
  -- show infix operator with single argument as section
  groupNest indentation (parens (argDoc e <-> opDoc var))
sExp2Doc (SApp exps) = groupNest indentation . foldr1 (<+>) . map argDoc $ exps
sExp2Doc (SId var fixity) = funDoc var
sExp2Doc (SLiteral lit) = text lit
sExp2Doc SLambda = text "(\\..)"
sExp2Doc (SIf exp) = groupNest indentation (text "if" <-> argDoc exp)
sExp2Doc (SCase exp) = groupNest indentation (text "case" <-> argDoc exp)
sExp2Doc (SGuard exp) = text "| " <> argDoc exp
sExp2Doc SInvalid = text "<cut>"
sExp2Doc SUnevaluated = text "_"
sExp2Doc SBottom = text "_|_"
sExp2Doc (SCycle var exp) = 
  groupNest indentation (text var <+> group (text "where" <+> sExp2Doc exp))

-- main function
hatRep2Doc :: HatRep a => Bool -> Bool -> Int -> a -> Doc
hatRep2Doc explicitCycle uneval depth = 
  sExp2Doc . hatRep2SExp explicitCycle uneval -- . toHatLimit depth

showExpression :: HatNode -> String -> String
showExpression node initial =
  pretty 80 
    (text initial <> groupNest (length initial)
      (hatRep2Doc True False 10 node))

showReduction :: Bool -> Int -> HatNode -> String -> String -> String
showReduction verboseMode precision node initial final =
  pretty 80 
    (text initial <> groupNest (length initial) 
      (hatRep2Doc True verboseMode precision node <> 
        (if isValidNode result
          then nil <-> text "=" <-> 
                 hatRep2Doc True verboseMode precision result
          else nil) <> 
          (if null final then nil else delimiter "  " <> text final)))
  where
  result = hatResult node


-- only for testing:
test1 = SApp [SId "fun" HatNoInfix,SLiteral "24",SLiteral "True",SApp[SId "+" (HatInfixL 5),SLiteral "3",SLiteral "4"]]

test2 = SApp [SId "*" (HatInfixL 6),SApp [SId "+" (HatInfixL 5),SApp [SId "-" (HatInfixL 5),SLiteral "3",SLiteral "6"],SApp [SId "-" (HatInfixL 5),SLiteral "3",SLiteral "6"]],test1]
 