-- Pretty printing of an ART expression.
-- Function for converting an instance of class HatRep into a Doc
-- for pretty printing.
module PrettyExp(hatRep2Doc,showExpression,showReduction) where

import HatTrace(HatRep(..),HatNodeType(..),HatNode,isValidNode
               ,HatInfixType(HatInfix,HatInfixL,HatInfixR,HatNoInfix)
               ,HatSourceRef(..))
import HatExpressionTree(HatLimit,toHatLimit)
import PrettyLib(Doc,text,(<>),delimiter,fdelimiter,nil,group,parens
                ,groupNest,pretty)
import Char(isAlpha)
import List (unzip3)


data SExp = 
  SApp [SExp] | -- n-ary application of at least 2 expressions
  SId String SFixity |
  SLiteral String |
  SLambda | 
  SIf SExp | SCase SExp | SGuard SExp |
  SInvalid | -- subexpression that was cut off (to limit depth)
  SUnevaluated |
  SBottom |
  SCycle String SExp  -- cyclic expression to be shown as `id where id = ..'

data SFixity = 
  SInfix Int | SInfixL Int | SInfixR Int | SAssoc Int String | SInfixDefault
  -- need own type for some hardcoded operators that are known to be
  -- semantically associative

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

transFixity :: HatInfixType -> SFixity
transFixity (HatInfix pri) = SInfix pri
transFixity (HatInfixL pri) = SInfixL pri
transFixity (HatInfixR pri) = SInfixR pri
transFixity HatNoInfix = SInfixDefault

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
         Just var -> (SId var SInfixDefault,[expNode],[]) 
                     -- `lower' end of cycle
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
                   in (SId var (transFixity (hatInfix expObj)),[],[])
  HatIdentNode -> let var = hatName expObj 
                  in (SId var
                       (case var of
                         "." | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 9 var
                         "++" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 5 var
                         "&&" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 3 var
                         "||" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 2 var
                         "*" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 7 var
                         "+" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 6 var
                         ">>" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 1 var
                         ">>=" | moduleName (hatSourceRef expObj) == "Prelude" 
                           -> SAssoc 1 var
                         _ -> transFixity (hatInfix expObj))
                     ,[],[var])
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


-- useful document combinator
(<->) :: Doc -> Doc -> Doc
d1 <-> d2 = d1 <> delimiter " " <> d2 

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = d1 <> fdelimiter " " <> d2

(<|>) :: Doc -> Doc -> Doc
d1 <|> d2 = d1 <> fdelimiter "" <> d2
  
(<*>) :: Doc -> Doc -> Doc
d1 <*> d2 = d1 <|> text "," <> d2

isOpSym :: String -> Bool
isOpSym sym = let c = head sym in not (isAlpha c || c == '[') 

funDoc :: String -> Doc
funDoc var = (if isOpSym var then parens else id) $ text var 

opDoc :: String -> Doc
opDoc var =  text ((if isAlpha $ head var then ('`' :) . (++ "'") else id) var)


data ArgPos = ALeft | ARight
isRight ARight = True
isRight ALeft = False
isLeft = not . isRight

-- surround by parentheses if necessary
-- first fixity of surrounding expression, then if left or right argument,
-- then fixity of expression itself
optParens :: SFixity -> ArgPos -> SFixity -> Doc -> Doc
optParens surFixity aPos ownFixity =
  case (priority surFixity) `compare` (priority ownFixity) of
    LT -> if priority surFixity == (-1) then groupNest indentation else id
    GT -> groupNest indentation . parens
    EQ -> if (isInfixR surFixity && isInfixR ownFixity && isRight aPos)
            || (isInfixL surFixity && isInfixL ownFixity && isLeft aPos)
            || sameAssoc surFixity ownFixity 
            then id
            else groupNest indentation . parens

sameAssoc :: SFixity -> SFixity -> Bool
sameAssoc (SAssoc _ var1) (SAssoc _ var2) = True
sameAssoc _ _ = False

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 f c [] = c
foldr0 f c xs = foldr1 f xs

listDoc :: SFixity -> ArgPos -> SExp -> Doc
listDoc surFixity aPos e =
  case maybeRest of
    Nothing -> if all isCharLiteral elems 
                 then groupNest 1 $ 
                        text "\"" <> 
                        (foldr0 (<|>) nil (map (text . getChar) elems)) <> 
                        text "\""
                 else group $
                        text "[" <> (foldr0 (<*>) nil (map sExp2Doc elems)) <>
                        text "]"
    Just eRest -> groupNest indentation .
                    optParens surFixity aPos (SInfixR 5) . 
                    foldr (<:>) (sExp2Doc eRest) . 
                    map sExp2Doc $ elems 
  where
  (elems,maybeRest) = getListElems e
  d1 <*> d2 = d1 <|> text "," <> d2
  d1 <:> d2 = d1 <|> text ":" <|> d2
  getListElems :: SExp -> ([SExp],Maybe SExp)
  getListElems (SApp [SId ":" _,ee,er]) = (ee:ees,mr)
    where
    (ees,mr) = getListElems er
  getListElems (SId "[]" _) = ([],Nothing)
  getListElems e = ([],Just e)
  isCharLiteral :: SExp -> Bool
  isCharLiteral (SLiteral ('\'':_)) = True
  isCharLiteral _ = False
  getChar :: SExp -> String
  getChar (SLiteral ('\'':cs)) = init cs

priority (SInfix p) = p
priority (SInfixL p) = p
priority (SInfixR p) = p
priority (SAssoc p _) = p
priority SInfixDefault = 9

isInfixL (SInfixL _) = True
isInfixL SInfixDefault = True
isInfixL _ = False

isInfixR (SInfixR _) = True
isInfixR _ = False

isNotInfixDefault SInfixDefault = False
isNotInfixDefault _ = False

considerAsOperator :: String -> SFixity -> Bool
considerAsOperator var fixity =  isOpSym var || isNotInfixDefault fixity 

-- a central function
sExp2Doc :: SExp -> Doc 
sExp2Doc = goDoc (SInfix (-1)) ARight 

-- fixity of surrounding expression and which sort of argument
goDoc :: SFixity -> ArgPos -> SExp -> Doc
goDoc surFixity aPos (SApp ((SId (',':xs) _):args)) =
  if length xs + 2 == length args 
    then group (text "(" <> foldr1 (<*>) (map sExp2Doc args)) <> text ")"
         -- print tuple properly
    else optParens surFixity aPos ownFixity . 
           (text ("(,"++xs++")")  <+>) . 
           foldr1 (<+>) . map (goDoc ownFixity ARight) $ args
         -- partial application of tuple constructor
  where
  ownFixity = SInfix 10

goDoc surFixity aPos (e@(SApp [SId ":" _,_,_])) = listDoc surFixity aPos e
goDoc surFixity aPos (SApp [SId var ownFixity,e1,e2]) 
  | considerAsOperator var ownFixity = 
    optParens surFixity aPos ownFixity
      (goDoc ownFixity ALeft e1 <|> opDoc var <|> goDoc ownFixity ARight e2)
goDoc surFixity aPos (SApp [SId var ownFixity,e]) 
  | considerAsOperator var ownFixity =
  -- show infix operator with single argument as section
  groupNest indentation . parens $ goDoc ownFixity ALeft e <-> opDoc var
goDoc surFixity aPos (SApp (fun:args)) = 
  optParens surFixity aPos ownFixity . (goDoc ownFixity ALeft fun <+>) . 
    foldr1 (<+>) . map (goDoc ownFixity ARight) $ args
  where
  ownFixity = SInfix 10
goDoc _ _ (SId var fixity) = funDoc var
goDoc _ _ (SLiteral lit) = text lit
goDoc _ _ SLambda = text "(\\..)"
goDoc _ _ (SIf exp) = 
  groupNest indentation . parens $ text "if" <-> sExp2Doc exp
goDoc _ _ (SCase exp) = 
  groupNest indentation . parens $ text "case" <-> sExp2Doc exp
goDoc _ _ (SGuard exp) = 
  groupNest indentation . parens $ text "| " <> sExp2Doc exp
goDoc _ _ SInvalid = text "<cut>"
goDoc _ _ SUnevaluated = text "_"
goDoc _ _ SBottom = text "_|_"
goDoc _ _ (SCycle var exp) = 
  groupNest indentation . parens $
    text var <+> group (text "where" <+> sExp2Doc exp)


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
test1 = SApp [SId "fun" SInfixDefault,SLiteral "24",SLiteral "True",SApp[SId "+" (SInfixL 5),SLiteral "3",SLiteral "4"]]

test2 = SApp [SId "*" (SInfixL 6),SApp [SId "+" (SInfixL 5),SApp [SId "-" (SInfixL 5),SLiteral "3",SLiteral "6"],SApp [SId "-" (SInfixL 5),SLiteral "3",SLiteral "6"]],test1]
 