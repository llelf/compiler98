{- ---------------------------------------------------------------------------
Restructure expressions with infix operators according to associativity
and precedence.  This variant of the infix resolution code is only
used when performing the tracing transformation.  When doing ordinary
compilation, resolving infix expressions is part of the Rename phase.
-}

module AuxFixity(fixInfixList) where

import PackedString(PackedString,packString,unpackPS)
import Extra(Pos(..),strPos,pair)
import Syntax
import SyntaxPos
import TokenId(TokenId(..), t_x, t_y, t_flip, tminus, tnegate, visImport)
import AssocTree
import PreImp
import AuxFile


{-
-- Main function of the module.
-}
fixInfixList :: AuxTree -> [Exp TokenId] -> Exp TokenId

fixInfixList env [] = error "I: fixInfix []"
fixInfixList env ees@(ExpVarOp pos op:es) =
  let fix = lookupFix env Var op
      exp' = reorder env es
      exp  = invertCheck pos op fix env exp'
--in ExpLambda pos [varx,vary] 
--                  (ExpApplication pos [ExpVar pos op,vary,varx])
--  where
--  varx = ExpVar pos t_x
--  vary = ExpVar pos t_y
  in if op == tminus
     then (ExpApplication pos [ExpVar pos tnegate, exp])
     else (ExpApplication pos [ExpVar pos t_flip, ExpVar pos op, exp])
             -- desugaring with flip better than lambda for reading a trace
fixInfixList env ees@(ExpConOp pos op:es) =
  let fix = lookupFix env (Con "") op
  in case fix of
	(Pre a,l) -> reorder env ees
	_ -> let exp' = reorder env es
                 exp  = invertCheck pos op fix env exp'
--           in ExpLambda pos [varx,vary] 
--                  (ExpApplication pos [ExpVar pos op,vary,varx])
--  where
--  varx = ExpVar pos t_x
--  vary = ExpVar pos t_y
             in (ExpApplication pos 
                     [ExpVar pos t_flip, ExpCon pos op, exp]) 
             -- desugaring with flip better than lambda for reading a trace
fixInfixList env ees =
  case last ees of
    ExpConOp pos op -> let fix  = lookupFix env (Con "") op
			   exp' = reorder env (init ees)
                           exp  = invertCheck pos op fix env exp'
                       in (ExpApplication pos [ExpCon pos op,exp])
    ExpVarOp pos op -> let fix  = lookupFix env Var op
			   exp' = reorder env (init ees)
                           exp  = invertCheck pos op fix env exp'
                       in (ExpApplication pos [ExpVar pos op,exp])
    _ -> reorder env ees



-- the primary function that re-orders an infix list into a single expression
reorder :: AuxTree -> [Exp TokenId] -> Exp TokenId
reorder env es = getExp env [] [] es


-- getExp and getOp together `parse' the list of expressions into a
-- single tree-shaped expression.  We expect the list to take the form
-- [exp,op,exp,op,exp,op...] i.e. exps and ops alternate.  Of course,
-- there are exceptions: where the whole expr is a left or right section
-- of an operator; and where a prefix operator starts the show.

getExp :: AuxTree			-- environment (contains fixity info)
	-> [((Fixity,Int),(Exp TokenId,Int))] -- stack of operators already seen
	-> [Exp TokenId]		-- stack of outstanding non-op exprs
	-> [Exp TokenId]		-- input list of exprs
	-> Exp TokenId			-- re-combined output

getExp env ops exps (e:es) =
  case e of
    ExpConOp pos o ->
      let fix = lookupFix env (Con "") o in
        case fix of
	  (Pre a,l) -> getExp env (stackPrefix fix (ExpCon pos o):ops) exps es
    ExpVarOp pos o ->
      let fix = lookupFix env Var o in
        case fix of
	  (Pre a,l) -> getExp env (stackPrefix fix (ExpVar pos o):ops) exps es
    _ -> getOp env ops (e:exps) es
getExp env ops [] [] =
   error ("Problem with infix section at unknown location.")
getExp env ops (e:es) [] =
   error ("Problem with infix section at "++strPos (getPos e))


getOp :: AuxTree			-- environment (contains fixity info)
	-> [((Fixity,Int),(Exp TokenId,Int))]	-- stack of operators
	-> [Exp TokenId]		-- stack of outstanding non-op exprs
	-> [Exp TokenId]		-- input list of exprs
	-> Exp TokenId			-- recombined output

getOp env ops exps [] = finish ops exps
getOp env ops exps ees@(ExpConOp pos op:es) =
  case harder pos ops (Con "") op env of
    Just (o,ops) -> getOp env ops (rebuild o exps) ees
    Nothing      -> let fop = stackInfix env (ExpCon pos op)
                    in getExp env (fop:ops) exps es
getOp env ops exps ees@(ExpVarOp pos op:es) =
  case harder pos ops Var op env of
    Just  (o,ops) -> getOp env ops (rebuild o exps) ees
    Nothing       -> let fop = stackInfix env (ExpVar pos op)
                     in getExp env (fop:ops) exps es
getOp env ops exps (e:es) =
   error ("Need infix operator at " ++ strPos (getPos e))


stackInfix :: AuxTree -> Exp TokenId -> ((Fixity,Int),(Exp TokenId,Int))
stackInfix env op@(ExpVar _ o) = (lookupFix env Var o     , (op,2::Int))
stackInfix env op@(ExpCon _ o) = (lookupFix env (Con "") o, (op,2::Int))

stackPrefix fix op = (fix,(op,1::Int))
 

-- harder decides whether a new operator has lower precedence than
-- the top of the operator stack.  If so, we can pop the stack and
-- rebuild the accumulated expression so far before continuing.
-- If however it has higher precedence, then we must push the new
-- operator onto the stack and keep going.  If it has equal precedence,
-- then left/right associativity must be taken into account.

harder :: Pos				-- position
	-> [((Fixity,Int),(b,c))]	-- stack of operators
	-> (String->Identifier)		-- helper for lookup in environment
	-> TokenId			-- operator name
	-> AuxTree			-- environment holding fixity info
	-> Maybe (((Fixity,Int),(b,c)),[((Fixity,Int),(b,c))])
harder pos [] kind op' env = Nothing
harder pos (ipop@((inf,pri),(_,_)):ops) kind op' env =
  let (inf',pri') = lookupFix env kind op' in
  if pri > pri' then
    Just (ipop,ops)
  else if pri == pri' then
    case inf of
      Def   -> Just (ipop,ops)
      L     -> Just (ipop,ops)
      Pre _ -> Just (ipop,ops)
      R     -> Nothing
      None  -> error ("Infix operator at "++strPos pos++" is non-associative.")
  else Nothing



-- finish transforms the two stacks (operators + exprs) into the final
-- recombined expression.
finish :: [((Fixity,c),(Exp TokenId,Int))]	-- stack of operators
          -> [Exp TokenId]			-- stack of non-op exprs
          -> Exp TokenId			-- output expr
finish [] []  = error "AuxFixity.finish: empty" 
finish [] [e] = e
finish [] _   = error "AuxFixity.finish: multiple expression"
finish (o:ops) es = finish ops (rebuild o es)


-- rebuild the expression stack, by combining the top two items with
-- the given operator.  Precondition: the operator must have higher
-- precedence than any operators between the exprs lower in the stack.

rebuild :: ((Fixity,c),(Exp TokenId,Int))	-- operator
           -> [Exp TokenId]			-- stack of expressions
           -> [Exp TokenId]			-- juggled the stack
rebuild (_,(op,2)) (e1:e2:es) =
        ExpApplication (getPos op) [op,e2,e1]:es
rebuild ((Pre fun,_) ,(op,_)) (e1:es) =
        ExpApplication (getPos op) [ExpVar (getPos op) (visImport fun),e1]:es
rebuild (_,(op,n)) es =
        error ("Not enough arguments at " ++ strPos (getPos op))


-- discover fixity information from the .hx file info.
lookupFix :: AuxTree -> (String->Identifier) -> TokenId -> (Fixity,Int)
lookupFix env kind op =
    case lookupAT env (kind (show op)) of
	Just info -> (fixity info, priority info)
	Nothing   -> error ("No auxinfo for operator "++show op)


leftFixity :: Fixity -> Bool
leftFixity L       = True
leftFixity Def     = True
leftFixity (Pre _) = True
leftFixity _       = False     		--- !!! Cheating Infix is InfixR  (??)


-- 'invertCheck' checks for priority inversion in an operator section.
invertCheck :: Show a => Int -> a -> (b,Int) -> AuxTree
		-> Exp TokenId -> Exp TokenId
invertCheck pos1 op1 (fix1,pri1) env exp =
  case exp of
    ExpApplication _ (ExpVar pos2 op2: es) -> check Var      pos2 op2
    ExpApplication _ (ExpCon pos2 op2: es) -> check (Con "") pos2 op2
    _ -> exp
  where
    check kind pos2 op2 =
      let (fix2,pri2) = lookupFix env kind op2
      in if pri2 < pri1 then
        error ("Fixity problem:\n  "
              ++show op1++" used at "++strPos pos1++" has precedence "
              ++show pri1++",\n  "
              ++show op2++" used at "++strPos pos2++" has precedence "
              ++show pri2++".\n  "
              ++"The partially applied operator "++show op1
              ++" should have lower precedence\n  "
              ++"than the fully-applied operator "
              ++show op2++" used inside the section.\n")
      else exp

{- --------------------------------------------------------------------------}
