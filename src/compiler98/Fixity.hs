{- ---------------------------------------------------------------------------
Restructure expressions with infix operators according to associativity and
precedence.
-}
module Fixity(fixInfixList) where

import PackedString(PackedString,packString,unpackPS)
import Extra(Pos(..),strPos,pair)
import Syntax
import SyntaxPos
import TokenId(TokenId(..),t_Lambda,t_x)
import IdKind(IdKind(..))
import State
import AssocTree
import PreImp
import RenameLib


-- Just == Bind
-- Nothing == Stack

reorder es = getExp [] [] es

getExp ops exps (e:es) =
  case e of
    ExpConOp pos o ->
      fixTid Con o >>>= \ fix ->
        case fix of
	  (InfixPre a,l) -> getExp (stackPrefix fix (ExpCon pos o):ops) exps es
    ExpVarOp pos o ->
      fixTid Var o >>>= \ fix ->
        case fix of
	  (InfixPre a,l) -> getExp (stackPrefix fix (ExpVar pos o):ops) exps es
    _ ->
      getOp ops (e:exps) es

getOp ops exps [] = finish ops exps
getOp ops exps ees@(ExpConOp pos op:es) =
  harder pos ops Con op >>>= \ lr ->  
  case lr of
    Just (o,ops) -> getOp   ops          (rebuild o exps) ees
    Nothing      -> stackInfix (ExpCon pos op) >>>= \ fop -> getExp  (fop:ops) exps es
getOp ops exps ees@(ExpVarOp pos op:es) =
  harder pos ops Var op >>>= \ lr ->  
  case lr of
    Just  (o,ops) -> getOp   ops          (rebuild o exps) ees
    Nothing       -> stackInfix (ExpVar pos op) >>>= \ fop -> getExp  (fop:ops) exps es
getOp ops exps (e:es) =
   error ("Need infix operator at " ++ strPos (getPos e))
 

finish [] []   = error "finish empty" 
finish [] [e] = unitS e
finish [] _   = error "finish multiple expression"
finish (o:ops) es = finish ops (rebuild o es)

        
stackInfix op@(ExpVar _ o) = fixTid Var o >>>= \ fix -> unitS (fix,(op,2::Int))
stackInfix op@(ExpCon _ o) = fixTid Con o >>>= \ fix -> unitS (fix,(op,2::Int))

stackPrefix fix op = (fix,(op,1::Int))

--harder :: Pos -> [((InfixClass a,Int),(g,f))] -> IdKind -> e 
--		-> State (b,(e -> TokenId),c,d) RenameState (Maybe ((((InfixClass a),Int),(g,f)),[((InfixClass a,Int),(g,f))])) RenameState 

harder pos [] kind op' = unitS Nothing
harder pos (ipop@((inf,pri),(op,_)):ops) kind op' =
  fixTid kind op' >>>= \ (inf',pri') ->
  if pri > pri' then
    unitS (Just (ipop,ops))
  else if pri == pri' then
    case inf of
      InfixDef   -> unitS (Just (ipop,ops))
      InfixL     -> unitS (Just (ipop,ops))
      InfixPre _ -> unitS (Just (ipop,ops))
      InfixR     -> unitS (Nothing)
      Infix      -> renameError ("Infix operator at " ++ strPos pos ++ " is non-associative.") (Just (ipop,ops))
  else unitS Nothing


stripExp (ExpVar _ o) = o
stripExp (ExpCon _ o) = o

rebuild (_,(op,2)) (e1:e2:es) = ExpApplication (getPos op) [op,e2,e1]:es
rebuild ((InfixPre fun,_) ,(op,_)) (e1:es) =
        ExpApplication (getPos op) [ExpVar (getPos op) fun,e1]:es
rebuild (_,(op,n)) es =
        error ("Not enough arguments at " ++ strPos (getPos op))

leftFixity InfixDef = True
leftFixity InfixL = True
leftFixity (InfixPre _) = True
leftFixity _ = False      		--- !!! Cheating Infix is InfixR

{-
Main function of the module.
-}
fixInfixList :: [Exp TokenId] -> RenameMonad (Exp TokenId)

fixInfixList [] = error "I: fixInfix []"
fixInfixList ees@(ExpVarOp pos op:es) =
  fixTid Var op >>>= \ fix ->
        case fix of
	  (InfixPre a,l) -> reorder ees
	  _ -> reorder es >>>= \ exp -> 
               unitS (ExpLambda pos [ExpVar pos t_x] 
                        (ExpApplication pos 
                           [ExpVar pos op, ExpVar pos t_x, exp]))
fixInfixList ees@(ExpConOp pos op:es) =
  fixTid Con op >>>= \ fix ->
        case fix of
	  (InfixPre a,l) -> reorder ees
	  _ -> reorder es >>>= \ exp -> 
               unitS (ExpLambda pos [ExpVar pos t_x] 
                        (ExpApplication pos 
                           [ExpCon pos op, ExpVar pos t_x, exp]))
fixInfixList ees =
  case last ees of
    ExpConOp pos op -> reorder (init ees) >>>= \ exp -> 
                       unitS (ExpApplication pos [ExpCon pos op,exp])
    ExpVarOp pos op -> reorder (init ees) >>>= \ exp -> 
                       unitS (ExpApplication pos [ExpVar pos op,exp])
    _ -> reorder ees

{- --------------------------------------------------------------------------}