module TraceDerive where

import Syntax
import TraceId (TraceId,mkLambdaBound,tokenId,getUnqualified,dropModule
               ,tTokenTrue,tTokenFalse,tTokenEqualEqual,tTokenAndAnd)
import TokenId (mkUnqualifiedTokenId,tEq)

-- ----------------------------------------------------------------------------

-- derive instances for all given classes for a data/newtype
derive :: [Context TraceId] -> Simple TraceId -> [Constr TraceId] 
       -> [(Pos,TraceId)] -> [Decl TraceId]
derive tyContexts sTy constrs = 
  map (deriveClass tyContexts (simpleToType sTy) pTyVars constrs)
  where
  Simple _ _ pTyVars = sTy

-- derive instance for a given class for a data/newtype
deriveClass :: [Context TraceId] -> Instance TraceId -> [(Pos,TraceId)] 
            -> [Constr TraceId] -> (Pos,TraceId) -> Decl TraceId
deriveClass tyContexts instTy pTyVars constrs (pos,cls) 
  | getUnqualified cls == "Eq" = deriveEq pos usualContexts cls instTy constrs
  | getUnqualified cls == "Ord" = DeclIgnore "Missing derived instance"
  | getUnqualified cls == "Bounded" = DeclIgnore "Missing derived instance"
  | getUnqualified cls == "Enum" = DeclIgnore "Missing derived instance"
  | getUnqualified cls == "Read" = DeclIgnore "Missing derived instance"
  | getUnqualified cls == "Show" = DeclIgnore "Missing derived instance"
  | getUnqualified cls == "Ix" = DeclIgnore "Missing derived instance"
  | otherwise = error ("deriveClass: unknown class " ++ show (tokenId cls))
  where
  -- this is a HACK that covers only the common cases
  -- for correct result would need to implement full context reduction
  -- and take the least fixpoint
  usualContexts = tyContexts ++ map (Context pos cls) pTyVars

-- ----------------------------------------------------------------------------

deriveEq :: Pos 
         -> [Context TraceId] -> TraceId -> Instance TraceId 
         -> [Constr TraceId] 
         -> Decl TraceId
deriveEq pos contexts cls ty constrs =
  DeclInstance pos contexts cls ty 
    (DeclsParse 
      [DeclFun pos (dropModule tTokenEqualEqual)
        (map funEqConstr constrs ++ 
          [Fun [PatWildcard pos,PatWildcard pos] 
            (Unguarded (ExpCon pos tTokenFalse)) noDecls])])
  where
  vars = traceVars pos
  funEqConstr :: Constr TraceId -> Fun TraceId
  funEqConstr constr = 
    if arity == 0 
      then
        Fun [ExpCon pos conId,ExpCon pos conId] 
          (Unguarded (ExpCon pos tTokenTrue)) noDecls
      else
        Fun 
          [ExpApplication pos (ExpCon pos conId : argsL)
          , ExpApplication pos  (ExpCon pos conId : argsR)]
          (Unguarded (foldr1 andExp (zipWith equalExp argsL argsR)))
          noDecls
    where
    andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]
    equalExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    equalExp e1 e2 = ExpApplication pos [ExpVar pos tTokenEqualEqual,e1,e2]
    (argsL,restVars) = splitAt arity vars
    argsR = take arity restVars
    conId = getConstrId constr
    arity = constrArity constr

-- ----------------------------------------------------------------------------
-- helper functions

-- infinite list of variables
-- only need not conflict with names of standard class methods
traceVars :: Pos -> [Exp TraceId]
traceVars pos = 
  map (ExpVar pos . mkLambdaBound . mkUnqualifiedTokenId . ('y':) . show) [1..]









