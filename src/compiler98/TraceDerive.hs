module TraceDerive where

import Syntax
import TraceId 
  (TraceId,mkLambdaBound,tokenId,getUnqualified,dropModule
  ,tPriority,tFixity,Fixity(..)
  ,tTokenTrue,tTokenFalse,tTokenEqualEqual,tTokenAndAnd
  ,tTokenEQ,tTokenCompare,tTokenLocalFromEnum,tTokenInt
  ,tTokenMinBound,tTokenMaxBound,tTokenFromEnum,tTokenToEnum
  ,tTokenError,tTokenEnumFrom,tTokenEnumFromThen,tTokenEnumFromTo
  ,tTokenEnumFromThenTo,tTokenGreater,tTokenGreaterEqual,tTokenCompose
  ,tTokenShowsPrec,tTokenShowParen,tTokenShowString,tTokenShowChar
  ,tTokenReadsPrec,tTokenReadParen,tTokenYield,tTokenAlt,tTokenThenAp
  ,tTokenThenLex)
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
  | getUnqualified cls == "Ord" = 
    deriveOrd pos usualContexts cls instTy constrs
  | getUnqualified cls == "Bounded" =
    deriveBounded pos usualContexts cls instTy constrs
  | getUnqualified cls == "Enum" = 
    deriveEnum pos usualContexts cls instTy constrs
  | getUnqualified cls == "Read" = 
    deriveRead pos usualContexts cls instTy constrs
  | getUnqualified cls == "Show" = 
    deriveShow pos usualContexts cls instTy constrs
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

deriveOrd :: Pos 
          -> [Context TraceId] -> TraceId -> Instance TraceId 
          -> [Constr TraceId] 
          -> Decl TraceId
deriveOrd pos contexts cls ty constrs =
  DeclInstance pos contexts cls ty 
    (DeclsParse 
      [DeclFun pos (dropModule tTokenCompare)
        (concatMap funCompareEqConstr constrs ++ 
          [Fun [var1,var2] 
            (Unguarded 
              (ExpApplication pos 
                [ExpVar pos tTokenCompare
                ,ExpType pos (ExpApplication pos [localFromEnumVar,var1])
                   [] (TypeCons pos tTokenInt [])
                ,ExpApplication pos [localFromEnumVar,var2]]))
            (DeclsParse 
              [DeclFun pos tTokenLocalFromEnum
                (zipWith funLocalFromEnum constrs [0..])])])])
  where
  var1:var2:vars = traceVars pos
  localFromEnumVar = ExpVar pos tTokenLocalFromEnum
  funCompareEqConstr constr = 
    if arity == 0 then []
      else
        [Fun
          [ExpApplication pos (ExpCon pos conId : argsL)
          ,ExpApplication pos (ExpCon pos conId : argsR)]
          (Unguarded (foldr1 caseExp (zipWith compareExp argsL argsR)))
          noDecls]
    where
    caseExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    caseExp e1 e2 = 
      ExpCase pos e1 
        [Alt (ExpCon pos tTokenEQ) (Unguarded e2) noDecls
        ,Alt var1 (Unguarded var1) noDecls]
    compareExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
    compareExp e1 e2 = ExpApplication pos [ExpVar pos tTokenCompare,e1,e2]
    (argsL,restVars) = splitAt arity vars
    argsR = take arity restVars
    conId = getConstrId constr
    arity = constrArity constr
  funLocalFromEnum constr num =
    Fun [ExpApplication pos (ExpCon pos conId : args)]
      (Unguarded (ExpLit pos (LitInteger Boxed num)))
      noDecls
    where
    args = replicate (constrArity constr) (PatWildcard pos)
    conId = getConstrId constr


-- ----------------------------------------------------------------------------

deriveBounded :: Pos 
              -> [Context TraceId] -> TraceId -> Instance TraceId 
              -> [Constr TraceId] 
              -> Decl TraceId
deriveBounded pos contexts cls ty constrs =
  DeclInstance pos contexts cls ty 
    (if all (== 0) (map constrArity constrs) 
      then
        (DeclsParse
          [DeclFun pos (dropModule tTokenMinBound) 
            [Fun [] (Unguarded (ExpCon pos (getConstrId (head constrs)))) 
              noDecls]
          ,DeclFun pos (dropModule tTokenMaxBound) 
            [Fun [] (Unguarded (ExpCon pos (getConstrId (last constrs)))) 
              noDecls]])
      else {- exactly one constructor -}
        let [constr] = constrs in
          (DeclsParse
            [DeclFun pos (dropModule tTokenMinBound) 
              [Fun [] (Unguarded 
                  (ExpApplication pos 
                    (ExpCon pos (getConstrId (head constrs)) 
                    : replicate (constrArity constr) 
                        (ExpVar pos tTokenMinBound)) )) 
              noDecls]
            ,DeclFun pos (dropModule tTokenMaxBound) 
              [Fun [] (Unguarded 
                  (ExpApplication pos 
                    (ExpCon pos (getConstrId (head constrs)) 
                    : replicate (constrArity constr) 
                        (ExpVar pos tTokenMaxBound)) )) 
              noDecls]]))

-- ----------------------------------------------------------------------------

deriveEnum :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveEnum pos contexts cls ty constrs =
  -- assert: all (== 0) (map constrArity constrs) 
  DeclInstance pos contexts cls ty
    (DeclsParse
      [DeclFun pos (dropModule tTokenFromEnum) 
        (zipWith funFromEnum constrs [0..])
      ,DeclFun pos (dropModule tTokenToEnum) 
        (zipWith funToEnum constrs [0..] ++ 
          [Fun [PatWildcard pos] (Unguarded 
            (ExpApplication pos 
              [ExpVar pos tTokenError
              ,ExpLit pos (LitString Boxed "toEnum: argument out of bounds")]))
            noDecls])
      ,DeclFun pos (dropModule tTokenEnumFrom)
        [Fun [var1] (Unguarded 
          (ExpApplication pos
            [ExpVar pos tTokenEnumFromTo,var1
            ,ExpCon pos (getConstrId (last constrs))]))
          noDecls]
      ,DeclFun pos (dropModule tTokenEnumFromThen)
        [Fun [var1,var2] (Unguarded
          (ExpApplication pos
            [ExpVar pos tTokenEnumFromThenTo,var1,var2
            ,ExpIf pos
              (ExpApplication pos 
                [ExpVar pos tTokenGreaterEqual
                ,ExpApplication pos [ExpVar pos tTokenFromEnum,var1]
                ,ExpApplication pos [ExpVar pos tTokenFromEnum,var2]])
              (ExpCon pos (getConstrId (last constrs)))
              (ExpCon pos (getConstrId (head constrs)))]))
          noDecls]
      ])
  where
  var1:var2:_ = traceVars pos
  funFromEnum constr num =
    Fun [ExpCon pos (getConstrId constr)] 
      (Unguarded (ExpLit pos (LitInteger Boxed num)))
      noDecls
  funToEnum constr num =
    Fun [ExpLit pos (LitInteger Boxed num)] 
      (Unguarded (ExpCon pos (getConstrId constr)))
      noDecls

-- ----------------------------------------------------------------------------

deriveShow :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveShow pos contexts cls ty constrs =
  DeclInstance pos contexts cls ty
    (DeclsParse
      [DeclFun pos (dropModule tTokenShowsPrec) 
        (map funShowsPrec constrs)])
  where
  precVar:vars = traceVars pos
  funShowsPrec constr =
    Fun [precVar,ExpApplication pos (ExpCon pos conId:args)] (Unguarded
      (if arity == 0 
         then showStringExp (getUnqualified conId)
         else
           case getUnqualified conId of
             ':':_ -> -- is defined in infix style (and arity == 2)
               ExpApplication pos 
                 [ExpVar pos tTokenShowParen
                 ,ExpApplication pos 
                   [ExpVar pos tTokenGreater,precVar
                   ,ExpLit pos 
                     (LitInteger Boxed priority)]
                 ,  showPrec priorityPlus1 (head args) 
                  `compose`
                    showStringExp (' ' : getUnqualified conId ++ " ")
                  `compose`
                    showPrec priorityPlus1 (args!!1)
                 ]
             _ ->
               let labels = map snd . getConstrLabels $ constr in
               if null labels 
                 then
                   ExpApplication pos 
                     [ExpVar pos tTokenShowParen
                     ,ExpApplication pos 
                       [ExpVar pos tTokenGreater,precVar
                       ,ExpLit pos (LitInteger Boxed priority)]
                     ,showStringExp (getUnqualified conId ++ " ")
                        `compose`
                       foldr1 composeSpace (map (showPrec priorityPlus1) args)
                     ]
                 else
                   showStringExp (getUnqualified conId ++ "{")
                     `compose`
                     foldr1 composeComma (zipWith showField labels args)
                     `compose`
                     showCharExp '}'
      ))
      noDecls
    where
    showStringExp s = 
      ExpApplication pos 
        [ExpVar pos tTokenShowString
        ,ExpLit pos (LitString Boxed s)]
    showCharExp c = 
      ExpApplication pos 
        [ExpVar pos tTokenShowChar
        ,ExpLit pos (LitChar Boxed c)]
    e1 `compose` e2 = ExpApplication pos [ExpVar pos tTokenCompose,e1,e2]
    e1 `composeSpace` e2 = e1 `compose` showCharExp ' ' `compose` e2
    e1 `composeComma` e2 = e1 `compose` showCharExp ',' `compose` e2
    showField label e = 
      showStringExp (getUnqualified label) `compose` showCharExp '=' 
        `compose` showPrec 0 e 
    showPrec d e = 
      ExpApplication pos 
        [ExpVar pos tTokenShowsPrec,ExpLit pos (LitInteger Boxed d),e]
    args = take arity vars
    conId = getConstrId constr
    arity = constrArity constr
    priority = toInteger (tPriority conId)
    priorityPlus1 = priority+1 

      
-- ----------------------------------------------------------------------------

deriveRead :: Pos 
           -> [Context TraceId] -> TraceId -> Instance TraceId 
           -> [Constr TraceId] 
           -> Decl TraceId
deriveRead pos contexts cls ty constrs =
  DeclInstance pos contexts cls ty
    (DeclsParse
      [DeclFun pos (dropModule tTokenReadsPrec) 
        [Fun [precVar] (Unguarded (foldr1 alt . map expReadsPrec $ constrs)) 
          noDecls]])
  where
  precVar:_ = traceVars pos
  e1 `alt` e2 = ExpApplication pos [ExpVar pos tTokenAlt,e1,e2]
  expReadsPrec constr =
    if arity == 0 
      then readParen (ExpCon pos tTokenFalse) 
             (yield (ExpCon pos conId) `thenLex` getUnqualified conId)
      else
        case getUnqualified conId of
          ':':_ -> -- is defined in infix style (and arity == 2)
            readParen precGreaterPriority
              (yield (ExpCon pos conId) `thenAp`
                readsArg `thenLex` getUnqualified conId `thenAp` readsArg)
          _ ->
           let labels = map snd . getConstrLabels $ constr in
           if null labels 
             then
               readParen precGreaterPriority
                 (foldl thenAp 
                    (yield (ExpCon pos conId) `thenLex` getUnqualified conId)
                    (replicate arity readsArg))
             else
               (foldl thenCommaField
                 (yield (ExpCon pos conId) `thenLex` getUnqualified conId 
                   `thenLex` "{" `thenField` (head labels))
                 (tail labels))
                 `thenLex` "}" 
    where
    infixl 6 `thenAp`,`thenLex`, `thenField`
    readParen c e = ExpApplication pos [ExpVar pos tTokenReadParen,c,e]
    yield e = ExpApplication pos [ExpVar pos tTokenYield,e]
    e1 `thenLex` s = ExpApplication pos [ExpVar pos tTokenThenLex,e1,string s]
    e1 `thenAp` e2 = ExpApplication pos [ExpVar pos tTokenThenAp,e1,e2]
    precGreaterPriority = 
      ExpApplication pos 
        [ExpVar pos tTokenGreater,precVar
        ,ExpLit pos (LitInteger Boxed priority)]
    string s = ExpLit pos (LitString Boxed s)
    readsArg = 
      ExpApplication pos 
        [ExpVar pos tTokenReadsPrec
        ,ExpLit pos (LitInteger Boxed priorityPlus1)]
    readsArg0 = 
      ExpApplication pos 
        [ExpVar pos tTokenReadsPrec
        ,ExpLit pos (LitInteger Boxed 0)]
    p `thenField` label = 
      p `thenLex` getUnqualified label `thenLex` "=" 
        `thenAp` readsArg0 
    p `thenCommaField` label = p `thenLex` "," `thenField` label
    conId = getConstrId constr
    arity = constrArity constr
    priority = toInteger (tPriority conId)
    priorityPlus1 = priority+1 


-- ----------------------------------------------------------------------------
-- helper functions

-- infinite list of variables
-- only need not conflict with names of standard class methods
traceVars :: Pos -> [Exp TraceId]
traceVars pos = 
  map (ExpVar pos . mkLambdaBound . mkUnqualifiedTokenId . ('y':) . show) [1..]









