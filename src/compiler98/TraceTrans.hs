{- ---------------------------------------------------------------------------
Transform a module for generating a trace.

Names are changed.
Module names are prefixed by a 'T'.
Variable names are prefixed to make room for new variable names 
refering to various traces and intermediate expressions.
Details of new name scheme near the end of this module.

Deferred:
Records: selectors won't have right type.
import/export of the form TyCon(..); need extension of module TraceId

In multi-module program may get name conflict, because also names internal to
a module may be exported. Would need to keep track of all top-level names
that should be exported by default. 

No monad is used in the transformation, 
because there is nothing inherently sequential.
Instead, the definitions of the transformation functions `t*' remind of an 
attribut grammar: the arguments are the inherited attributes, the elements
of the result tuples are the synthetic attributes.
---------------------------------------------------------------------------- -}

module TraceTrans (traceTrans) where

import Syntax
import SyntaxPos (HasPos(getPos))
import TokenId (TokenId(TupleId,Visible,Qualified)
               ,qualify,visible,extractV
               ,tPrelude,t_Tuple,t_Arrow,tTrue,tFalse,t_otherwise,t_undef
               ,tMain,tmain,tseq)
import PackedString (PackedString,packString,unpackPS)
import Extra (Pos,noPos,strPos,fromPos)
import TraceId (TraceId,tokenId,arity,isLambdaBound,fixPriority,just
               ,tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail)
import AuxFile (AuxiliaryInfo)	-- needed only for hbc's broken import mechanism
import List (isPrefixOf,union,partition)
import Char (isAlpha,digitToInt)

infixr 6 `typeFun`	-- hbc won't let me declare this later.

type Arity = Int

-- ----------------------------------------------------------------------------
-- Transform a module

traceTrans :: Bool    -- transform for tracing (not for non-tracing)
           -> String  -- filename of module 
           -> String  -- base filename for the trace files (without extension)
           -> Module TraceId -> Module TokenId
traceTrans traced filename traceFilename 
 (Module pos modId exps impDecls fixDecls decls) =
  Module pos
    (nameTransModule modId)
    (if isMain modId then Just [] {- export everything -} else tExports exps)
    (tImpDecls impDecls)
    [] -- no fix info needed, because pretty printed output not ambiguous
    (DeclsParse 
      (decls' 
       ++ [defNameMod pos modId filename traced]
       ++ map (defNameCon modTrace) cons 
       ++ map (defNameVar True modTrace) tvars 
       ++ map (defNameVar False modTrace) vars 
       ++ (if traced then map (defNamePos modTrace) poss else [])
       ++ if isMain modId then [defMain traceFilename] else [] ))
  where
  modTrace = ExpVar pos (nameTraceInfoModule modId)
  (poss,tvars,vars,cons) = getModuleConsts consts
  (DeclsParse decls',consts) = tDecls traced (ExpVar pos tokenMkTRoot) decls

-- ----------------------------------------------------------------------------
-- construct new main function definition

-- main = do
--  T.openTrace "artFilename"
--  case omain Prelude.undefined Prelude.undefined of
--    T.R v _ -> v
--  T.closeTrace
defMain :: String -> Decl TokenId

defMain artFilename =
  DeclFun noPos tokenmain
    [Fun [] (Unguarded (ExpDo noPos 
      [StmtExp (ExpApplication noPos 
                 [ExpVar noPos tokenOpenTrace
                 ,ExpLit noPos (LitString Boxed artFilename)])
      ,StmtExp (ExpCase noPos 
                 (ExpVar noPos tokenomain)
                 [Alt (ExpApplication noPos 
                        [ExpVar noPos tokenR
                        ,ExpVar noPos tokenValue
                        ,PatWildcard noPos]) 
                      (Unguarded (ExpVar noPos tokenValue)) noDecls ])
      ,StmtExp (ExpVar noPos tokenCloseTrace)
      ]))
      noDecls]
  where
  tokenmain = visible (reverse "main")
  tokenomain = nameTransVar (just tmain)
  tokenValue = visible (reverse "value")

-- ----------------------------------------------------------------------------
-- Transform imports and exports

tExports :: Maybe [Export TraceId] -> Maybe [Export TokenId]
tExports = fmap (concatMap tExport) 

tExport :: Export TraceId -> [Export TokenId]
tExport (ExportModid pos modId) = [ExportModid pos (nameTransModule modId)]
tExport (ExportEntity pos entity) = map (ExportEntity pos) (tEntity entity)


tImpDecls :: [ImpDecl TraceId] -> [ImpDecl TokenId]
tImpDecls decls = 
  (if any importsPreludeOrOriginal decls 
     then id 
     else ((Import (noPos,nameTransModule (just tPrelude)) (Hiding [])) :))
    ((ImportQ (noPos,tPrelude) (Hiding []))
     -- ^ hide the standard Prelude 
    :(ImportQas (noPos,Visible tracingModule) 
       (noPos,Visible tracingModuleShort) (Hiding [])) 
    :(map tImpDecl decls))
  where 
  importsPreludeOrOriginal :: ImpDecl TraceId -> Bool
  importsPreludeOrOriginal impDecl = 
    let id = importedModule impDecl
    in tokenId id == tPrelude || "TraceOrig" `isPrefixOf` (getUnqualified id)

tImpDecl :: ImpDecl TraceId -> ImpDecl TokenId
tImpDecl (Import (pos,id) spec) = 
  Import (pos,nameTransModule id) (tImpSpec spec)
tImpDecl (ImportQ (pos,id) spec) =
  ImportQ (pos,nameTransModule id) (tImpSpec spec)
tImpDecl (ImportQas (pos1,id1) (pos2,id2) spec) =
  ImportQas (pos1,nameTransModule id1) (pos2,nameTransModule id2) 
    (tImpSpec spec)
tImpDecl (Importas (pos1,id1) (pos2,id2) spec) =
  Importas (pos1,nameTransModule id1) (pos2,nameTransModule id2) 
    (tImpSpec spec)

tImpSpec :: ImpSpec TraceId -> ImpSpec TokenId
tImpSpec (NoHiding entities) = NoHiding (concatMap tEntity entities)
tImpSpec (Hiding entities)   = Hiding (concatMap tEntity entities)

tEntity :: Entity TraceId -> [Entity TokenId]
tEntity (EntityVar pos id) = [EntityVar pos (nameTransVar id)]
tEntity (EntityTyConCls pos id) = [EntityTyConCls pos (nameTransTyConCls id)]
  -- INCOMPLETE: if TyCon(..) need also to import/export references to traces
  -- of data constructor names, but how know their names?
tEntity (EntityTyCon pos id posConIds) = 
  (EntityTyCon pos (nameTransTyConCls id) (mapSnd nameTransCon posConIds))
  : map (\(pos,id) -> EntityVar pos (nameTraceInfoCon id)) posConIds
tEntity (EntityTyCls pos id posVarIds) =
  [EntityTyCls pos (nameTransTyConCls id) (mapSnd nameTransVar posVarIds)]

-- ----------------------------------------------------------------------------
-- New top-level definitions for generating shared trace info
-- 
-- Trace info for positions and identifier information. They have to be 
-- top-level, so that they (and their side-effect) are only evaluated once.
-- INCOMPLETE: an optimising compiler may need noinline pragma. 
-- The variables referring to variable information need to include the 
-- position in the name, because the same variable name may be used several 
-- times.

defNameMod :: Pos -> TraceId -> String -> Bool -> Decl TokenId
defNameMod pos id filename traced =
  DeclFun pos (nameTraceInfoModule id) 
    [Fun [] 
      (Unguarded 
        (ExpApplication pos 
          [ExpVar pos tokenMkModule
          ,ExpLit pos (LitString Boxed (getUnqualified id))
          ,ExpLit pos (LitString Boxed filename)
          ,ExpCon pos (if traced then tTrue else tFalse)])) 
      noDecls]

defNameCon :: Exp TokenId -> (Pos,TraceId) -> Decl TokenId
defNameCon modTrace (pos,id) =
  DeclFun pos (nameTraceInfoCon id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          [ExpVar pos tokenMkAtomCon
          ,modTrace
          ,ExpLit pos (LitInt Boxed (encodePos pos))
          ,ExpLit pos (LitInt Boxed (fixPriority id))
          ,ExpLit pos (LitString Boxed (getUnqualified id))]))
      noDecls]

defNameVar :: Bool -> Exp TokenId -> (Pos,TraceId) -> Decl TokenId
defNameVar toplevel modTrace (pos,id) =
  DeclFun pos (nameTraceInfoVar pos id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          [ExpVar pos (tokenMkAtomId toplevel)
          ,modTrace
          ,ExpLit pos (LitInt Boxed (encodePos pos))
          ,ExpLit pos (LitInt Boxed (fixPriority id))
          ,ExpLit pos (LitString Boxed (getUnqualified id))]))
      noDecls]

defNamePos :: Exp TokenId -> Pos -> Decl TokenId
defNamePos modTrace pos =
  DeclFun pos (nameTraceInfoPos pos)
    [Fun []
      (Unguarded
        (ExpApplication pos
          [ExpVar pos tokenMkPos
          ,modTrace
          ,ExpLit pos (LitInt Boxed (encodePos pos))]))
      noDecls]

encodePos :: Pos -> Int
encodePos pos = 10000*row + col -- encoding of positions in trace file
  where 
  (row, col) = fromPos pos

-- ----------------------------------------------------------------------------
-- abstract data type 
-- implements sets of positions, defined this-level and local variables, 
-- and defined constructors (no duplicates)
-- this-level means defined on the currently considered declaration level,
-- local means defined in some declaration local to the current declaration.
-- variables and constructors come with the position at which they are defined
-- precondition: variables and constructors are only added once
-- because same position may be used for a variable, an application etc,
-- a position may be added several times.

data ModuleConsts = MC [Pos]  -- positions used in traces
                       [(Pos,TraceId)]  -- this-level variable ids for traces
                       [(Pos,TraceId)]  -- variable ids for use in traces
                       [(Pos,TraceId)]  -- constructor ids for use in traces

emptyModuleConsts :: ModuleConsts
emptyModuleConsts = MC [] [] [] []

addPos :: Pos -> ModuleConsts -> ModuleConsts
addPos pos (MC poss tids ids cons) = MC (pos `insert` poss) tids ids cons

addVar :: Pos -> TraceId -> ModuleConsts -> ModuleConsts
addVar pos id (MC poss tids ids cons) = 
  MC (pos `insert` poss) ((pos,id):tids) ids cons

addCon :: Pos -> TraceId -> ModuleConsts -> ModuleConsts
addCon pos id (MC poss tids ids cons) =
  MC (pos `insert` poss) tids ids ((pos,id):cons)

-- both from the same declaration level
merge :: ModuleConsts -> ModuleConsts -> ModuleConsts
merge (MC poss1 tids1 ids1 cons1) (MC poss2 tids2 ids2 cons2) = 
  MC (poss1 `union` poss2) (tids1 ++ tids2) (ids1 ++ ids2) (cons1 ++ cons2)

-- combine this declaration level with a local declaration level
withLocal :: ModuleConsts -> ModuleConsts -> ModuleConsts
withLocal (MC poss1 tids1 ids1 cons1) (MC poss2 tids2 ids2 []) =
  MC (poss1 `union` poss2) tids1 (ids1 ++ tids2 ++ ids2) cons1
withLocal _ _ = error "TraceTrans.withLocal: locally defined data constructors"

getModuleConsts :: ModuleConsts 
                -> ([Pos],[(Pos,TraceId)],[(Pos,TraceId)],[(Pos,TraceId)])
getModuleConsts (MC pos tids ids cons) = (pos,tids,ids,cons)

insert :: Eq a => a -> [a] -> [a] 
insert p ps = if p `elem` ps then ps else p:ps

-- ----------------------------------------------------------------------------
-- Transformation of declarations, expressions etc.

-- the input of this transformation has a DeclFun for each definition equation
-- the following function combines equations for a variable into one DeclFun
combineFuns :: [Decl TraceId] -> [Decl TraceId]
combineFuns (decl@(DeclFun pos id [fun]) : decls) =
  case combineFuns decls of
    (DeclFun pos2 id2 fun2s : decls') | tokenId id == tokenId id2
       -> DeclFun pos id (fun:fun2s) : decls'
    xs -> decl : xs
combineFuns (decl : decls) = decl : combineFuns decls
combineFuns [] = []


tDecls :: Bool -> Exp TokenId -> Decls TraceId -> (Decls TokenId,ModuleConsts)
tDecls traced parent (DeclsParse decls) = (DeclsParse decls',declsConsts)
  where
  (decls',declsConsts) = 
    foldr combine ([],emptyModuleConsts) . map (tDecl traced parent) 
    . combineFuns $ decls
  combine :: ([Decl id],[Decl id],ModuleConsts) -> ([Decl id],ModuleConsts)
          -> ([Decl id],ModuleConsts)
  combine (ds11,ds12,c1) (ds,c2) = (ds11++ds12++ds,c1 `merge` c2)


-- for declarations in class and instance definitions:
tDecls2 :: Bool -> Exp TokenId -> Decls TraceId 
        -> (Decls TokenId,[Decl TokenId],ModuleConsts)
tDecls2 traced parent (DeclsParse decls) = 
  (DeclsParse (concat declss1)
  ,concat declss2
  ,foldr merge emptyModuleConsts declsConstss)
  where
  (declss1,declss2,declsConstss) = 
    unzip3 (map (tDecl traced parent) . combineFuns $ decls)

tDecl2 :: Bool -> Exp TokenId -> Decl TraceId 
       -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tDecl2 traced parent (DeclFun pos id [Fun [] rhs localDecls]) = 
  -- methods always take a position and trace as argument,
  -- even if these are no used in cafs
  ([DeclFun pos id' 
     [Fun [PatWildcard pos,PatWildcard pos] 
       (Unguarded (ExpVar pos shareId)) noDecls]]
  ,DeclFun pos shareId fun' : decls'
  ,declConsts)
  where
  shareId = nameShare id
  ([DeclFun _ id' fun'],decls',declConsts) = 
    tCaf traced parent pos id rhs localDecls
tDecl2 traced parent decl = tDecl traced parent decl


singleDecl :: Decl id -> ([Decl id],[a],ModuleConsts)
singleDecl decl = ([decl],[],emptyModuleConsts)

tDecl :: Bool -> Exp TokenId -> Decl TraceId 
      -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tDecl _ _ (DeclType lhsTy rhsTy) = 
  singleDecl $ DeclType (tSimple lhsTy) (tType rhsTy)
tDecl _ _ (DeclData sort contexts lhsTy constrs derive) = 
  ([DeclData sort (tContexts contexts) (tSimple lhsTy) 
    (map tConstr constrs) (tPosClss derive)]
  ,[]
  ,foldr (uncurry addCon) emptyModuleConsts (map getCon constrs))
  where
  getCon (Constr pos id _) = (pos,id)
tDecl _ _ (DeclDataPrim pos id size) = 
  error ("Cannot trace primitive data type (" ++ show id 
    ++ " at position " ++ strPos pos ++ ")")
tDecl traced parent (DeclClass pos contexts clsId tyId decls) = 
  ([DeclClass pos (tContexts contexts) (nameTransTyConCls clsId) 
     (nameTransTyVar tyId) decls1]
  ,decls2  -- auxiliary definitions have to be outside the class definition
  ,declsConsts)
  where
  (decls1,decls2,declsConsts) = tDecls2 traced parent decls
tDecl traced parent (DeclInstance pos contexts clsId inst decls) = 
  ([DeclInstance pos (tContexts contexts) (nameTransTyConCls clsId) 
     (tType inst) decls1]
  ,decls2  -- auxiliary definitions have to be outside the instance definition
  ,declsConsts)
  where
  (decls1,decls2,declsConsts) = tDecls2 traced parent decls
tDecl _ _ (DeclDefault tys) = 
  singleDecl $ DeclDefault (map tType tys)
tDecl _ _ d@(DeclPrimitive pos fnId arity ty) =
  error "TraceTrans:tDecl _ (DeclPrimitive _ _ _ _) should not occur"
  -- A hack to be able to have primitives with untransformed types
  -- hopefully primitives are not needed any more at all
  -- if isPrefixOf "_tprim_" (reverse . unpackPS . extractV . nameOrg $ fnId)
  --   then error "tDecl _tprim_ should not occur"
  --   else singleDecl $ DeclPrimitive pos (nameOrg fnId) 2 (tFunType ty)
tDecl _ _ (DeclForeignImp pos Haskell hasName fnId arity _ ty _) =
  tHaskellPrimitive pos 
    (qualify (tail revHasModNameP) revHasUnqualName) 
    fnId arity ty
  where
  (revHasUnqualName,revHasModNameP) = span (/= '.') . reverse $ hasName 
tDecl _ _ (DeclForeignImp pos callConv cname fnId arity fspec ty duplicateId) =
  -- fnId is always let-bound, even with zero arguments
  ([DeclFun pos (nameTransVar fnId) 
     [Fun [sr,useParent]
       (Unguarded 
         (ExpApplication pos 
           [ExpVar pos (tokenPrim arity),ExpVar pos (nameTraceInfoVar pos fnId)
           ,ExpVar pos (nameForeign fnId),sr,useParent]))
       noDecls]
   ,DeclVarsType [(pos,nameTransVar fnId)] [] (tFunType ty)]
  ,[DeclForeignImp pos callConv
     (if null cname then getUnqualified fnId else cname) 
     (nameForeign fnId) arity fspec (tokenIdType ty) (nameForeign fnId)]
  ,addVar pos fnId emptyModuleConsts)
  where
  sr = ExpVar pos (nameSR fnId)
  useParent = ExpVar pos (nameTrace fnId)
tDecl _ _ (DeclForeignExp pos callConv str fnId _) =
  error ("Cannot trace foreign export (used at " ++ strPos pos ++ ")")
tDecl _ _ (DeclVarsType vars contexts ty) =
  ((if null lambdaVars then [] 
      else [DeclVarsType (tPosExps lambdaVars) 
             (tContexts contexts) (wrapType (tType ty))])
   ++
   (if null letVars then [] else [DeclVarsType (tPosExps letVars) 
                                   (tContexts contexts) (tFunType ty)])
  ,[],emptyModuleConsts)
  where
  (lambdaVars,letVars) = partition (isLambdaBound . snd) vars
tDecl traced parent (DeclPat (Alt (ExpVar pos id) rhs decls)) = 
  -- this case may occur because of the next equation
  tCaf traced parent pos id rhs decls
tDecl traced parent (DeclPat (Alt (PatAs pos id pat) rhs decls)) = 
  (dFun1++dPat1,dFun2++dPat2,funConsts `merge` patConsts)
  where
  (dFun1,dFun2,funConsts) = tCaf traced parent pos id rhs decls
  (dPat1,dPat2,patConsts) = 
    tDecl traced parent (DeclPat (Alt pat (Unguarded (ExpVar pos id)) noDecls))
tDecl traced parent (DeclPat (Alt pat rhs decls)) =
  -- unfortunately we cannot transform a pattern binding into another pattern
  -- binding; we have to introduce an explicit `case' to be able to terminate 
  -- with an appropriate error message when the pattern does not match.
  -- xi = lazySat (case patId of (t,y1,..,yn) -> indir t yi) xti
  -- patId = case e' of 
  --           p' -> (t,y1,..,yn)
  --           _  -> fail noPos parent
  -- xti = mkConstVar p pos "xi"
  (map projDef patPosIds
  ,DeclFun noPos patId 
    [Fun [] 
      (Unguarded 
        (ExpCase noPos rhs'
          [Alt pat' (Unguarded tuple) noDecls
          ,Alt (PatWildcard noPos) (Unguarded (mkFailExp noPos parent)) noDecls
          ]))
      decls']
   :map (uncurry (mkConstDecl parent)) patPosIds
  ,foldr (uncurry addVar) 
    (emptyModuleConsts `withLocal` (declsConsts `merge` rhsConsts)) patPosIds)
  where
  firstId = snd . head $ patPosIds
  patId = nameShare firstId
  resultTraceId = nameTrace2 firstId
  tuple = mkTupleExp noPos (ExpVar noPos resultTraceId : map tPat patVars)
  patPosIds = map (\(ExpVar pos id) -> (pos,id)) patVars
  patVars = getPatVars pat
  pat' = case tPat pat of
           ExpApplication p [r,v,_] -> 
             ExpApplication p [r,v,ExpVar noPos resultTraceId]
  (rhs',rhsConsts) = tRhs traced False parent failContinuation rhs
  (decls',declsConsts) = tDecls traced parent decls

  mkConstDecl :: Exp TokenId -> Pos -> TraceId -> Decl TokenId
  mkConstDecl parent pos id =
    DeclFun pos (nameTrace id)
      [Fun [] (Unguarded (mkConstVar parent pos id traced)) noDecls]

  projDef :: (Pos,TraceId) -> Decl TokenId
  projDef (pos,id) =
    DeclFun pos (nameTransVar id) 
      [Fun []
        (Unguarded (ExpApplication pos 
          [combSat pos traced False
          ,ExpCase pos (ExpVar pos patId)
            [Alt tuple
              (Unguarded 
                (ExpApplication pos 
                  [ExpVar pos tokenIndir
                  ,ExpVar pos resultTraceId,ExpVar pos (nameTransVar id)])) 
              noDecls]
          ,ExpVar pos (nameTrace id)]))
        noDecls]

  getPatVars :: Pat id -> [Pat id]
  getPatVars (ExpRecord pat fields) =
    getPatVars pat ++ concatMap getFieldVars fields
    where
    getFieldVars (FieldExp _ _ pat) = getPatVars pat
  getPatVars (ExpApplication _ pats) = concatMap getPatVars pats
  getPatVars pat@(ExpVar pos id) = [pat]
  getPatVars (ExpCon _ _) = []
  getPatVars (ExpLit _ _) = []
  getPatVars (ExpList _ pats) = concatMap getPatVars pats
  getPatVars (PatAs pos id pat) = ExpVar pos id : getPatVars pat
  getPatVars (PatWildcard _) = []
  getPatVars (PatIrrefutable _ pat) = getPatVars pat
tDecl traced parent (DeclFun pos id [Fun [] rhs localDecls]) = 
  tCaf traced parent pos id rhs localDecls
    -- a caf has many dynamic parents and hence uses the static parent
tDecl _ parent (DeclFun pos id (Fun [] _ _ : _)) =
  error "tDecl: variable multiple defined"
tDecl traced parent (DeclFun pos id funs) = 
  tFun traced pos id funs  -- a function does not use the static parent
tDecl _ _ (DeclFixity _) = 
  ([],[],emptyModuleConsts) 
  -- fixity declarations have been processed before 
  -- not needed in output, because pretty printer produces unambiguous output
tDecl _ _ _ = error "tDecl: unknown sort of declaration"


-- constructor definition in type definition
tConstr :: Constr TraceId -> Constr TokenId
tConstr (Constr pos conId tyArgs) =
  Constr pos (nameTransCon conId) (tTyArgs tyArgs)
tConstr (ConstrCtx tyVars contexts pos conId tyArgs) =
  ConstrCtx (tPosTyVars tyVars) (tContexts contexts) 
    pos (nameTransCon conId) (tTyArgs tyArgs)


tHaskellPrimitive :: Pos -> TokenId -> TraceId -> Arity -> Type TraceId 
                  -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tHaskellPrimitive pos hasId fnId arity ty 
  -- import of a Haskell function
  -- used for defining builtin Haskell functions
  -- transformation yields a wrapper to the untransformed function
  | arity == 0 =
    ([DeclVarsType [(pos,nameTransVar fnId)] [] (wrapType (tType ty))
     ,DeclFun pos (nameTransVar fnId)
       [Fun []
         (Unguarded 
           (ExpApplication pos 
             [combSat pos False False
             ,ExpApplication pos 
               [expFrom pos ty, ExpVar pos tokenHiddenRoot, ExpVar pos hasId]
	     ,useParent])) 
	 noDecls]]
    ,[DeclFun pos useParentId
       [Fun [] (Unguarded 
         (mkConstVar (ExpVar pos tokenMkTRoot) pos fnId False)) noDecls]]
    ,addVar pos fnId emptyModuleConsts)
  | otherwise =
    ([DeclVarsType [(pos,nameTransVar fnId)] [] (tFunType ty)
     ,DeclFun pos (nameTransVar fnId) 
       [Fun [sr,useParent]
         (Unguarded 
           (ExpApplication pos 
             [combFun pos False arity
             ,ExpVar pos (nameTraceInfoVar pos fnId)
             ,ExpVar pos wrappedId',sr,useParent]))
          noDecls]]
    ,[DeclFun pos wrappedId' 
       [Fun (hidden:args)
         (Unguarded (ExpApplication pos
           [expFrom pos tyRes, hidden,
             ExpApplication pos (ExpVar pos hasId : zipWith to tyArgs args)]))
         noDecls]]
    ,addVar pos fnId emptyModuleConsts)
    where
    sr = ExpVar pos (nameSR fnId)
    useParent = ExpVar pos useParentId
    useParentId = nameTrace fnId
    hidden = ExpVar pos (nameTrace2 fnId)
    args = take arity . map (ExpVar pos) . nameArgs $ fnId
    wrappedId' = nameWorker fnId
    to :: Type TraceId -> Exp TokenId -> Exp TokenId
    to ty arg = ExpApplication pos [expTo pos ty, hidden, arg]
    -- assert: length (tyArgs) = arity
    (tyArgs,tyRes) = decomposeFunType ty
    decomposeFunType :: Type TraceId -> ([Type TraceId],Type TraceId)
    decomposeFunType (TypeCons _ tyCon [ty1,ty2]) | isFunTyCon tyCon =
      (ty1:args,res) 
      where
      (args,res) = decomposeFunType ty2
    decomposeFunType ty = ([],ty)


tCaf :: Bool -> Exp TokenId -> Pos -> TraceId -> Rhs TraceId -> Decls TraceId
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tCaf traced parent pos id rhs localDecls =
  ([DeclFun pos (nameTransVar id)
     [Fun []
       (Unguarded 
         (ExpApplication pos [combSat pos traced False,rhs',useParent]))
       localDecls']
   ]
   -- id = lazySat rhs' traceId
   --   where
   --   localDecls'
  ,[DeclFun pos useParentId
     [Fun [] (Unguarded (mkConstVar parent pos id traced)) noDecls]
   ]
   -- traceId = constId parent pos id
  ,addVar pos id emptyModuleConsts `withLocal` 
    (rhsConsts `merge` localDeclsConsts))
  where
  useParent = ExpVar pos useParentId
  useParentId = nameTrace id
  (rhs',rhsConsts) = tRhs traced True useParent failContinuation rhs
  (localDecls',localDeclsConsts) = tDecls traced useParent localDecls


tFun :: Bool -> Pos -> TraceId -> [Fun TraceId]
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tFun traced pos id funs =
  ([DeclFun pos (nameTransVar id) 
     [Fun [sr,parent]
       (Unguarded
         (ExpApplication pos
           [combFun pos traced funArity
           ,ExpVar pos (nameTraceInfoVar pos id)
           ,ExpVar pos wrappedId',sr,parent]))
       noDecls]]
  ,DeclFun pos wrappedId' funs' : newDecls'  
   -- in same scope as possible type decl (which hasn't been implemented)
  ,addVar pos id (emptyModuleConsts `withLocal` funConsts))
  where
  funArity = case funs of (Fun pats _ _ : _) -> length pats
  sr = ExpVar pos (nameSR id)
  parent = ExpVar pos (nameTrace id)
  wrappedId' = nameWorker id
  (funs',newDecls',funConsts) = 
    tFunClauses traced pos (ExpVar pos (nameTrace id)) (nameFuns id) 
      (map (ExpVar pos) (nameArgs id)) funArity False funs
         

tFunClauses :: Bool
            -> Pos 
            -> Exp TokenId -- variable that can be bound to parent
            -> [TokenId]   -- ids for definitions that clauses are turned into
            -> [Exp TokenId] -- vars for naming arguments that are not vars
            -> Arity
            -> Bool -- preceeding fun-clause will never fail
            -> [Fun TraceId] 
            -> ([Fun TokenId],[Decl TokenId],ModuleConsts)

tFunClauses _ _ _ _ _ _ True [] = ([],[],emptyModuleConsts)
tFunClauses _ pos parent ids pVars funArity False [] =
  ([Fun 
     (parent : replicate funArity (PatWildcard pos))
     (Unguarded (continuationToExp parent failContinuation)) noDecls]
  ,[],emptyModuleConsts)
tFunClauses traced pos parent ids pVars funArity _
 (Fun pats (Unguarded exp) decls : funs) =
  (Fun (parent : pats') (Unguarded exp') decls' : funs'
  ,funDecls
  ,declsConsts `merge` funConsts `withLocal` expConsts)
  where
  pats' = tPats pats
  (exp',expConsts) = tExp traced True parent exp
  (decls',declsConsts) = tDecls traced parent decls
  (funs',funDecls,funConsts) = tFunClauses traced pos parent ids pVars 
                                 funArity (neverFailingPats pats) funs
tFunClauses traced pos parent ids pVars funArity _
 (Fun pats (Guarded gdExps) decls : funs)
  | not (null funs) && canFail gdExps =
    ([Fun (parent : pats') (Unguarded gdExps') decls'
     ,Fun (parent : replicate funArity (PatWildcard pos)) 
       (Unguarded (continuationToExp parent failCont)) noDecls]
    ,DeclFun pos contId funs' : funDecls
    ,declsConsts `merge` funConsts `withLocal` gdExpsConsts)
  where
  contId = head ids
  failCont = functionContinuation contId vars
  (pats',vars) = namePats (tPats pats) pVars 
  (gdExps',gdExpsConsts) = tGuardedExps traced True parent failCont gdExps
  (decls',declsConsts) = tDecls traced parent decls
  (funs',funDecls,funConsts) = tFunClauses traced pos parent (tail ids) pVars 
                                 funArity (neverFailingPats pats) funs
tFunClauses traced pos parent ids pVars funArity _
 (Fun pats (Guarded gdExps) decls : funs) =
  -- last clause or guards cannot fail
  (Fun (parent:pats') (Unguarded gdExps') decls' : funs'
  ,funDecls
  ,declsConsts `merge` funConsts `withLocal` gdExpsConsts)
  where
  pats' = tPats pats
  (gdExps',gdExpsConsts) = tGuardedExps traced True parent failContinuation 
                             gdExps
  (decls',declsConsts) = tDecls traced parent decls
  (funs',funDecls,funConsts) = tFunClauses traced pos parent ids pVars 
                                 funArity (neverFailingPats pats) funs


-- Returns False only if one of the guards definitely has value True.
canFail :: [(Exp TraceId,Exp TraceId)] -> Bool
canFail [] = True
canFail ((ExpCon _ cid, _) : gdExps) = not (isTrue cid) && canFail gdExps
canFail ((ExpVar _ cid, _) : gdExps) = not (isOtherwise cid) && canFail gdExps
canFail (_ : gdExps) = canFail gdExps


namePats :: [Pat TokenId] -> [Pat TokenId] -> ([Pat TokenId],[Pat TokenId])
namePats pats vars = unzip (zipWith namePat pats vars)

-- Obtain a variable that names the given pattern;
-- straightforward if pattern has variable at top-level;
-- otherwise use provided variable
namePat :: Pat TokenId  -- pattern to name
        -> Pat TokenId  -- default variable
        -> (Pat TokenId,Pat TokenId)  -- named pattern, name variable
namePat pat@(ExpVar _ _) _ = (pat,pat)
namePat pat@(PatAs pos id pat') _ = (pat,ExpVar pos id)
namePat pat var@(ExpVar pos id) = (PatAs pos id pat,var)


tRhs :: Bool         -- traced?
     -> Bool         -- equal to parent? 
     -> Exp TokenId  -- parent
     -> ContExp      -- continuation in case of pattern match failure
     -> Rhs TraceId  
     -> (Exp TokenId,ModuleConsts)

tRhs traced cr parent failCont (Unguarded exp) = tExp traced cr parent exp
tRhs traced cr parent failCont (Guarded gdExps) =
  tGuardedExps traced cr parent failCont gdExps


tGuardedExps :: Bool         -- traced?
             -> Bool         -- equal to parent? 
             -> Exp TokenId  -- parent
             -> ContExp      -- continuation in case of pattern match failure
             -> [(Exp TraceId,Exp TraceId)]  
             -> (Exp TokenId,ModuleConsts)
tGuardedExps _ cr parent failCont [] = 
  (continuationToExp parent failCont,emptyModuleConsts)
tGuardedExps traced cr parent failCont ((guard,exp):gdExps) =
  (ExpCase pos guard'
    [Alt (wrapExp pos guardValue guardTrace)
      (Unguarded
        (ExpLet pos
          (DeclsParse
            [DeclFun pos newParentId
              [Fun []
                (Unguarded (mkConstGuard pos parent guardTrace traced)) 
                noDecls]])
          (ExpIf pos guardValue exp' gdExps')))
-- see if seq is necessary at all for traced version
-- definitely should not be used for untraced version
--          (ExpApplication pos 
--            [ExpVar pos tseq,newParent
--            ,ExpIf pos guardValue exp' gdExps'])))
      noDecls]
  -- case guard' of
  --   R gv gt -> let t = mkConstGuard pos parent gt
  --              in t `seq` (if gv then exp' else gdExps') 
  ,pos `addPos` guardConsts `merge` expConsts `merge` gdExpsConsts)
  where
  pos = getPos guard
  guardValue = ExpVar pos guardValueId
  guardTrace = ExpVar pos guardTraceId
  newParent = ExpVar pos newParentId
  (newParentId:guardValueId:guardTraceId:_) = namesFromPos pos
  (guard',guardConsts) = tExp traced False parent guard
  (exp',expConsts) = tExp traced cr newParent exp
  (gdExps',gdExpsConsts) = tGuardedExps traced cr newParent failCont gdExps


-- -----------------------------------------
-- Abstract continuation for guards
--
-- To correctly create the trace within guards, a continuation is used.
-- The type ContExp should be abstract. Its implementation is only used in 
-- the following three functions.

data ContExp = Fail | Function TokenId [Exp TokenId]

failContinuation :: ContExp
failContinuation = Fail

functionContinuation :: TokenId -> [Exp TokenId] -> ContExp
functionContinuation = Function

continuationToExp :: Exp TokenId   -- trace
                  -> ContExp 
                  -> Exp TokenId
continuationToExp parent Fail = mkFailExp noPos parent
continuationToExp parent (Function fun args) =
  ExpApplication noPos (ExpVar noPos fun : parent : args)


-- Transform expressions

tExps :: Bool           -- traced
      -> Exp TokenId    -- parent
      -> [Exp TraceId]  -- expressions
      -> ([Exp TokenId],ModuleConsts)
tExps traced parent = 
  foldr 
    (\e (es',cs) -> let (e',c) = tExp traced False parent e 
                    in (e':es',c `merge` cs))
    ([],emptyModuleConsts)

-- Second argument True iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
tExp :: Bool -> Bool -> Exp TokenId -> Exp TraceId 
     -> (Exp TokenId,ModuleConsts)
tExp traced cr parent (ExpLambda pos pats body) =
  (ExpApplication pos 
    [combFun pos traced funArity
    ,ExpVar pos tokenMkAtomLambda
    ,if neverFailingPats pats 
       then ExpLambda pos (lambdaParent : pats') body'
       else ExpLambda pos (lambdaParent : vars)
              (ExpCase pos (mkTupleExp pos vars)
                [Alt (mkTupleExp pos pats') (Unguarded body') noDecls
                ,Alt (PatWildcard pos) 
                   (Unguarded (mkFailExp pos lambdaParent)) noDecls])
    ,mkSRExp pos traced
    ,parent]
  ,pos `addPos` bodyConsts)
  where
  pats' = tPats pats
  (body',bodyConsts) = tExp traced True lambdaParent body
  vars = map (ExpVar pos) . take funArity $ varsIds
  lambdaParent = ExpVar pos lambdaParentId
  (lambdaParentId:varsIds) = namesFromPos pos
  funArity = length pats
tExp traced cr parent (ExpLet pos decls body) =
  (ExpLet pos decls' body'
  ,declConsts `withLocal` bodyConsts)
  where
  (decls',declConsts) = tDecls traced parent decls
  (body',bodyConsts) = tExp traced cr parent body
tExp traced cr parent (ExpDo pos stmts) =
  tExp traced cr parent (removeDo stmts)
tExp traced cr parent (ExpCase pos e alts) =
  (ExpApplication pos
    [combApply pos traced cr 1
    ,mkSRExp pos traced
    ,parent
    ,ExpApplication pos
      [combFun pos traced 1
      ,ExpVar pos tokenMkAtomCase
      ,ExpLet pos (DeclsParse (DeclFun pos varId fun' : defs'))
        (ExpVar pos varId)
      ,mkSRExp pos traced
      ,parent]
    ,e']
  -- ap1 sr parent 
  --   (fun1 mkAtomCase (let varId = fun'; defs' in varId) sr parent)
  --   e'
  ,pos `addPos` funConsts)
  where
  (varId:caseParentId:argId:funsIds) = namesFromPos pos
  (e',eConsts) = tExp traced False parent e
  (fun',defs',funConsts) = 
    tFunClauses traced pos (ExpVar pos caseParentId) funsIds 
      [ExpVar pos argId] 1 False
      . map alt2Fun $ alts
tExp traced cr parent (ExpIf pos cond e1 e2) =
  -- case [[g]]^False_parent of
  --   R gr gt -> 
  --     let ifParent = mkTAp2 parent (mkTConst parent atomIf sr) gt parent sr
  --     in makeSat 
  --          (if gr 
  --             then [[e1]]^True_ifParent 
  --             else [[e2]]^True_ifParent)
  --          ifParent
  (ExpCase pos cond'
    [Alt (wrapExp pos condV condT)
      (Unguarded
        (ExpLet pos
          (DeclsParse
            [DeclFun pos ifParentId
              [Fun []
                (Unguarded
                  (ExpApplication pos
                    [ExpVar pos (tokenMkTAp 2)
                    ,parent
                    ,mkConst parent (ExpVar pos tokenMkAtomIf) pos traced
                    ,condT,parent,mkSRExp pos traced]))
                noDecls]])
          (ExpApplication pos 
            [combSat pos traced cr,ExpIf pos condV e1' e2',ifParent])))
      noDecls]
  ,pos `addPos` condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond',condConsts) = tExp traced False parent cond
  (e1',e1Consts) = tExp traced True ifParent e1
  (e2',e2Consts) = tExp traced True ifParent e2
  (condV:condT:_) = map (ExpVar pos) newIds
  ifParent = ExpVar pos ifParentId 
  (ifParentId:newIds) = namesFromPos pos
tExp traced cr parent (ExpType pos e contexts ty) =
  (ExpType pos e' (tContexts contexts) (tType ty)
  ,eConsts)
  where
  (e',eConsts) = tExp traced cr parent e
tExp traced cr parent (ExpRecord e fields) =
  error "Cannot yet trace record expressions"
tExp traced cr parent (ExpApplication pos (f@(ExpCon _ _) : es))=
  tConApp traced parent f es
tExp traced cr parent (ExpApplication pos es) =
  (ExpApplication pos 
    (combApply pos traced cr (length es - 1):mkSRExp pos traced:parent:es')
  ,pos `addPos` esConsts)
  where
  (es',esConsts) = tExps traced parent es
tExp traced cr parent (ExpVar pos id) =
  if isLambdaBound id  
    then 
      if cr 
      then (ExpApplication pos [ExpVar pos tokenIndir,parent,e']
           ,emptyModuleConsts) 
      else (e',emptyModuleConsts)
    else (ExpApplication pos [e',mkSRExp pos traced,parent]
         ,pos `addPos` emptyModuleConsts)
  where
  e' = ExpVar pos (nameTransVar id) 
tExp traced cr parent e@(ExpCon pos id) =
  tSatConApp traced parent e []
tExp traced cr parent (ExpLit pos (LitString _ s)) =
  -- the result is very large; should use special wrapper that
  -- transforms string in traced string instead
  tExp traced cr parent (ExpList pos (map (ExpLit pos . LitChar Boxed) s))
tExp traced cr parent (ExpLit pos lit) =
  (ExpApplication pos [tLit lit,mkSRExp pos traced,parent,ExpLit pos lit]
  ,pos `addPos` emptyModuleConsts)
  where
  tLit (LitInt _ _) = ExpVar pos tokenConInt
  tLit (LitChar _ _) = ExpVar pos tokenConChar
  tLit (LitInteger _ _) = ExpVar pos tokenFromConInteger  
                          -- removed after typechecking
  tLit (LitRational _ _) = ExpVar pos tokenFromConRational
                           -- removed after typechecking
  tLit (LitDouble _ _) = ExpVar pos tokenConDouble
  tLit (LitFloat _ _) = ExpVar pos tokenConFloat
  -- do Int, Double or Float literals actually exists at this compiler stage?
tExp traced cr parent (ExpList pos es) =
  -- the result is very large; should use special wrapper that
  -- transforms list in traced list instead
  tExp traced cr parent . mkTList pos $ es
tExp _ _ _ _ = error "tExp: unknown sort of expression"

-- return False if matching the pattern may fail
-- otherwise try to return True
-- (safe approximation)
neverFailingPat :: Pat id -> Bool
neverFailingPat (ExpVar _ _) = True
neverFailingPat (PatAs _ _ pat) = neverFailingPat pat
neverFailingPat (PatIrrefutable _ _) = True
neverFailingPat (PatWildcard _ ) = True
neverFailingPat _ = False

neverFailingPats :: [Pat id] -> Bool
neverFailingPats = all neverFailingPat

-- conversion of case alternative into function definition alternative 
alt2Fun :: Alt a -> Fun a
alt2Fun (Alt pat rhs decls) = Fun [pat] rhs decls 


-- Transform data constructor application.
-- Number of arguments may be smaller than arity of the data constructor.
tConApp :: Bool          -- traced?
        -> Exp TokenId   -- parent
        -> Exp TraceId   -- data constructor
        -> [Exp TraceId] -- arguments
        -> (Exp TokenId,ModuleConsts)  

tConApp traced parent c@(ExpCon pos id) args 
  | conArity > numberOfArgs = -- undersaturated application
    (ExpApplication pos 
      (ExpVar pos (tokenPa numberOfArgs)
      :ExpCon pos (nameTransCon id)
      :ExpVar pos (tokenCn (conArity-numberOfArgs))
      :mkSRExp pos traced
      :parent
      :ExpVar pos (nameTraceInfoCon id)
      :args')
    ,pos `addPos` argsConsts)
  | otherwise = tSatConApp traced parent c args
  where
  Just conArity = arity id  -- a constructor always has an arity
  numberOfArgs = length args
  (args',argsConsts) = tExps traced parent args


-- Transform data constructor application with number of args equal arity.
tSatConApp :: Bool          -- traced?
           -> Exp TokenId   -- parent
           -> Exp TraceId   -- data constructor
           -> [Exp TraceId] -- arguments 
           -> (Exp TokenId,ModuleConsts)
tSatConApp traced parent (ExpCon pos id) args =
  (ExpApplication pos 
    (ExpVar pos (tokenCon (length args))
    :mkSRExp pos traced
    :parent
    :ExpCon pos (nameTransCon id)
    :ExpVar pos (nameTraceInfoCon id)
    :args')
  ,pos `addPos` argsConsts)
  where
  (args',argsConsts) = tExps traced parent args

-- Desugar do-statements 
removeDo :: [Stmt TraceId] -> Exp TraceId
removeDo [StmtExp e] = e
removeDo (StmtExp e : stmts) =
  ExpApplication pos [ExpVar pos tTokenGtGt,e,removeDo stmts] 
  where
  pos = getPos e
removeDo (StmtLet decls : stmts) =
  ExpLet pos decls (removeDo stmts)
  where
  pos = getPos decls
removeDo (StmtBind pat e : stmts) =
  ExpApplication pos 
    [ExpVar pos tTokenGtGtEq,e, 
    if neverFailingPat pat 
      then ExpLambda pos [pat] (removeDo stmts)
      else ExpLambda pos [newVar] 
             (ExpCase pos newVar 
               [Alt pat (Unguarded (removeDo stmts)) noDecls
               ,Alt (PatWildcard pos) 
                 (Unguarded 
                   (ExpApplication pos 
                     [ExpVar pos tTokenFail
                     ,ExpLit pos 
                       (LitString Boxed 
                         "pattern-match failure in do expression")]))
                 noDecls])
    ]
  where
  newVar = ExpVar pos (just newId)
  newId:_ = namesFromPos pos
  pos = getPos e


tPats :: [Pat TraceId] -> [Pat TokenId]
tPats = map tPat

tPat (ExpRecord pat fields) = ExpRecord (tPat pat) (map tField fields)
  where
  tField (FieldExp pos id pat) = FieldExp pos (nameTransField id) (tPat pat)
tPat (ExpApplication pos (ExpCon pos2 id : pats)) = 
  wrapExp pos (ExpApplication pos (ExpCon pos2 (nameTransCon id) : tPats pats))
    (PatWildcard pos)
tPat (ExpVar pos id) = ExpVar pos (nameTransVar id)
tPat (ExpCon pos id) = 
  wrapExp pos (ExpCon pos (nameTransCon id)) (PatWildcard pos)
tPat (ExpLit pos (LitString _ s)) =
  tPat . mkTList pos . map (ExpLit pos . LitChar Boxed) $ s
tPat (ExpLit pos lit) = 
  wrapExp pos (ExpLit pos lit) (PatWildcard pos)  -- type change
tPat (ExpList pos pats) = tPat . mkTList pos $ pats
tPat (PatAs pos id pat) = PatAs pos (nameTransVar id) (tPat pat)
tPat (PatWildcard pos) = PatWildcard pos  -- type change
tPat (PatIrrefutable pos pat) = 
  case tPat pat of
    ExpApplication pos' [r,p',t'] -> 
      ExpApplication pos' [r,PatIrrefutable pos p',t']
    x -> x
tPat (PatNplusK pos id id' k comp neg) = error "Cannot trace n+k patterns"
tPat _ = error "tPat: unknown pattern"

-- convert a list of expressions into a list expression (with TraceIds)
mkTList :: Pos -> [Exp TraceId] -> Exp TraceId
mkTList pos = 
  foldr (\x xs -> ExpApplication pos [cons,x,xs]) (ExpCon pos tTokenNil)
  where
  cons = ExpCon pos tTokenCons


-- ----------------------------------------------------------------------------
-- Transform types

tTyArgs :: [(Maybe [(Pos,TraceId)],Type TraceId)] 
        -> [(Maybe [(Pos,TokenId)],Type TokenId)]
tTyArgs = map tTyArg

tTyArg :: (Maybe [(Pos,TraceId)],Type TraceId)
       -> (Maybe [(Pos,TokenId)],Type TokenId)
tTyArg (maybePosIds,ty) = 
  (fmap (mapSnd nameTransField) maybePosIds,wrapType (tType ty))


-- ty ==> SR -> Trace -> [[ty]]
tFunType :: Type TraceId -> Type TokenId
tFunType ty = 
  TypeCons pos tokenSR [] `typeFun` TypeCons pos tokenTrace [] 
    `typeFun` wrapType (tType ty)
  where
  pos = getPos ty

-- just rewrite function types:
-- t1 -> t2  ==>  Trace -> R t1 -> R t2
tType :: Type TraceId -> Type TokenId
tType (TypeCons pos tyCon tys) =
--  if isFunTyCon tyCon 
--    then TypeCons pos tokenTrace [] 
--           `typeFun` TypeCons pos t_Arrow (map (wrapType . tType) tys)
--    else 
  TypeCons pos (nameTransTyConCls tyCon) (map tType tys)
tType (TypeApp lTy rTy) = TypeApp (tType lTy) (tType rTy)
tType (TypeVar pos tyId) = TypeVar pos (nameTransTyVar tyId)
tType (TypeStrict pos ty) = TypeStrict pos (tType ty)

-- ty ==> R ty  (!ty ==> ! (R ty))
wrapType :: Type TokenId -> Type TokenId
wrapType (TypeStrict pos ty) = TypeStrict pos (wrapType ty)
wrapType ty = TypeCons noPos tokenR [ty]

-- just replace TraceIds by TokenIds
tokenIdType :: Type TraceId -> Type TokenId
tokenIdType (TypeCons pos tyCon tys) =
  TypeCons pos (nameTransTyConCls tyCon) (map tokenIdType tys)
tokenIdType (TypeApp lTy rTy) = TypeApp (tokenIdType lTy) (tokenIdType rTy)
tokenIdType (TypeVar pos tyId) = TypeVar pos (nameTransTyVar tyId)
tokenIdType (TypeStrict pos ty) = TypeStrict pos (tokenIdType ty) 

-- function type constructor
-- infixr 6 `typeFun`
typeFun :: Type TokenId -> Type TokenId -> Type TokenId
typeFun ty1 ty2 = TypeCons noPos t_Arrow [ty1,ty2]


tContexts :: [Context TraceId] -> [Context TokenId]
tContexts = map tContext

tContext :: Context TraceId -> Context TokenId
tContext (Context pos clsId (pos',tyVarId)) =
  Context pos (nameTransTyConCls clsId) (pos',nameTransTyVar tyVarId)

tSimple :: Simple TraceId -> Simple TokenId 
tSimple (Simple pos tycon posArgs) =
  Simple pos (nameTransTyConCls tycon) (tPosTyVars posArgs)

tPosExps :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosExps = mapSnd nameTransVar

tPosClss :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosClss = mapSnd nameTransTyConCls

tPosTyVars :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosTyVars = mapSnd nameTransTyVar


-- ----------------------------------------------------------------------------
-- New names
-- Module names and hence all qualifications are prefixed.
-- Names of classes, type constructors and type variables remain unchanged.
-- Names of data constructors remain unchanged.
-- (everything but expression variables)

-- names referring to traces (or parts thereof) of program fragments:

nameTraceInfoModule :: TraceId -> TokenId
nameTraceInfoModule = prefixName 't' (error "nameTraceInfoModule: operator")

nameTraceInfoVar :: Pos -> TraceId -> TokenId
nameTraceInfoVar = prefixPosName 'a' '+'

nameTraceInfoCon :: TraceId -> TokenId
nameTraceInfoCon = prefixName 'a' '+'

nameTraceInfoPos :: Pos -> TokenId
nameTraceInfoPos pos = mkUnqualifiedTokenId (('p':) . showsEncodePos pos $ "")

-- names referring to transformed program fragments:

nameTransModule :: TraceId -> TokenId
nameTransModule = updateToken updateModule
  where
  updateModule ('T':'r':'a':'c':'e':'O':'r':'i':'g':orgName) = orgName
  updateModule (name@"Main") = name  -- if the module is `Main', then unchanged
  updateModule name = modulePrefix : name

nameTransTyConCls  :: TraceId -> TokenId
nameTransTyConCls = updateToken id  -- only module name is changed

nameTransTyVar :: TraceId -> TokenId
nameTransTyVar = updateToken id  -- only module name is changed

nameTransCon :: TraceId -> TokenId
nameTransCon = updateToken id  -- only module name is changed

nameTransField :: TraceId -> TokenId
nameTransField = prefixName 'e' '^'

nameTransVar :: TraceId -> TokenId
nameTransVar = prefixName 'o' '!'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: TraceId -> TokenId
nameWorker = prefixName 'w' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: TraceId -> TokenId
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: TraceId -> TokenId
nameShare = prefixName 's' '|'

-- variable for a trace
nameTrace :: TraceId -> TokenId
nameTrace = prefixName 't' '$'

-- second variable for a trace
nameTrace2 :: TraceId -> TokenId
nameTrace2 = prefixName 'u' '<'

-- name for a local variable for a source reference
nameSR :: TraceId -> TokenId
nameSR = prefixName 'p' '='

-- infinite list of var ids made from one id (for function clauses)
nameFuns :: TraceId -> [TokenId]
nameFuns = prefixNames 'f' '>'

-- infinite list of var ids made from one id (for naming arguments)
nameArgs :: TraceId -> [TokenId]
nameArgs = prefixNames 'g' '/'

-- infinite list of ids made from a position
namesFromPos :: Pos -> [TokenId]
namesFromPos pos =
  map (mkUnqualifiedTokenId . ('v':) . showsEncodePos pos . ('v':) . show) 
    [1..]

-- Generation of new variables

showsEncodePos :: Pos -> ShowS
showsEncodePos pos = shows line . ('v':) . shows column 
  where
  (line,column) = fromPos pos

showsSymEncodePos :: Pos -> ShowS
showsSymEncodePos pos = 
  \xs -> numToSym (show line) ++ '=' : numToSym (show column) ++ xs 
  where
  (line,column) = fromPos pos

prefixName :: Char -> Char -> TraceId -> TokenId
prefixName c d = updateToken update
  where
  update name = if isOperatorName name then d:name else c:name

prefixPosName :: Char -> Char -> Pos -> TraceId -> TokenId
prefixPosName c d pos = updateToken update
  where
  update name = if isOperatorName name 
                  then (d:) . showsSymEncodePos pos $ name 
                  else (c:) . showsEncodePos pos $ name

prefixNames :: Char -> Char -> TraceId -> [TokenId]
prefixNames c d token = map (($ token) . updateToken . update) [1..]
  where
  update no name = if isOperatorName name
                    then (d:) . (++ name) . numToSym . show $ no
                    else (c:) . (++ name) . show $ no

isOperatorName :: String -> Bool
isOperatorName = not . isAlpha . head

numToSym :: String -> String
numToSym = map (("!#$%&*+/<>" !!) . digitToInt)

-- Tokens

modulePrefix = 'T'

mkUnqualifiedTokenId :: String -> TokenId
mkUnqualifiedTokenId = visible . reverse

getUnqualified :: TraceId -> String
getUnqualified = reverse . unpackPS . extractV . tokenId

-- apply function to unqualified name part 
-- and prefix module name (if qualified)
updateToken :: (String -> String) -> TraceId -> TokenId
updateToken f traceId | isFunTyCon traceId = qualify "T" "nuF"
updateToken f traceId = 
  case tokenId (traceId) of
    t@(TupleId n) -> 
      Qualified 
        (if n <= 2 then tracingModuleShort else transPreludeModule) 
        (packString . reverse . f $ ("Tuple"++show n)) 
    Visible n     -> 
      Visible (packString . reverse . f . unqual $ n) 
    Qualified m n -> 
      Qualified 
        (packString . reverse . updateModule . reverse . unpackPS $ m)
        (packString . reverse . f . unqual $ n) 
    _             -> error "TraceTrans: updateToken"
  where
  transPreludeModule = packString . reverse . updateModule $ "Prelude"
  updateModule (name@"Main") = name -- if module is `Main', then unchanged
  updateModule name = modulePrefix : name
  unqual :: PackedString -> String
  unqual n = case reverse . unpackPS $ n of -- change predefined names
               -- "->" -> "Fun"
               ":" -> "Cons" 
               "[]" -> "List" -- here both type and data constructor
               s -> s


-- ----------------------------------------------------------------------------
-- hardwired Haskell combinators and other names used by transformed modules

mkConst :: Exp TokenId -> Exp TokenId -> Pos -> Bool -> Exp TokenId
mkConst parent atom pos traced = 
  ExpApplication pos 
    [ExpVar pos tokenMkConst,parent,atom,mkSRExp pos traced]

-- only used where definition and use position coincide 
-- (cafs, pattern bindings)
mkConstVar :: Exp TokenId -> Pos -> TraceId -> Bool -> Exp TokenId
mkConstVar parent pos id traced = 
  mkConst parent (ExpVar pos (nameTraceInfoVar pos id)) pos traced

mkSRExp :: Pos -> Bool -> Exp TokenId
mkSRExp pos traced = 
  ExpVar pos (if traced then nameTraceInfoPos pos else tokenMkNoPos)

combSat :: Pos -> Bool -> Bool -> Exp TokenId
combSat pos traced cr = 
  ExpVar pos (if traced then tokenLazySat else tokenULazySat)
  -- (if cr then tokenEagerSat else tokenLazySat)

combApply :: Pos -> Bool -> Bool -> Arity -> Exp TokenId
combApply pos traced cr a = 
  ExpVar pos ((if traced then tokenAp else tokenUAp) a)
  -- ((if cr then tokenRap else tokenAp) a)

combFun :: Pos -> Bool -> Arity -> Exp TokenId
combFun pos traced a = ExpVar pos ((if traced then tokenFun else tokenUFun) a)

mkConstGuard :: Pos -> Exp TokenId -> Exp TokenId -> Bool -> Exp TokenId
mkConstGuard pos parent guardTrace traced =
  ExpApplication pos
    [ExpVar pos (tokenMkTAp 2),parent
    ,mkConst parent (ExpVar pos tokenMkAtomGuard) pos traced
    ,guardTrace,parent,mkSRExp pos traced]

-- apply data constructor R
wrapExp :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
wrapExp pos ev et = ExpApplication pos [ExpCon pos tokenR,ev,et]

-- hardwired tokens:

tracingModule :: PackedString
tracingModule = packString . reverse $ "Hat" -- name of module with combinators

tracingModuleShort :: PackedString
tracingModuleShort = packString . reverse $ "T"  -- abbreviation


mkTracingToken :: String -> TokenId
mkTracingToken s = Qualified tracingModuleShort (packString . reverse $ s)

mkTracingTokenArity :: String -> Arity -> TokenId
mkTracingTokenArity s a = mkTracingToken (s ++ show a)

typeModule :: PackedString
typeModule = packString . reverse $ "TPreludeBuiltinTypes" 

mkTypeToken :: String -> TokenId
mkTypeToken s = Qualified typeModule (packString . reverse $ s)


-- tokens for trace constructors:

tokenMkTRoot :: TokenId
tokenMkTRoot = mkTracingToken "mkTRoot"

tokenHiddenRoot :: TokenId
tokenHiddenRoot = mkTracingToken "hiddenRoot"

tokenR :: TokenId
tokenR = mkTracingToken "R"

tokenMkTAp :: Arity -> TokenId
tokenMkTAp = mkTracingTokenArity "mkTAp"

tokenMkModule :: TokenId
tokenMkModule = mkTracingToken "mkModule"

tokenMkPos :: TokenId
tokenMkPos = mkTracingToken "mkSourceRef" 

tokenMkNoPos :: TokenId
tokenMkNoPos = mkTracingToken "mkNoSourceRef"

tokenMkAtomCon :: TokenId
tokenMkAtomCon = mkTracingToken "mkAtomCon"

tokenMkAtomId :: Bool -> TokenId
tokenMkAtomId toplevel = 
  mkTracingToken (if toplevel then "mkAtomIdToplevel" else "mkAtomId") 

tokenMkConst :: TokenId
tokenMkConst = mkTracingToken "mkTNm"

tokenMkAtomLambda :: TokenId
tokenMkAtomLambda = mkTracingToken "mkNTLambda"

tokenMkAtomCase :: TokenId
tokenMkAtomCase = mkTracingToken "mkNTCase"

tokenMkAtomIf :: TokenId
tokenMkAtomIf = mkTracingToken "mkNTIf"

tokenMkAtomGuard :: TokenId
tokenMkAtomGuard = mkTracingToken "mkNTGuard"


-- tokens for expression combinators:

tokenPrim :: Arity -> TokenId
tokenPrim = mkTracingTokenArity "prim"

tokenCon :: Arity -> TokenId
tokenCon = mkTracingTokenArity "con"

tokenPa :: Arity -> TokenId
tokenPa = mkTracingTokenArity "pa"

tokenCn :: Arity -> TokenId
tokenCn = mkTracingTokenArity "cn"

-- tokenEagerSat :: TokenId
-- tokenEagerSat = mkTracingToken "eagerSat"

tokenLazySat :: TokenId
tokenLazySat = mkTracingToken "lazySat"
tokenULazySat :: TokenId
tokenULazySat = mkTracingToken "ulazySat"

tokenAp :: Arity -> TokenId
tokenAp = mkTracingTokenArity "ap" 
tokenUAp :: Arity -> TokenId
tokenUAp = mkTracingTokenArity "uap"

-- tokenRap :: Arity -> TokenId
-- tokenRap = mkTracingTokenArity "rap" 

tokenFun :: Arity -> TokenId
tokenFun = mkTracingTokenArity "fun"
tokenUFun :: Arity -> TokenId
tokenUFun = mkTracingTokenArity "ufun"

tokenIndir :: TokenId
tokenIndir = mkTracingToken "indir"

tokenConInt :: TokenId
tokenConInt = mkTracingToken "conInt"
tokenConChar :: TokenId
tokenConChar = mkTracingToken "conChar"
tokenConFloat :: TokenId
tokenConFloat = mkTracingToken "conFloat"
tokenConDouble :: TokenId
tokenConDouble = mkTracingToken "conDouble"
tokenFromConInteger :: TokenId
tokenFromConInteger = mkTracingToken "fromConInteger"
tokenFromConRational :: TokenId
tokenFromConRational = mkTracingToken "fromConRational"
-- tokenPatFromConInteger :: TokenId
-- tokenPatFromConInteger = mkTracingToken "patFromConInteger"
-- tokenPatFromConRational :: TokenId
-- tokenPatFromConRational = mkTracingToken "patFromConRational"

-- function for pattern-match failure error message
tokenFatal :: TokenId
tokenFatal = mkTracingToken "fatal"

-- other hardcoded tokens:

tokenOpenTrace :: TokenId
tokenOpenTrace = mkTracingToken "openTrace"

tokenCloseTrace :: TokenId
tokenCloseTrace = mkTracingToken "closeTrace"

tokenSR :: TokenId
tokenSR = mkTracingToken "SR"

tokenTrace :: TokenId
tokenTrace = mkTracingToken "Trace"

-- ----------------------------------------------------------------------------

expTo :: Pos -> Type TraceId -> Exp TokenId
expTo = expType True

expFrom :: Pos -> Type TraceId -> Exp TokenId
expFrom = expType False

expType :: Bool -> Pos -> Type TraceId -> Exp TokenId
expType to pos (TypeVar _ tyId) = 
  ExpVar pos (mkTypeToken (prefix to ++ "Id"))
expType to pos (TypeCons _ tyCon []) = 
  ExpVar pos (mkTypeToken (prefix to ++ typeName tyCon))
expType to pos (TypeCons _ tyCon [ty1,ty2]) | isFunTyCon tyCon =
  ExpApplication pos 
    [ExpVar pos (mkTypeToken (prefix to ++ "Fun")) 
    ,expType (not to) pos ty1 
    ,expType to pos ty2] 
expType to pos (TypeCons _ tyCon tys) = 
  ExpApplication pos 
    (ExpVar pos (mkTypeToken (prefix to ++ typeName tyCon)) 
    : map (expType to pos) tys)

prefix :: Bool -> String
prefix True = "to"
prefix False = "from"

typeName :: TraceId -> String
typeName aId = 
  case tokenId aId of
    TupleId n -> "Tuple" ++ show n
    _         -> getUnqualified aId

-- ----------------------------------------------------------------------------
-- various little helper functions

-- test for specific tokens

isFunTyCon :: TraceId -> Bool
isFunTyCon id = (tokenId id) == t_Arrow

isTrue :: TraceId -> Bool
isTrue id = (tokenId id) == tTrue

isOtherwise :: TraceId -> Bool
isOtherwise id = (tokenId id) == t_otherwise

isMain :: TraceId -> Bool
isMain id = (tokenId id) == tMain

-- other stuff

mkFailExp :: Pos -> Exp TokenId -> Exp TokenId
mkFailExp pos parent = ExpApplication pos [ExpVar pos tokenFatal,parent]

mkTupleExp :: Pos -> [Exp TokenId] -> Exp TokenId
mkTupleExp pos es = ExpApplication pos (ExpCon pos (t_Tuple (length es)): es)

importedModule :: ImpDecl a -> a
importedModule (Import (_,id) _) = id 
importedModule (ImportQ (_,id) _) = id
importedModule (ImportQas (_,id) _ _) = id 
importedModule (Importas (_,id) _ _) = id

noDecls :: Decls id
noDecls = DeclsParse []

mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd f = map (\(x,y) -> (x,f y))

-- ----------------------------------------------------------------------------
-- End
