{- ---------------------------------------------------------------------------
Transform a module for generating a trace.

Names are changed.
Module names are prefixed by a 'T'.
Variable names are prefixed to make room for new variable names 
refering to various traces and intermediate expressions.
Details of new name scheme near the end of this module.

Deferred:
import/export of the form TyCon(..); need extension of module TraceId
to handle data constructor info and field selectors

In multi-module program may get name conflict, because also names internal to
a module may be exported. Would need to keep track of all top-level names
that should be exported by default. 

No monad is used in the transformation, 
because there is nothing inherently sequential.
Instead, the definitions of the transformation functions `t*' remind of an 
attribut grammar: the arguments are the inherited attributes, the elements
of the result tuples are the synthetic attributes.
---------------------------------------------------------------------------- -}

module TraceTrans (traceTrans,maybeStripOffQual) where

import Syntax
import SyntaxPos (HasPos(getPos))
import TokenId (TokenId(TupleId,Visible,Qualified)
               ,mkUnqualifiedTokenId,isTidCon
               ,qualify,visible,extractV,extractM,dropM
               ,tPrelude,t_Tuple,t_Arrow,tTrue,tFalse,t_otherwise,t_undef
               ,tMain,tmain,tseq,t_Colon,t_List)
import TraceDerive (derive)
import PackedString (PackedString,packString,unpackPS)
import Extra (Pos,noPos,strPos,fromPos,mapListSnd,mapSnd)
import TraceId (TraceId,tokenId,arity,isLambdaBound,fixPriority,mkLambdaBound
               ,getUnqualified,modLetBound
               ,tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
               ,tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenMinus)
import AuxTypes (AuxiliaryInfo) -- needed for hbc's broken import mechanism
import List (isPrefixOf,union,partition,nubBy,delete)
import Char (isAlpha,digitToInt)
import Ratio (numerator,denominator)
import Maybe (fromJust,catMaybes)

import Extra (strace)

infixr 6 `typeFun`	-- hbc won't let me declare this later.

type Arity = Int

data Scope = Global | Local deriving Eq

isLocal :: Scope -> Bool
isLocal Local = True
isLocal Global = False


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
    (tImpDecls modId impDecls)
    [] -- no fix info needed, because pretty printed output not ambiguous
    (DeclsParse 
      (decls' 
       ++ [defNameMod pos modId filename traced]
       ++ map (defNameCon modTrace) cons 
       ++ map (defNameVar Global modTrace) tvars 
       ++ map (defNameVar Local modTrace) vars 
       ++ (if traced then map (defNamePos modTrace) poss else [])
       ++ if isMain modId then [defMain traceFilename] else [] ))
  where
  modTrace = ExpVar pos (nameTraceInfoModule modId)
  (poss,tvars,vars,cons) = getModuleConsts consts
  (DeclsParse decls',consts) = tDecls Global traced (mkRoot pos) decls
  

-- ----------------------------------------------------------------------------
-- If module is part of the given module (Prelude or TPrelude),
-- then strip off all of these module qualifications from identifiers.
-- These qualified identifiers are introduced by desugaring in the parser
-- and the fixity correction.

maybeStripOffQual :: String -> Module TokenId -> Module TokenId
maybeStripOffQual p mod@(Module pos modId exps impDecls fixDecls decls) =
  if isPreModule p modId then fmap (stripModule p) mod else mod

isPreModule :: String -> TokenId -> Bool
isPreModule p = (p `isPrefixOf`) . reverse . unpackPS . extractV 

stripModule :: String -> TokenId -> TokenId
stripModule qual' (Qualified qual unqual) | qual' == qualModule qual =
  Visible unqual
stripModule qual token = token

qualModule :: PackedString -> String
qualModule = reverse . takeWhile (/= '.') . unpackPS

-- ----------------------------------------------------------------------------
-- construct new main function definition

-- main = T.traceIO "artFilename" gmain
-- main = do
--  T.openTrace "artFilename"
--  case omain Prelude.undefined Prelude.undefined of
--    T.R v _ -> v
--  T.closeTrace
defMain :: String -> Decl TokenId

defMain artFilename =
  DeclFun noPos tokenmain 
    [Fun [] (Unguarded 
      (ExpApplication noPos 
        [ExpVar noPos tokenTraceIO
        ,ExpLit noPos (LitString Boxed artFilename)
        ,ExpApplication noPos 
          [ExpVar noPos tokengmain,mkSRExp noPos False,mkRoot noPos]]))
      noDecls]
  where
  tokenmain = visible (reverse "main")
  tokengmain = nameTransLetVar (mkLambdaBound tmain)

-- ----------------------------------------------------------------------------
-- Transform imports and exports

tExports :: Maybe [Export TraceId] -> Maybe [Export TokenId]
tExports = fmap (concatMap tExport) 

tExport :: Export TraceId -> [Export TokenId]
tExport (ExportModid pos modId) = [ExportModid pos (nameTransModule modId)]
tExport (ExportEntity pos entity) = map (ExportEntity pos) (tEntity entity)


tImpDecls :: TraceId -> [ImpDecl TraceId] -> [ImpDecl TokenId]
tImpDecls modId decls = 
    ImportQ (noPos,tPrelude) (Hiding [])
    -- import original Prelude qualified
    -- actually should hide Prelude as far as possible using
    -- ImportQ (noPos,tPrelude) (NoHiding [])
    -- but nhc98 needs access to original Prelude for desugaring
    :ImportQas (noPos,Visible tracingModule) 
       (noPos,Visible tracingModuleShort) (Hiding [])
    :map tImpDecl decls

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
tEntity (EntityVar pos id) = [EntityVar pos (nameTransLetVar id)]
tEntity (EntityConClsAll pos id) = [EntityConClsAll pos (nameTransTyConCls id)]
  -- INCOMPLETE: if TyCon(..) need also to import/export references to traces
  -- of data constructor names, but how know their names?
tEntity (EntityConClsSome pos id posIds)
  | not (null pCons) =  -- i.e. definitely a TyCon
    (EntityConClsSome pos (nameTransTyConCls id)
                          (mapListSnd nameTransCon pCons
                           ++ mapListSnd nameTransField pFields))
    : map (\(pos,id) -> EntityVar pos (nameTraceInfoCon id)) pCons
    -- ++ map (\(pos,id) -> EntityVar pos (nameTraceInfoField id)) pFields
  | otherwise = -- i.e. probably a TyClass
    [EntityConClsSome pos (nameTransTyConCls id) 
      (mapListSnd nameTransLetVar posIds ++ mapListSnd nameShare posIds)]
  where
  (pCons,pFields)   = partition (isTidCon.tokenId.snd) posIds

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

defNameCon :: Exp TokenId -> (Pos,TraceId,[TraceId]) -> Decl TokenId
defNameCon modTrace (pos,id,labels) =
  DeclFun pos (nameTraceInfoCon id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          (ExpVar pos (tokenMkAtomConstructor withLabels)
          :modTrace
          :ExpLit pos (LitInt Boxed (encodePos pos))
          :ExpLit pos (LitInt Boxed (fixPriority id))
          :ExpLit pos (LitInt Boxed (fromJust (arity id)))
          :ExpLit pos (LitString Boxed (getUnqualified id))
          :if withLabels
             then (:[]) . mkList pos . 
                    map (ExpVar pos . nameTraceInfoVar pos Global) $
                    labels
             else []
          )))
      noDecls]
  where
  withLabels = not (null labels)

defNameVar :: Scope -> Exp TokenId -> (Pos,TraceId,Scope) -> Decl TokenId
defNameVar idScope refMod (pos,id,defScope) =
  DeclFun pos (nameTraceInfoVar pos defScope id)
    [Fun []
      (Unguarded
        (ExpApplication pos
          [ExpVar pos tokenMkAtomVariable
          ,refMod
          ,ExpLit pos (LitInt Boxed (encodePos pos))
          ,ExpLit pos (LitInt Boxed (fixPriority id))
           -- all identifiers in definition position are assumed to 
           -- be equipped with an arity; 
           -- only those defined by pattern bindings do not; they have arity 0.
          ,ExpLit pos (LitInt Boxed 
            (case (arity id) of 
              Just a -> a
              Nothing -> 0))
          ,ExpLit pos (LitString Boxed (getUnqualified id))
          ,ExpCon pos (if isLocal idScope then tTrue else tFalse)]))
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
-- precondition: a constructor with position is only added once
-- a variable with position may be added several times, because
-- position may be zero 
-- because same position may be used for a variable, an application etc,
-- a position may be added several times.
-- The scope states if the variable is defined globally or locally;
-- note that definitions in classes/instances are local

data ModuleConsts = 
  MC [Pos]  -- positions used in traces
    [(Pos,TraceId,Scope)]  -- this-level variable ids for traces
    [(Pos,TraceId,Scope)]  -- variable ids for use in traces
    [(Pos,TraceId,[TraceId])]  -- constructor ids for use in traces
                               -- together with field labels (global)

emptyModuleConsts :: ModuleConsts
emptyModuleConsts = MC [] [] [] []

addPos :: Pos -> ModuleConsts -> ModuleConsts
addPos pos (MC poss tids ids cons) = MC (pos `insert` poss) tids ids cons

addVar :: Pos -> TraceId -> Scope -> ModuleConsts -> ModuleConsts
addVar pos id scope (MC poss tids ids cons) = 
  MC (pos `insert` poss) ((pos,id,scope) `insert` tids) ids cons

addCon :: Pos -> TraceId -> [TraceId] -> ModuleConsts -> ModuleConsts
addCon pos id labels (MC poss tids ids cons) =
  MC (pos `insert` poss) tids ids ((pos,id,labels): cons)

-- both from the same declaration level
merge :: ModuleConsts -> ModuleConsts -> ModuleConsts
merge (MC poss1 tids1 ids1 cons1) (MC poss2 tids2 ids2 cons2) = 
  MC (poss1 `union` poss2) (tids1 `union` tids2) (ids1 `union` ids2) 
    (cons1 ++ cons2)

-- combine this declaration level with a local declaration level
withLocal :: ModuleConsts -> ModuleConsts -> ModuleConsts
withLocal (MC poss1 tids1 ids1 cons1) (MC poss2 tids2 ids2 []) =
  MC (poss1 `union` poss2) tids1 (ids1 `union` tids2 `union` ids2) cons1
withLocal _ _ = error "TraceTrans.withLocal: locally defined data constructors"

getModuleConsts :: ModuleConsts 
                -> ([Pos],[(Pos,TraceId,Scope)],[(Pos,TraceId,Scope)]
                   ,[(Pos,TraceId,[TraceId])])
getModuleConsts (MC pos tids ids cons) = (pos,tids,ids,cons)

-- avoid duplicate
insert :: Eq a => a -> [a] -> [a] 
insert p ps = p : delete p ps

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

tDecls :: Scope -> Bool -> Exp TokenId -> Decls TraceId 
       -> (Decls TokenId,ModuleConsts)
tDecls scope traced parent (DeclsParse decls) = (DeclsParse decls',declsConsts)
  where
  (decls',declsConsts) = 
    foldr combine ([],emptyModuleConsts) . map (tDecl scope traced parent) 
    . combineFuns $ decls
  combine :: ([Decl id],[Decl id],ModuleConsts) -> ([Decl id],ModuleConsts)
          -> ([Decl id],ModuleConsts)
  combine (ds11,ds12,c1) (ds,c2) = (ds11++ds12++ds,c1 `merge` c2)

-- for declarations in class and instance definitions:
-- (considered local, because they have their own scope)
tDecls2 :: Bool -> Exp TokenId -> Decls TraceId 
        -> (Decls TokenId,[Decl TokenId],ModuleConsts)
tDecls2 traced parent (DeclsParse decls) = 
  (DeclsParse (concat declss1 ++ catMaybes (map declSharedVar decls))
  ,concat declss2
  ,foldr merge emptyModuleConsts declsConstss)
  where
  (declss1,declss2,declsConstss) = 
    unzip3 (map (tDecl Local traced parent) . combineFuns $ decls)

declSharedVar :: Decl TraceId -> Maybe (Decl TokenId)
declSharedVar (DeclVarsType vars contexts ty) =
  Just (DeclVarsType (tPosShares vars) (tContexts contexts) (tConstType ty))
declSharedVar _ = Nothing

{-
tDecl2 :: Bool -> Exp TokenId -> Decl TraceId 
       -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tDecl2 traced parent (DeclFun pos id [Fun [] rhs localDecls]) = 
  -- methods always take a position and trace as argument,
  -- even if these are not used in cafs,
  -- because instances of one method may have different arities
  -- but all must have the "same" type.
  -- Sharing of constants is lost; outside of class/instance
  -- a shared constant would still need a class context (=> no longer constant)
  ([DeclFun pos id' 
     [Fun [PatWildcard pos,PatWildcard pos] rhs' localDecls']]
  ,traceDecl
  ,declConsts)
  where
  ([DeclFun _ id' [Fun [] rhs' localDecls']],traceDecl,declConsts) = 
    tCaf Local traced parent pos id rhs localDecls
tDecl2 traced parent decl = tDecl Local traced parent decl

-}


singleDecl :: Decl id -> ([Decl id],[a],ModuleConsts)
singleDecl decl = ([decl],[],emptyModuleConsts)

-- Sharing of constants in classes/instances
-- may be lost if class/instance has a context,
-- because then the shareId also has this context and is no longer a constant.

tDecl :: Scope -> Bool -> Exp TokenId -> Decl TraceId 
      -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tDecl _ _ _ (DeclType lhsTy rhsTy) = 
  singleDecl $ DeclType (tSimple lhsTy) (tType rhsTy)
tDecl Global _ _ (DeclData sort contexts lhsTy constrs pClss) = 
  ([DeclData sort (tContexts contexts) (tSimple lhsTy) 
    (map tConstr constrs) []] 
    -- "derive" should be empty, because transformed classes cannot be derived
  ,instDecl:fieldSelectorDecls++deriveDecls
  ,foldr addConInfo (fieldSelectorConsts `merge` deriveConsts) constrs)
  where
  (DeclsParse deriveDecls,deriveConsts) = 
     tDecls Global False (mkRoot noPos) 
       (DeclsParse (derive contexts lhsTy constrs pClss))
  instDecl = wrapValInstDecl (getPos lhsTy) contexts lhsTy constrs
  (fieldSelectorDecls,fieldSelectorConsts) = mkFieldSelectors constrs
  addConInfo :: Constr TraceId -> ModuleConsts -> ModuleConsts
  addConInfo constr = 
    addCon (getPos constr) (getConstrId constr) 
      (map snd (getConstrLabels constr))
tDecl _ _ _ (DeclDataPrim pos id size) = 
  error ("Cannot trace primitive data type (" ++ show (tokenId id) 
    ++ " at position " ++ strPos pos ++ ")")
tDecl _ traced parent (DeclClass pos contexts clsId tyId decls) = 
  ([DeclClass pos (tContexts contexts) (nameTransTyConCls clsId) 
     (nameTransTyVar tyId) decls1]
  ,decls2  -- auxiliary definitions have to be outside the class definition
  ,declsConsts)
  where
  (decls1,decls2,declsConsts) = tDecls2 traced parent decls
tDecl _ traced parent (DeclInstance pos contexts clsId inst decls) = 
  ([DeclInstance pos (tContexts contexts) (nameTransTyConCls clsId) 
     (tType inst) decls1]
  ,decls2  -- auxiliary definitions have to be outside the instance definition
  ,declsConsts)
  where
  (decls1,decls2,declsConsts) = tDecls2 traced parent decls
tDecl _ _ _ (DeclDefault tys) = 
  singleDecl $ DeclDefault (map tType tys)
tDecl _ _ _ d@(DeclPrimitive pos fnId arity ty) =
  error "TraceTrans:tDecl _ _ _ (DeclPrimitive _ _ _ _) should not occur"
tDecl _ _ _ (DeclForeignImp pos Haskell hasName fnId arity _ ty _) =
  tHaskellPrimitive pos 
    (if null revHasModNameP 
       then visible revHasUnqualName 
       else (qualify (tail revHasModNameP) revHasUnqualName))
    fnId arity ty
  where
  (revHasUnqualName,revHasModNameP) = span (/= '.') . reverse $ hasName 
tDecl _ _ _ 
  (DeclForeignImp pos callConv cname fnId arity fspec ty duplicateId) =
  (funDecls
  ,DeclForeignImp pos callConv
    (if null cname then getUnqualified fnId else cname) 
    (nameForeign fnId) arity fspec (tokenIdType ty) (nameForeign fnId)
   :wrapperDecls
  ,consts)
  where
  (funDecls,wrapperDecls,consts) = 
    tHaskellPrimitive pos (nameForeign fnId) fnId arity ty
  sr = ExpVar pos (nameSR fnId)
  useParent = ExpVar pos (nameTrace fnId)
tDecl _ _ _ (DeclForeignExp pos callConv str fnId _) =
  error ("Cannot trace foreign export (used at " ++ strPos pos ++ ")")
tDecl _ _ _ (DeclVarsType vars contexts ty) =
  ([DeclVarsType (tPosExps vars) (tContexts contexts) (tFunType ty)]
  -- shared constants need to be typed, in case they are overloaded,
  -- so that monomorphic restriction does not yield to type error
  -- (actually then sharing is unfortunately lost)
   ++ if null constVars then [] 
        else [DeclVarsType (tPosShares constVars) 
               (tContexts contexts) (tConstType ty)]
  ,[],emptyModuleConsts)
  where
  (constVars,nonConstVars) = partition (isNonMethodConstant . snd) vars
  isNonMethodConstant :: TraceId -> Bool
  isNonMethodConstant id = 
    isLambdaBound id || -- variables in pattern bindings are lambda bound
      (case arity id of
        Just n  -> n == 0
        Nothing -> False)
{-
  -- Variables of arity 0 do not take SR and Trace argument, so that
  -- their values are shared. Note that type signatures for class methods
  -- are handled differently by tDecl2
  ((if null constVars then [] 
      else [DeclVarsType (tPosExps constVars) 
             (tContexts contexts) (tConstType ty)])
   ++
   (if null nonConstVars then [] else [DeclVarsType (tPosExps nonConstVars) 
                                        (tContexts contexts) (tFunType ty)])
  ,[],emptyModuleConsts)
  where
  (constVars,nonConstVars) = partition (isNonMethodConstant . snd) vars
  isNonMethodConstant :: TraceId -> Bool
  isNonMethodConstant id = 
    isLambdaBound id || -- variables in pattern bindings are lambda bound
      (case arity id of
        Just n  -> n == 0
        Nothing -> False)
-}
tDecl scope traced parent (DeclPat (Alt (ExpVar pos id) rhs decls)) = 
  -- this case may occur because of the next equation
  tCaf scope traced parent pos id rhs decls
tDecl scope traced parent (DeclPat (Alt (PatAs pos id pat) rhs decls)) = 
  (dFun1++dPat1,dFun2++dPat2,funConsts `merge` patConsts)
  where
  id' = modLetBound id
  (dFun1,dFun2,funConsts) = tCaf scope traced parent pos id' rhs decls
  (dPat1,dPat2,patConsts) = 
    tDecl scope traced parent 
      (DeclPat (Alt pat (Unguarded (ExpVar pos id')) noDecls))
tDecl scope traced parent (DeclPat (Alt pat rhs decls)) =
  -- unfortunately we cannot transform a pattern binding into another pattern
  -- binding; we have to introduce an explicit `case' to be able to terminate 
  -- with an appropriate error message when the pattern does not match.
  -- first rewrite as p = e, then
  -- xi sr p = constUse sr p zi
  -- zi = constDef parent 
  --        (\_ -> (case patId of (t,y1,..,yn) -> projection sr t yi))
  -- patId = case e' of 
  --           p' -> (t,y1,..,yn)
  --           _  -> fail noPos parent
  (map useDef patPosIds
  ,DeclFun noPos patId 
    [Fun [] 
      (Unguarded 
        (ExpCase noPos exp'
          [Alt pat'' (Unguarded tuple) noDecls
          ,Alt (PatWildcard noPos) (Unguarded (mkFailExp noPos parent)) noDecls
          ]))
      decls']
   : map projDef patPosIds
  ,foldr (\(pos,id) -> addVar pos id scope) 
    (emptyModuleConsts `withLocal` altConsts) patPosIds)
  where
  pos = getPos pat
  firstId = snd . head $ patPosIds
  patId = nameTraceShared pos firstId
  resultTraceId = nameTrace2 firstId
  tuple = mkTupleExp noPos (ExpVar noPos resultTraceId : patVars')
  patPosIds = map (\(ExpVar pos id) -> (pos,id)) patVars
  (patVars',[],[]) = tPats patVars 
  patVars = getPatVars pat
  pat'' = case pat' of
           ExpApplication p [r,v,_] -> 
             ExpApplication p [r,v,ExpVar noPos resultTraceId]
  (Fun [pat'] (Unguarded exp') decls',altConsts) = 
     tFun traced False parent failContinuation (Fun [pat] rhs decls)
  useSR = ExpVar pos (nameSR firstId)
  useParent = ExpVar pos (nameTrace firstId)

  useDef :: (Pos,TraceId) -> Decl TokenId
  useDef (pos,id) =
    DeclFun pos (nameTransLetVar id)
      [Fun [useSR,useParent]
        (Unguarded (ExpApplication pos
          [combConstUse pos,useSR,useParent,ExpVar pos (nameShare id)]))
        noDecls]

  projDef :: (Pos,TraceId) -> Decl TokenId
  projDef (pos,id) =
    DeclFun pos (nameShare id) 
      [Fun []
        (Unguarded (ExpApplication pos 
          [combConstDef pos traced 
          ,parent
          ,ExpVar pos (nameTraceInfoVar pos scope id)
          ,ExpLambda pos [PatWildcard pos]
            (ExpCase pos (ExpVar pos patId)
              [Alt tuple
                (Unguarded 
                  (ExpApplication pos 
                    [ExpVar pos tokenProjection
                    ,mkSRExp pos traced
                    ,ExpVar pos resultTraceId
                    ,ExpVar pos (nameTransLambdaVar id)])) 
                noDecls])]))
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
tDecl scope traced parent (DeclFun pos id [Fun [] rhs localDecls]) = 
  tCaf scope traced parent pos id rhs localDecls
    -- a caf has many dynamic parents and hence uses the static parent
tDecl _ _ parent (DeclFun pos id (Fun [] _ _ : _)) =
  error "tDecl: variable multiple defined"
tDecl scope traced parent (DeclFun pos id funs) = 
  tFuns scope traced pos id funs  -- a function does not use the static parent
tDecl _ _ _ (DeclFixity _) = ([],[],emptyModuleConsts) 
  -- fixity declarations have been processed before 
  -- not needed in output, because pretty printer produces unambiguous output
tDecl _ _ _ (DeclIgnore s) = ([DeclIgnore s],[],emptyModuleConsts)
tDecl _ _ _ _ = error "tDecl: unknown sort of declaration"


-- constructor definition in type definition
tConstr :: Constr TraceId -> Constr TokenId
tConstr (Constr pos conId tyArgs) =
  Constr pos (nameTransCon conId) (tTyArgs tyArgs)
tConstr (ConstrCtx tyVars contexts pos conId tyArgs) =
  ConstrCtx (tPosTyVars tyVars) (tContexts contexts) 
    pos (nameTransCon conId) (tTyArgs tyArgs)


-- build the instance of class WrapVal for type with given data constructors
-- this instance is needed for constructing and updating with labelled fields
wrapValInstDecl :: Pos -> [Context TraceId] -> Simple TraceId 
                -> [Constr TraceId] -> Decl TokenId
wrapValInstDecl pos contexts ty constrs =
  DeclInstance pos (map (fmap tokenId) contexts) tokenWrapValClass 
    (fmap tokenId (simpleToType ty)) 
    (DeclsParse [DeclFun pos ({-dropM-} tokenWrapValFun) (map wrapValFun constrs)])
  where
  traceTokenWrapValFun = mkLambdaBound (dropM tokenWrapValFun)
  sr = ExpVar pos (nameSR traceTokenWrapValFun)
  parent = ExpVar pos (nameTrace traceTokenWrapValFun)
  varId = nameTrace2 traceTokenWrapValFun -- actually not a trace
  var = ExpVar pos varId
  infiniteTraces = map (ExpVar pos) . nameArgs $ traceTokenWrapValFun
  wrapValFun :: Constr TraceId -> Fun TokenId
  wrapValFun constr =
    Fun [sr,PatAs pos varId consApp,parent] 
      (Unguarded (wrapExp pos var consAppTrace)) noDecls
    where
    consAppTrace = 
      if numOfArgs == 0 then fun
        else ExpApplication pos .
               (ExpVar pos (tokenMkExpApp numOfArgs) :) . (parent :) .
                (sr :) . (fun :) $ traces 
    fun = 
      ExpApplication pos 
        [ExpVar pos tokenMkExpValueUse
        ,parent,sr,ExpVar pos (nameTraceInfoCon consId)]
    consApp =
      if numOfArgs == 0 then ExpCon pos (tokenId consId)
        else ExpApplication pos . (ExpCon pos (tokenId consId) :) .
               map (wrapExp pos (PatWildcard pos)) $ traces
    consId = getConstrId constr
    traces = take numOfArgs infiniteTraces :: [Exp TokenId]
    numOfArgs = sum . map (repeated . fst) $ constrArgs
    repeated Nothing = 1
    repeated (Just labels) = length labels
    constrArgs :: [(Maybe [(Pos,TraceId)],Type TraceId)]
    constrArgs = getConstrArgumentList constr


tHaskellPrimitive :: Pos -> TokenId -> TraceId -> Arity -> Type TraceId 
                  -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tHaskellPrimitive pos hasId fnId arity ty 
  -- import of a Haskell function
  -- used for defining builtin Haskell functions
  -- transformation yields a wrapper to the untransformed function
  | arity == 0 =
    ([DeclVarsType [(pos,nameTransLetVar fnId)] [] (tFunType ty)
     ,DeclFun pos (nameTransLetVar fnId)
       [Fun [sr,useParent] 
         (Unguarded (ExpApplication pos
           [combConstUse pos,sr,useParent,ExpVar pos shareId]))
         noDecls]]
    ,[DeclFun pos shareId
       [Fun []
         (Unguarded (ExpApplication pos 
           [combConstDef pos False,mkRoot pos
           ,ExpVar pos (nameTraceInfoVar pos Global fnId)
           ,ExpLambda pos [useParent] 
             (ExpApplication pos 
               [expFrom pos ty,useParent, ExpVar pos hasId])]))
         noDecls]]
    ,addVar pos fnId Global emptyModuleConsts)
  | otherwise =
    ([DeclVarsType [(pos,nameTransLetVar fnId)] [] (tFunType ty)
     ,DeclFun pos (nameTransLetVar fnId) 
       [Fun [sr,useParent]
         (Unguarded 
           (ExpApplication pos 
             [combFun pos False arity
             ,ExpVar pos (nameTraceInfoVar pos Global fnId)
             ,sr,useParent,ExpVar pos wrappedId']))
          noDecls]]
    ,[DeclFun pos wrappedId' 
       [Fun (args++[hidden])
         (Unguarded (ExpApplication pos
           [expFrom pos tyRes,hidden
           ,ExpApplication pos (ExpVar pos hasId : zipWith to tyArgs args)]))
         noDecls]]
    ,addVar pos fnId Global emptyModuleConsts)
    where
    sr = ExpVar pos (nameSR fnId)
    useParent = ExpVar pos useParentId
    useParentId = nameTrace fnId
    hidden = ExpVar pos (nameTrace2 fnId)
    args = take arity . map (ExpVar pos) . nameArgs $ fnId
    wrappedId' = nameWorker fnId
    shareId = nameShare fnId
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


mkFieldSelectors :: [Constr TraceId] -> ([Decl TokenId],ModuleConsts)
mkFieldSelectors constrs = 
    foldr combine ([],emptyModuleConsts) . map (uncurry mkFieldSelector) $
      nonDuplicatePosFields
  where
  combine :: (Decl TokenId,ModuleConsts) -> ([Decl TokenId],ModuleConsts) 
          -> ([Decl TokenId],ModuleConsts)
  combine (decl1,modConsts1) (decls2,modConsts2) = 
    (decl1:decls2,modConsts1 `merge` modConsts2)
  nonDuplicatePosFields :: [(Pos,TraceId)]
  nonDuplicatePosFields = 
    nubBy (\(_,id1) (_,id2) -> tokenId id1 == tokenId id2) posFields
  posFields = 
    concat [pf | (Just pf,_) <- concatMap getConstrArgumentList constrs]


-- construct the traced version of a field selector, using the 
-- normal field selector, i.e. from zname :: T -> R Int construct
-- gname :: SR -> Trace -> R (Fun T Int)
-- gname sr p = fun1 "name" hname sr p
--   where
--   hname :: Trace -> R T -> R Int
--   hname p (R v _) = projection sr p (zname v)
mkFieldSelector :: Pos -> TraceId -> (Decl TokenId,ModuleConsts)
mkFieldSelector pos fieldId =
  (DeclFun pos (nameTransLetVar fieldId) 
    [Fun [sr,parent]
      (Unguarded
        (ExpApplication pos
          [combFun pos False 1
          ,ExpVar pos (nameTraceInfoVar pos Global fieldId)
          ,sr,parent,ExpVar pos wrappedId']))
      (DeclsParse 
        [DeclFun pos wrappedId' 
          [Fun [wrapExp pos var (PatWildcard pos),parent] 
            (Unguarded 
              (ExpApplication pos 
                [ExpVar pos tokenProjection,mkSRExp pos False,parent
                ,ExpApplication pos [ExpVar pos (nameTransField fieldId),var]
                ])) 
            noDecls]])]
  ,addVar pos fieldId Global emptyModuleConsts)
  where
  sr = ExpVar pos (nameSR fieldId)
  parent = ExpVar pos (nameTrace fieldId)
  wrappedId' = nameWorker fieldId
  var = ExpVar pos varId
  varId:_ = nameArgs fieldId


tCaf :: Scope -> Bool -> Exp TokenId -> Pos -> TraceId -> Rhs TraceId 
     -> Decls TraceId
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)
tCaf scope traced parent pos id rhs localDecls =
  -- id sr p = constUse sr p id'
  -- id' = constDef parent "id" (\p' -> [[rhs]]_p')
  ([DeclFun pos (nameTransLetVar id)
     [Fun [useSR,useParent] 
       (Unguarded (ExpApplication pos 
         [combConstUse pos,useSR,useParent,id'])) 
       noDecls]
   ,DeclFun pos idId'
     [Fun [] 
       (Unguarded (ExpApplication pos 
         [combConstDef pos traced,parent
         ,ExpVar pos (nameTraceInfoVar pos scope id)
         ,ExpLambda pos [useParent] (smartExpLet pos localDecls' rhs')])) 
       noDecls]]
  ,[]
  ,addVar pos id scope emptyModuleConsts `withLocal` 
    (rhsConsts `merge` localDeclsConsts))
  where
  idId' = nameShare id
  id' = ExpVar pos idId'
  useSR = ExpVar pos (nameSR id)
  useParentId = nameTraceShared pos id
  useParent = ExpVar pos useParentId
  (rhs',rhsConsts) = tRhs traced True useParent failContinuation rhs
  (localDecls',localDeclsConsts) = tDecls Local traced useParent localDecls
  smartExpLet :: Pos -> Decls a -> Exp a -> Exp a
  smartExpLet pos (DeclsParse []) e = e
  smartExpLet pos decls e = ExpLet pos decls e

tFuns :: Scope -> Bool -> Pos -> TraceId -> [Fun TraceId]
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tFuns scope traced pos id funs =
  ([DeclFun pos (nameTransLetVar id) 
     [Fun [sr,parent]
       (Unguarded
         (ExpApplication pos
           [combFun pos traced funArity
           ,ExpVar pos (nameTraceInfoVar pos scope id)
           ,sr,parent,ExpVar pos wrappedId']))
       (DeclsParse (DeclFun pos wrappedId' funs' : newDecls'))]]
       -- worker must be local, because if wrapper is within an instance,
       -- then worker cannot be in instance scope; neither can it be on
       -- top level scope, because there may be several instances for the
       -- same class and all workers have the same name.
       -- In same scope as possible type decl (which hasn't been implemented)
  ,[]
  ,addVar pos id scope (emptyModuleConsts `withLocal` funConsts))
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
     (replicate funArity (PatWildcard pos) ++ [parent])
     (Unguarded (continuationToExp parent failContinuation)) noDecls]
  ,[],emptyModuleConsts)
tFunClauses traced pos parent ids pVars funArity _ (fun@(Fun pats _ _) : funs)
  | not (null funs) && funCanFail fun =
    ([Fun (pats'' ++ [parent]) rhs' decls'
     ,Fun (vars ++ [parent]) 
       (Unguarded (continuationToExp parent failCont)) noDecls]
    ,DeclFun pos contId funs' : funsDecls
    ,funConsts `merge` funsConsts)
  where
  contId = head ids
  failCont = functionContinuation contId vars
  (pats'',vars) = namePats pats' pVars 
  (Fun pats' rhs' decls',funConsts) = tFun traced True parent failCont fun
  (funs',funsDecls,funsConsts) = 
    tFunClauses traced pos parent (tail ids) pVars 
      funArity (neverFailingPats pats) funs
tFunClauses traced pos parent ids pVars funArity _ (fun@(Fun pats _ _): funs) =
  -- last clause or guards and numeric literals cannot fail
  (Fun (pats'++[parent]) rhs' decls' : funs'
  ,funsDecls
  ,funConsts `merge` funsConsts)
  where
  (Fun pats' rhs' decls',funConsts) = 
    tFun traced True parent failContinuation fun
  (funs',funsDecls,funsConsts) = tFunClauses traced pos parent ids pVars 
                                 funArity (neverFailingPats pats) funs


-- Numeric literals need to be overloaded with respect to the new
-- transformed numeric classes; hence they cannot just be left wrapped
-- in patterns
-- Transform such literals into equality conditions in guards.
-- Need also to desugare ~, because that is the easiest way to deal with 
-- literals within the scope of a ~. NOT YET DONE.
tFun :: Bool -- traced
     -> Bool -- this is reduct of parent
     -> Exp TokenId -- parent
     -> ContExp -- continuation in case of pattern match failure
     -> Fun TraceId -> (Fun TokenId,ModuleConsts)
-- Definition similar to tGuardedExps
tFun traced cr parent contExp (Fun pats rhs decls) =
  if null conditions  -- implies null patsDecls
    then (Fun pats' (Unguarded rhs') decls',declsConsts `withLocal` rhsConsts)
    else
      (Fun pats' 
        (Unguarded (ExpApplication pos 
          (if traced 
             then
               [combGuard pos True,mkSRExp pos traced,parent,cond'
               ,ExpLambda pos [newParent] (ExpLet pos patsDecls' rhs')
               ,ExpLambda pos [newParent] 
                 (continuationToExp newParent contExp)]
             else
               [combGuard pos False,cond',ExpLet pos patsDecls' rhs'
               ,continuationToExp parent contExp])
        )) decls'
      ,pos `addPos` condConsts `merge` patsDeclsConsts `merge` declsConsts 
       `withLocal` rhsConsts)  
      -- condConsts contains positions of the boolean expressions
      -- patsDeclsConsts contains positions of the bound variables
  where
  (pats',conditions,patsDecls) = tPats pats
  (patsDecls',patsDeclsConsts) = 
    tDecls Local traced (if traced then newParent else parent) 
      (DeclsParse patsDecls) 
  (cond',condConsts) = tExp traced False parent (foldr1 andExp conditions)
  (rhs',rhsConsts) = 
    tRhs traced cr 
      (if null conditions || not traced then parent else newParent) contExp rhs
  (decls',declsConsts) = tDecls Local traced parent decls
  andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
  andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]
  pos = getPos pats
  newParent = ExpVar pos (nameFromPos pos)


funCanFail :: Fun TraceId -> Bool
funCanFail (Fun pats rhs _) = 
  any numericLitIn pats || case rhs of
    Unguarded _ -> False
    Guarded gdExps -> gdExpsCanFail gdExps

numericLitIn :: Pat TraceId -> Bool
numericLitIn (ExpRecord pat fields) = 
  numericLitIn pat || any numericLitInField fields
  where
  numericLitInField (FieldExp _ _ pat) = numericLitIn pat
numericLitIn (ExpApplication _ pats) = any numericLitIn pats
numericLitIn (ExpList _ pats) = any numericLitIn pats
numericLitIn (PatAs _ _ pat) = numericLitIn pat
numericLitIn (PatIrrefutable _ pat) = numericLitIn pat
numericLitIn (ExpLit _ (LitInteger _ _)) = True
numericLitIn (ExpLit _ (LitRational _ _)) = True
numericLitIn _ = False


-- Returns False only if one of the guards definitely evaluates to True.
gdExpsCanFail :: [(Exp TraceId,Exp TraceId)] -> Bool
gdExpsCanFail [] = True
gdExpsCanFail ((ExpCon _ cid, _) : gdExps) = 
  not (isTrue cid) && gdExpsCanFail gdExps
gdExpsCanFail ((ExpVar _ cid, _) : gdExps) = 
  not (isOtherwise cid) && gdExpsCanFail gdExps
gdExpsCanFail (_ : gdExps) = gdExpsCanFail gdExps


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
tGuardedExps True cr parent failCont ((guard,exp):gdExps) =
  (ExpApplication pos 
    [combGuard pos True,mkSRExp pos True,parent,guard'
    ,ExpLambda pos [guardParent] exp',ExpLambda pos [guardParent] gdExps']
  ,pos `addPos` guardConsts `merge` expConsts `merge` gdExpsConsts)
  where
  (guard',guardConsts) = tExp True False parent guard
  (exp',expConsts) = tExp True cr guardParent exp
  (gdExps',gdExpsConsts) = tGuardedExps True cr guardParent failCont gdExps
  guardParent = ExpVar pos (nameFromPos pos)
  pos = getPos guard
tGuardedExps False cr parent failCont ((guard,exp):gdExps) =
  (ExpApplication pos 
    [combGuard pos False,guard',exp',gdExps']
  ,guardConsts `merge` expConsts `merge` gdExpsConsts)
  where
  (guard',guardConsts) = tExp False False parent guard
  (exp',expConsts) = tExp False cr parent exp
  (gdExps',gdExpsConsts) = tGuardedExps False cr parent failCont gdExps
  pos = getPos guard

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
  ExpApplication noPos (ExpVar noPos fun : args ++ [parent])


mapMerge2 :: (a -> (b,ModuleConsts)) -> [a] -> ([b],ModuleConsts)
mapMerge2 f = mapSnd (foldr merge emptyModuleConsts) . unzip . map f


-- Transform expressions

tExps :: Bool           -- traced
      -> Exp TokenId    -- parent
      -> [Exp TraceId]  -- expressions
      -> ([Exp TokenId],ModuleConsts)
tExps traced parent = mapMerge2 (tExp traced False parent)


-- Second argument True iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
tExp :: Bool -> Bool -> Exp TokenId -> Exp TraceId 
     -> (Exp TokenId,ModuleConsts)
tExp traced cr parent (ExpLambda pos pats body) =
  (ExpApplication pos 
    [combFun pos traced funArity
    ,ExpVar pos tokenMkAtomLambda
    ,mkSRExp pos traced
    ,parent
    ,if neverFailingPats pats 
       then ExpLambda pos (pats' ++ [lambdaParent]) body'
       else ExpLambda pos (vars ++ [lambdaParent])
              (ExpCase pos (mkTupleExp pos vars)
                [Alt (mkTupleExp pos pats') (Unguarded body') noDecls
                ,Alt (PatWildcard pos) 
                   (Unguarded (mkFailExp pos lambdaParent)) noDecls])]
  ,pos `addPos` bodyConsts)
  where
  (Fun pats' (Unguarded body') _,bodyConsts) = 
    tFun traced True lambdaParent failContinuation 
      (Fun pats (Unguarded body) noDecls)
  vars = map (ExpVar pos) . take funArity $ varsIds
  lambdaParent = ExpVar pos lambdaParentId
  (lambdaParentId:varsIds) = namesFromPos pos
  funArity = length pats
tExp traced cr parent (ExpLet pos decls body) =
  (ExpLet pos decls' body'
  ,declConsts `withLocal` bodyConsts)
  where
  (decls',declConsts) = tDecls Local traced parent decls
  (body',bodyConsts) = tExp traced cr parent body
tExp traced cr parent (ExpDo pos stmts) =
  tExp traced cr parent (removeDo stmts)
tExp traced cr parent (ExpCase pos e alts) =
  (ExpApplication pos 
    [combCase pos traced,mkSRExp pos traced,parent
    ,ExpLet pos (DeclsParse (DeclFun pos varId fun' : defs')) 
      (ExpVar pos varId)
    ,e']
  ,pos `addPos` eConsts `merge` funConsts)
  where
  (varId:caseParentId:argId:funsIds) = namesFromPos pos
  (e',eConsts) = tExp traced False parent e
  (fun',defs',funConsts) = 
    tFunClauses traced pos (ExpVar pos caseParentId) funsIds 
      [ExpVar pos argId] 1 False
      . map alt2Fun $ alts
tExp True cr parent (ExpIf pos cond e1 e2) =
  (ExpApplication pos 
    [combIf pos True,mkSRExp pos True,parent,cond'
    ,ExpLambda pos [ifParent] e1',ExpLambda pos [ifParent] e2']
  ,pos `addPos` condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond',condConsts) = tExp True False parent cond
  (e1',e1Consts) = tExp True True ifParent e1
  (e2',e2Consts) = tExp True True ifParent e2
  ifParent = ExpVar pos (nameFromPos pos)
tExp False cr parent (ExpIf pos cond e1 e2) =
  (ExpApplication pos [combIf pos False,parent,cond',e1',e2']
  ,condConsts `merge` e1Consts `merge` e2Consts)
  where
  (cond',condConsts) = tExp False False parent cond
  (e1',e1Consts) = tExp False True parent e1
  (e2',e2Consts) = tExp False True parent e2
tExp traced cr parent (ExpType pos e contexts ty) =
  (ExpType pos e' (tContexts contexts) (wrapType (tType ty))
  ,eConsts)
  where
  (e',eConsts) = tExp traced cr parent e
tExp traced cr parent (ExpApplication pos (f@(ExpCon _ _) : es))=
  tConApp traced parent f es
tExp traced cr parent (ExpApplication pos es) =
  (ExpApplication pos 
    (combApply pos traced (length es - 1):mkSRExp pos traced:parent:es')
  ,pos `addPos` esConsts)
  where
  (es',esConsts) = tExps traced parent es
tExp traced cr parent (ExpVar pos id) =
  if isLambdaBound id  
    then 
      let e' = ExpVar pos (nameTransLambdaVar id) 
      in
        if cr 
        then (ExpApplication pos 
               [ExpVar pos tokenProjection,mkSRExp pos traced,parent,e']
             ,pos `addPos` emptyModuleConsts) 
        else (e',emptyModuleConsts)
    else 
      (ExpApplication pos 
        [ExpVar pos (nameTransLetVar id),mkSRExp pos traced,parent]
      ,pos `addPos` emptyModuleConsts)
tExp traced cr parent e@(ExpCon pos id) =
  tConApp traced parent e []
tExp traced cr parent (ExpLit pos litstr@(LitString _ s)) =
  -- Because the result is very large use special combinator that
  -- transforms string in traced string at runtime instead
  (ExpApplication pos
     [ExpVar pos tokenFromLitString,mkSRExp pos traced,parent
     ,ExpLit pos litstr]
  ,pos `addPos` emptyModuleConsts)
tExp traced cr parent (ExpLit pos lit@(LitChar _ _)) =
  (ExpApplication pos 
    [ExpVar pos tokenConChar,mkSRExp pos traced,parent,ExpLit pos lit]
  ,pos `addPos` emptyModuleConsts)
tExp traced cr parent (ExpLit pos lit@(LitRational b r)) =
  -- desugar rational constant into explicit use of ":%",
  -- because Rational is not a primitive type but defined in PreludeBasic
  -- however, this way mkNTRational is not used at all
  (ExpApplication pos 
    [combApply pos traced 1,sr,parent
    ,ExpApplication pos [ExpVar pos tokenFromRational,sr,parent]
    ,ExpApplication pos 
      [ExpCon pos tokenR
      ,ExpApplication pos 
        [ExpCon pos tokenConRational
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,num]
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,denom]]
      ,ExpApplication pos 
        [ExpVar pos tokenMkAtomRational,sr,parent,ExpLit pos lit]]]
  ,pos `addPos` emptyModuleConsts)
  where
  num = ExpLit pos (LitInteger b (numerator r))
  denom = ExpLit pos (LitInteger b (denominator r))
  sr = mkSRExp pos traced
tExp traced cr parent (ExpLit pos lit@(LitInteger _ _)) =
  (ExpApplication pos 
    [combApply pos traced 1,sr
    ,parent
    ,ExpApplication pos [ExpVar pos tokenFromInteger,sr,parent]
    ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,ExpLit pos lit]]
  ,pos `addPos` emptyModuleConsts)
  where
  sr = mkSRExp pos traced
tExp traced cr parent (ExpList pos es) =
  -- use special combinator that transforms list at runtime;
  -- desugaring and subsequent transformation would lead to large program.
  (ExpApplication pos
     [ExpVar pos tokenFromExpList,sr,parent,ExpList pos es']
  ,pos `addPos` esConsts)
  where
  (es',esConsts) = tExps traced parent es
  sr = mkSRExp pos traced
tExp traced cr parent (ExpRecord (ExpCon pos consId) fields) = -- construction
  (ExpApplication pos 
    [ExpVar pos tokenWrapValFun,sr,ExpRecord consUndefined fields',parent]
  ,pos `addPos` fieldsConsts)
  where
  consUndefined = 
    if consArity == 0 then ExpCon pos (nameTransCon consId)
      else ExpApplication pos . (ExpCon pos (nameTransCon consId) :) .
             take consArity . repeat $ 
               ExpApplication pos 
                 [ExpVar pos tokenUndefined,mkSRExp pos False,parent]
  Just consArity = arity consId
  sr = mkSRExp pos traced
  (fields',fieldsConsts) = mapMerge2 (tField traced parent) fields
tExp True cr parent (ExpRecord exp fields) = -- update
  (ExpLet pos 
    (DeclsParse $ 
      zipWith (DeclFun pos) fieldVarIds 
        (map ((:[]) . flip (Fun []) noDecls . Unguarded) fieldExps'))
    (ExpApplication pos
      (combUpdate pos True (length labels):mkSRExp pos True:parent:exp'
      :ExpLambda pos [var] (ExpRecord var varFields')
      :labels ++ fieldVars))
  ,pos `addPos` expConsts `merge` fieldsConsts)
  where
  (exp',expConsts) = tExp True False parent exp
  (fields',fieldsConsts) = mapMerge2 (tField True parent) fields
  labels = map (ExpVar pos . nameTraceInfoVar noPos Global) labelIds
  varFields' = zipWith (FieldExp pos) (map fieldLabel fields') fieldVars
  fieldExps' = map fieldExp fields'
  fieldVars = map (ExpVar pos) fieldVarIds
  fieldVarIds = map nameShare labelIds
  labelIds = map fieldLabel fields
  var = ExpVar pos (nameFromPos pos)
  pos = getPos fields
  fieldLabel (FieldExp _ labelId _) = labelId
  fieldExp (FieldExp _ _ exp) = exp
tExp False cr parent (ExpRecord exp fields) = -- update
  (ExpApplication pos
    [combUpdate pos False undefined,parent,exp'
    ,ExpLambda pos [var] (ExpRecord var fields')]
  ,expConsts `merge` fieldsConsts)
  where
  (exp',expConsts) = tExp True False parent exp
  (fields',fieldsConsts) = mapMerge2 (tField True parent) fields
  var = ExpVar pos (nameFromPos pos)
  pos = getPos fields
tExp _ _ _ _ = error "tExp: unknown sort of expression"


tField :: Bool -> Exp TokenId -> Field TraceId 
       -> (Field TokenId,ModuleConsts)
tField traced parent (FieldExp pos labelId exp) =
  (FieldExp pos (nameTransField labelId) exp',expConsts)
  where
  (exp',expConsts) = tExp traced False parent exp


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

fun2Alt :: Fun a -> Alt a
fun2Alt (Fun [pat] rhs decls) = Alt pat rhs decls


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
  newVar = ExpVar pos (mkLambdaBound newId)
  newId:_ = namesFromPos pos
  pos = getPos e


mapCombine3 :: (a -> (b,[c],[d])) -> [a] -> ([b],[c],[d])
mapCombine3 f = (\(p,es,ds) -> (p,concat es,concat ds)) . unzip3 . map f

-- the first part of the result is transformed pattern
-- the transformation has to remove numeric constants and
-- n+k patterns from a pattern, because numeric arguments are
-- wrapped and hence translating into constants and n+k patterns is impossible.
-- Furthermore we want their reductions in the trace as well.
-- the second part contains boolean expressions that are
-- implicitly tested in the original pattern but have to be made
-- explicit in the transformed program, e.g.
-- tPats 3 = (x,{x==3},{})
-- the third part contains definitions of numeric variables 
-- that orginate from n+k patterns
-- e.g.: tPats (n+2) = (x,{x>=2},{n=x-2})
-- Note that variables in patterns are always lambda bound
-- (pattern bindings are treated specially anyway)
tPats :: [Pat TraceId] -> ([Pat TokenId],[Exp TraceId],[Decl TraceId])
tPats = mapCombine3 tPat

tPat :: Pat TraceId -> (Pat TokenId,[Exp TraceId],[Decl TraceId])
tPat (ExpRecord (ExpCon pos id) fields) = 
  (wrapExp pos (ExpRecord (ExpCon pos (nameTransCon id)) fields') 
    (PatWildcard pos)
  ,fieldsExps
  ,fieldsDecls)
  where
  (fields',fieldsExps,fieldsDecls) = mapCombine3 tField fields
  tField (FieldExp pos id pat) = 
    (FieldExp pos (nameTransField id) pat',patExps,patDecls)
    where
    (pat',patExps,patDecls) = tPat pat
tPat (ExpApplication pos (ExpCon pos2 id : pats)) = 
  (wrapExp pos 
    (ExpApplication pos (ExpCon pos2 (nameTransCon id) : pats'))
    (PatWildcard pos)
  ,patsExps,patsDecls)
  where
  (pats',patsExps,patsDecls) = tPats pats
  -- negative numeric literals are represented as (negate number):
tPat (ExpApplication _ [_,ExpLit pos (LitInteger boxed i)]) =
  tPat (ExpLit pos (LitInteger boxed (-i)))
tPat (ExpApplication _ [_,ExpLit pos (LitRational boxed r)]) =
  tPat (ExpLit pos (LitRational boxed (-r)))
tPat (ExpApplication _ [_,ExpLit pos _]) = error "tPat: app expLit"
tPat (ExpVar pos id) = (ExpVar pos (nameTransLambdaVar id),[],[])
tPat (ExpCon pos id) = 
  (wrapExp pos (ExpCon pos (nameTransCon id)) (PatWildcard pos),[],[])
tPat (ExpLit pos (LitString _ s)) =
  tPat . mkTList pos . map (ExpLit pos . LitChar Boxed) $ s
tPat (ExpLit pos lit@(LitChar _ _)) = 
  (wrapExp pos (ExpLit pos lit) (PatWildcard pos),[],[]) 
tPat e@(ExpLit pos lit) = -- only LitInteger and LitRational left
  (ExpVar pos (nameTransLambdaVar tid)
  ,[ExpApplication pos 
     [ExpVar pos tTokenEqualEqual,ExpVar pos tid,e]]
  ,[])
  where
  tid = mkLambdaBound (nameFromPos pos)
tPat (ExpList pos pats) = tPat . mkTList pos $ pats
tPat (PatAs pos id pat) = 
  (PatAs pos (nameTransLambdaVar id) pat',patExps,patDecls)
  where
  (pat',patExps,patDecls) = tPat pat
tPat (PatWildcard pos) = (PatWildcard pos,[],[])  -- type change
tPat (PatIrrefutable pos pat) = 
  if null patExps 
    then
      (case pat' of
         ExpApplication pos' [r,p',t'] -> 
           ExpApplication pos' [r,PatIrrefutable pos p',t']
         x -> x
      ,[],[])
    else error "Numeric literal inside ~ is currently not implemented."
  where
  (pat',patExps,patDecls) = tPat pat 
tPat (PatNplusK pos id _ k _ _) = 
  (ExpVar pos (nameTransLambdaVar tid2)
  ,[ExpApplication pos 
     [ExpVar pos tTokenGreaterEqual,var2,k]]
  ,[DeclFun pos id
     [Fun []
       (Unguarded (ExpApplication pos [ExpVar pos tTokenMinus,var2,k]))
       noDecls]])
  where
  var2 = ExpVar pos tid2
  tid2 = mkLambdaBound (nameFromPos pos)
tPat p = error ("tPat: unknown pattern at " ++ strPos (getPos p))


-- Convert a list of expressions into a list expression (with TraceIds).
-- Needed for list literals in patterns.
mkTList :: Pos -> [Exp TraceId] -> Exp TraceId
mkTList pos = 
  foldr (\x xs -> ExpApplication pos [cons,x,xs]) (ExpCon pos tTokenNil)
  where
  cons = ExpCon pos tTokenCons

mkList :: Pos -> [Exp TokenId] -> Exp TokenId
mkList pos = 
  foldr (\x xs -> ExpApplication pos [cons,x,xs]) (ExpCon pos t_List)
  where
  cons = ExpCon pos t_Colon

-- ----------------------------------------------------------------------------
-- Transform types

tTyArgs :: [(Maybe [(Pos,TraceId)],Type TraceId)] 
        -> [(Maybe [(Pos,TokenId)],Type TokenId)]
tTyArgs = map tTyArg

tTyArg :: (Maybe [(Pos,TraceId)],Type TraceId)
       -> (Maybe [(Pos,TokenId)],Type TokenId)
tTyArg (maybePosIds,ty) = 
  (fmap (mapListSnd nameTransField) maybePosIds,wrapType (tType ty))



-- ty ==> R [[ty]]
tConstType :: Type TraceId -> Type TokenId
tConstType ty = wrapType (tType ty)


-- ty ==> RefSrcPos -> Trace -> R [[ty]]
tFunType :: Type TraceId -> Type TokenId
tFunType ty = 
  TypeCons pos tokenRefSrcPos [] `typeFun` TypeCons pos tokenRefExp [] 
    `typeFun` wrapType (tType ty)
  where
  pos = getPos ty

-- just rewrite function types:
-- t1 -> t2  ==>  Fun [[t1]] [[t2]]
tType :: Type TraceId -> Type TokenId
tType (TypeCons pos tyCon tys) =
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

tPosShares :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosShares = mapListSnd nameShare

tPosExps :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosExps = mapListSnd nameTransLetVar

tPosClss :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosClss = mapListSnd nameTransTyConCls

tPosTyVars :: [(Pos,TraceId)] -> [(Pos,TokenId)]
tPosTyVars = mapListSnd nameTransTyVar


-- ----------------------------------------------------------------------------
-- New names
-- Module names and hence all qualifications are prefixed.
-- Names of classes, type constructors and type variables remain unchanged.
-- Names of data constructors remain unchanged.
-- (everything but expression variables)
-- As prefix characters only those characters can be chosen that do
-- not start a reserved identifier or operator. Otherwise the transformation
-- might create a reserved identifier.
-- (uppercase identifiers can be prefixed by such a character, because
-- a reserved identifier will never be created by prefixing)

-- names referring to traces (or parts thereof) of program fragments:

nameTraceInfoModule :: TraceId -> TokenId
nameTraceInfoModule = prefixName 't' (error "nameTraceInfoModule: operator")

nameTraceInfoVar :: Pos -> Scope -> TraceId -> TokenId
nameTraceInfoVar pos Global = prefixName 'a' '+'
nameTraceInfoVar pos Local = prefixPosName 'a' '+' pos

nameTraceInfoGlobalVar :: TraceId -> TokenId
nameTraceInfoGlobalVar = prefixName 'a' '+'

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
nameTransField = prefixName 'b' '^'

nameTransLetVar :: TraceId -> TokenId
nameTransLetVar = prefixName 'g' '!'

nameTransLambdaVar :: TraceId -> TokenId
nameTransLambdaVar = prefixName 'f' '&'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: TraceId -> TokenId
nameWorker = prefixName 'h' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: TraceId -> TokenId
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: TraceId -> TokenId
nameShare = prefixName 's' '|'

-- variable for a trace including position
nameTraceShared :: Pos -> TraceId -> TokenId
nameTraceShared = prefixPosName 'j' '$'

-- variable for a trace
nameTrace :: TraceId -> TokenId
nameTrace = prefixName 'j' '$'

-- second variable for a trace
nameTrace2 :: TraceId -> TokenId
nameTrace2 = prefixName 'k' '@'

-- name for a local variable for a source reference
nameSR :: TraceId -> TokenId
nameSR = prefixName 'p' '%'

-- infinite list of var ids made from one id (for function clauses)
nameFuns :: TraceId -> [TokenId]
nameFuns = prefixNames 'y' '>'

-- infinite list of var ids made from one id (for naming arguments)
nameArgs :: TraceId -> [TokenId]
nameArgs = prefixNames 'z' '^'

-- a single id made from a position (different from below)
nameFromPos :: Pos -> TokenId
nameFromPos pos = mkUnqualifiedTokenId . ('v':) . showsEncodePos pos $ "n"

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
isOperatorName = not . (\c -> isAlpha c || c == '_') . head

numToSym :: String -> String
numToSym = map (("!#$%&*+^@>" !!) . digitToInt)

-- Tokens

modulePrefix = 'T'

-- apply function to unqualified name part 
-- and prefix module name (if qualified)
updateToken :: (String -> String) -> TraceId -> TokenId
updateToken f traceId | isFunTyCon traceId = 
  Qualified tracingModuleShort (packString . reverse $ "Fun")
updateToken f traceId = 
  case tokenId (traceId) of
    t | eqPredefined "[]" t -> 
      Qualified tracingModuleShort (packString . reverse . f $ "List")
    t | eqPredefined ":" t -> 
      Qualified tracingModuleShort (packString . reverse . f $ "Cons")
    t@(TupleId n) -> 
      Qualified 
        tracingModuleShort
        (packString . reverse . f $ ("Tuple"++show n)) 
    Visible n     -> 
      Visible (packString . reverse . f . unqual $ n) 
    Qualified m n -> 
      Qualified 
        (packString . reverse . updateModule . reverse . unpackPS $ m)
        (packString . reverse . f . unqual $ n) 
    _             -> error "TraceTrans: updateToken"
  where
  updateModule (name@"Main") = name -- if module is `Main', then unchanged
  updateModule name = modulePrefix : name
  unqual :: PackedString -> String
  unqual = reverse . unpackPS

--             case reverse . unpackPS $ n of -- change predefined names
--               -- "->" -> "Fun"
--               ":" -> "Cons" 
--               "[]" -> "List" -- here both type and data constructor
--               s -> s


-- ----------------------------------------------------------------------------
-- hardwired Haskell combinators and other names used by transformed modules

mkRoot :: Pos -> Exp TokenId
mkRoot pos = 
  ExpVar pos tokenMkRoot

mkSRExp :: Pos -> Bool -> Exp TokenId
mkSRExp pos traced = 
  ExpVar pos (if traced then nameTraceInfoPos pos else tokenMkNoPos)

combApply :: Pos -> Bool -> Arity -> Exp TokenId
combApply pos traced a = 
  ExpVar pos ((if traced then tokenAp else tokenUAp) a)

combFun :: Pos -> Bool -> Arity -> Exp TokenId
combFun pos traced a = ExpVar pos ((if traced then tokenFun else tokenUFun) a)

combConstUse :: Pos -> Exp TokenId
combConstUse pos = ExpVar pos tokenConstUse

combConstDef :: Pos -> Bool -> Exp TokenId 
combConstDef pos traced = 
  ExpVar pos (if traced then tokenConstDef else tokenUConstDef)

combGuard :: Pos -> Bool -> Exp TokenId
combGuard pos traced =
  ExpVar pos (if traced then tokenGuard else tokenUGuard)

combIf :: Pos -> Bool -> Exp TokenId
combIf pos traced =
  ExpVar pos (if traced then tokenIf else tokenUIf)

combCase :: Pos -> Bool -> Exp TokenId
combCase pos traced =
  ExpVar pos (if traced then tokenCase else tokenUCase)

combUpdate :: Pos -> Bool -> Arity -> Exp TokenId
combUpdate pos traced arity = 
  ExpVar pos (if traced then tokenUpdate arity else tokenUUpdate)

-- apply data constructor R
wrapExp :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
wrapExp pos ev et = ExpApplication pos [ExpCon pos tokenR,ev,et]

-- hardwired tokens:
-- constants are here just for sharing

tracingModule :: PackedString
tracingModule = packString . reverse $ "Hat" -- name of module with combinators

tracingModuleShort :: PackedString
tracingModuleShort = packString . reverse $ "T"  -- abbreviation

tPreludeModule :: PackedString
tPreludeModule = packString . reverse $ "TPreludeBasic"

mkTPreludeToken :: String -> TokenId
mkTPreludeToken s = Qualified tPreludeModule (packString . reverse $ s)

mkTracingToken :: String -> TokenId
mkTracingToken s = Qualified tracingModuleShort (packString . reverse $ s)

mkTracingTokenArity :: String -> Arity -> TokenId
mkTracingTokenArity s a = mkTracingToken (s ++ show a)

mkTypeToken :: String -> TokenId
mkTypeToken s@"fromId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toChar" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromChar" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toInt" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromInt" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toInteger" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromInteger" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toFloat" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromFloat" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toDouble" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromDouble" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s = Visible (packString . reverse $ s)


-- tokens for trace constructors:

tokenMkRoot :: TokenId
tokenMkRoot = mkTracingToken "mkRoot"

tokenR :: TokenId
tokenR = mkTracingToken "R"

tokenMkModule :: TokenId
tokenMkModule = mkTracingToken "mkModule"

tokenMkPos :: TokenId
tokenMkPos = mkTracingToken "mkSrcPos" 

tokenMkNoPos :: TokenId
tokenMkNoPos = mkTracingToken "mkNoSrcPos"

tokenMkAtomConstructor :: Bool -> TokenId
tokenMkAtomConstructor withFields = 
  mkTracingToken 
    (if withFields then "mkConstructorWFields" else "mkConstructor")

tokenMkAtomVariable :: TokenId
tokenMkAtomVariable = mkTracingToken "mkVariable"

tokenMkExpApp :: Arity -> TokenId
tokenMkExpApp = mkTracingTokenArity "mkApp"

tokenMkExpValueUse :: TokenId
tokenMkExpValueUse = mkTracingToken "mkValueUse"

tokenMkAtomRational :: TokenId
tokenMkAtomRational = mkTracingToken "mkAtomRational"

tokenMkAtomLambda :: TokenId
tokenMkAtomLambda = mkTracingToken "mkLambda"

-- tokens for expression combinators:

tokenAp :: Arity -> TokenId
tokenAp = mkTracingTokenArity "ap" 
tokenUAp :: Arity -> TokenId
tokenUAp = mkTracingTokenArity "uap"

tokenFun :: Arity -> TokenId
tokenFun = mkTracingTokenArity "fun"
tokenUFun :: Arity -> TokenId
tokenUFun = mkTracingTokenArity "ufun"

tokenCon :: Arity -> TokenId
tokenCon = mkTracingTokenArity "con"

tokenPa :: Arity -> TokenId
tokenPa = mkTracingTokenArity "pa"

tokenCn :: Arity -> TokenId
tokenCn = mkTracingTokenArity "cn"

tokenConstUse :: TokenId
tokenConstUse = mkTracingToken "constUse"

tokenConstDef :: TokenId
tokenConstDef = mkTracingToken "constDef"
tokenUConstDef :: TokenId
tokenUConstDef = mkTracingToken "uconstDef"

tokenGuard :: TokenId
tokenGuard = mkTracingToken "cguard"
tokenUGuard :: TokenId
tokenUGuard = mkTracingToken "ucguard"

tokenIf :: TokenId
tokenIf = mkTracingToken "cif"
tokenUIf :: TokenId
tokenUIf = mkTracingToken "ucif"

tokenCase :: TokenId
tokenCase = mkTracingToken "ccase"
tokenUCase :: TokenId
tokenUCase = mkTracingToken "uccase"

tokenUpdate :: Arity -> TokenId
tokenUpdate = mkTracingTokenArity "update"
tokenUUpdate :: TokenId
tokenUUpdate = mkTracingToken "uupdate"

tokenProjection :: TokenId
tokenProjection = mkTracingToken "projection"

tokenConChar :: TokenId
tokenConChar = mkTracingToken "conChar"

tokenConInteger :: TokenId
tokenConInteger = mkTracingToken "conInteger"

tokenFromLitString :: TokenId
tokenFromLitString = mkTracingToken "fromLitString"
tokenFromExpList :: TokenId
tokenFromExpList = mkTracingToken "fromExpList"

tokenWrapValClass :: TokenId
tokenWrapValClass = mkTracingToken "WrapVal"
tokenWrapValFun :: TokenId
tokenWrapValFun = mkTracingToken "wrapVal"

-- tokens of the Prelude

tokenUndefined :: TokenId
tokenUndefined = mkTPreludeToken "gundefined"

-- for integer literals
tokenFromInteger :: TokenId
tokenFromInteger = mkTPreludeToken "gfromInteger"

-- for rational literals
tokenConRational :: TokenId
tokenConRational = mkTPreludeToken ":%"
tokenFromRational :: TokenId
tokenFromRational = mkTPreludeToken "gfromRational"

-- function for pattern-match failure error message
tokenFatal :: TokenId
tokenFatal = mkTracingToken "fatal"

-- other hardcoded tokens:

tokenTraceIO :: TokenId
tokenTraceIO = mkTracingToken "traceIO"

tokenRefSrcPos :: TokenId
tokenRefSrcPos = mkTracingToken "RefSrcPos"

tokenRefExp :: TokenId
tokenRefExp = mkTracingToken "RefExp"

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
    TupleId n               -> "Tuple" ++ show n
    t | eqPredefined "[]" t -> "List"
    _                       -> getUnqualified aId

-- ----------------------------------------------------------------------------
-- various little helper functions

extractUnqual :: TokenId -> String
extractUnqual = reverse . unpackPS . extractV


eqPredefinedTrace :: String -> TraceId -> Bool
eqPredefinedTrace s id = eqPredefined s (tokenId id)

eqPredefined :: String -> TokenId -> Bool
eqPredefined s id = 
  -- a bit of a hack
  -- without qualification and even with qualification "Prelude"
  -- the token does not necessarily originate from Prelude
  -- but we pretend it does
  ((==) s . extractUnqual $ id) && 
    ((==) "Prelude" . qualModule . extractM $ id)

-- test for specific tokens

isFunTyCon :: TraceId -> Bool
isFunTyCon = eqPredefinedTrace "->"
-- (tokenId id) == t_Arrow

isTrue :: TraceId -> Bool
isTrue = eqPredefinedTrace "True"
-- (tokenId id) == tTrue

isOtherwise :: TraceId -> Bool
isOtherwise = eqPredefinedTrace "otherwise"
-- (tokenId id) == t_otherwise

isMain :: TraceId -> Bool
isMain = eqPredefinedTrace "Main"
-- (tokenId id) == tMain

-- other stuff

mkFailExp :: Pos -> Exp TokenId -> Exp TokenId
mkFailExp pos parent = ExpApplication pos [ExpVar pos tokenFatal,parent]

mkTupleExp :: Pos -> [Exp TokenId] -> Exp TokenId
mkTupleExp pos es = ExpApplication pos (ExpCon pos (t_Tuple (length es)): es)


-- ----------------------------------------------------------------------------

instance Functor Module where 
  fmap f (Module pos id mayExports imps fixs decls) =
    Module pos (f id) (fmap (map (fmap f)) mayExports) (map (fmap f) imps) 
      (map (mapDeclFixity f) fixs) (fmap f decls)

instance Functor Export where
  fmap f (ExportEntity pos entity) = ExportEntity pos (fmap f entity)
  fmap f (ExportModid pos id) = ExportModid pos (f id)

instance Functor ImpDecl where
  fmap f (Import (pos,id) impSpec) = Import (pos,f id) (fmap f impSpec)
  fmap f (ImportQ (pos,id) impSpec) = ImportQ (pos,f id) (fmap f impSpec)
  fmap f (ImportQas (pos,id) (pos2,id2) impSpec) =
    ImportQas (pos,f id) (pos2,f id2) (fmap f impSpec)
  fmap f (Importas (pos,id) (pos2,id2) impSpec) =
    Importas (pos,f id) (pos2,f id2) (fmap f impSpec)

instance Functor ImpSpec where
  fmap f (NoHiding entities) = NoHiding (map (fmap f) entities)
  fmap f (Hiding entities) = Hiding (map (fmap f) entities)

instance Functor Entity where
  fmap f (EntityVar pos id) = EntityVar pos (f id)
  fmap f (EntityConClsAll pos id) = EntityConClsAll pos (f id)
  fmap f (EntityConClsSome pos id pids) =
				EntityConClsSome pos (f id) (mapListSnd f pids)

instance Functor InfixClass where
  fmap f InfixDef = InfixDef
  fmap f InfixL = InfixL
  fmap f InfixR = InfixR
  fmap f Infix = Infix
  fmap f (InfixPre a) = InfixPre (f a)

mapDeclFixity :: (a -> b) -> FixDecl a -> FixDecl b
mapDeclFixity f (iclass,fix,fixIds) = (fmap f iclass,fix,map (fmap f) fixIds)

instance Functor FixId where
  fmap f (FixCon pos id) = FixCon pos (f id)
  fmap f (FixVar pos id) = FixVar pos (f id)

instance Functor Decls where
  fmap f (DeclsParse decls) = DeclsParse (map (fmap f) decls)
  fmap f (DeclsScc decldeps) = DeclsScc (map (fmap f) decldeps)

instance Functor DeclsDepend where
  fmap f (DeclsNoRec d) = DeclsNoRec (fmap f d)
  fmap f (DeclsRec ds) = DeclsRec (map (fmap f) ds)

instance Functor Decl where
  fmap f (DeclType simple ty) = DeclType (fmap f simple) (fmap f ty)
  fmap f (DeclTypeRenamed pos id) = DeclTypeRenamed pos id
  fmap f (DeclData sort contexts simple constrs derive) = 
    DeclData sort (map (fmap f) contexts) (fmap f simple) 
      (map (fmap f) constrs) (map (\(p,i)->(p,f i)) derive)
  fmap f (DeclDataPrim pos id size) = DeclDataPrim pos (f id) size
  fmap f (DeclConstrs pos id fields) =
    DeclConstrs pos (f id) (map (\(p,i1,i2)->(p,f i1,f i2)) fields)
  fmap f (DeclClass pos contexts cls ty decls) =
    DeclClass pos (map (fmap f) contexts) (f cls) (f ty) (fmap f decls)
  fmap f (DeclInstance pos contexts cls inst decls) =
    DeclInstance pos (map (fmap f) contexts) (f cls) (fmap f inst) 
      (fmap f decls)
  fmap f (DeclDefault tys) = DeclDefault (map (fmap f) tys)
  fmap f (DeclPrimitive pos id a ty) = DeclPrimitive pos (f id) a (fmap f ty)
  fmap f (DeclForeignImp pos callconv extfun id a fspec ty id') =
    DeclForeignImp pos callconv extfun (f id) a fspec (fmap f ty) (f id')
  fmap f (DeclVarsType vars contexts ty) =
    DeclVarsType (mapListSnd f vars) (map (fmap f) contexts) (fmap f ty)
  fmap f (DeclPat alt) = DeclPat (fmap f alt)
  fmap f (DeclFun pos id funs) = DeclFun pos (f id) (map (fmap f) funs)
  fmap f (DeclIgnore s) = DeclIgnore s
  fmap f (DeclError s) = DeclError s
  fmap f (DeclAnnot decl annots) = 
    DeclAnnot (fmap f decl) (map (fmap f) annots)
  fmap f (DeclFixity fixity) = 
    DeclFixity (mapDeclFixity f fixity)

instance Functor Annot where
  fmap f (AnnotArity (p,id) i) = AnnotArity (p,f id) i
  fmap f (AnnotPrimitive (p,id) s) = AnnotPrimitive (p, f id) s
  fmap f (AnnotNeed idss) = AnnotNeed (map (map f) idss)
  fmap f AnnotUnknown = AnnotUnknown

instance Functor Fun where
  fmap f (Fun pats rhs decls) = 
    Fun (map (fmap f) pats) (fmap f rhs) (fmap f decls)

instance Functor Alt where
  fmap f (Alt pat rhs decls) =
    Alt (fmap f pat) (fmap f rhs) (fmap f decls)

instance Functor Rhs where
  fmap f (Unguarded exp) = Unguarded (fmap f exp)
  fmap f (Guarded gdexps) = 
    Guarded (map (\(e1,e2)->(fmap f e1,fmap f e2)) gdexps)

instance Functor Type where
  fmap f (TypeCons pos id tys) = TypeCons pos (f id) (map (fmap f) tys)
  fmap f (TypeApp ty1 ty2) = TypeApp (fmap f ty1) (fmap f ty2)
  fmap f (TypeVar pos id) = TypeVar pos (f id)
  fmap f (TypeStrict pos ty) = TypeStrict pos (fmap f ty)

instance Functor Simple where
  fmap f (Simple pos id ids) = Simple pos (f id) (mapListSnd f ids)

instance Functor Context where
  fmap f (Context pos id (p,id2)) = Context pos (f id) (p,f id2)

instance Functor Constr where
  fmap f (Constr pos id fields) = Constr pos (f id) (mapFields f fields)
  fmap f (ConstrCtx vars contexts pos id fields) =
    ConstrCtx (mapListSnd f vars) (map (fmap f) contexts) pos (f id) 
      (mapFields f fields)

mapFields :: (a -> b) 
          -> [(Maybe [(Pos,a)],Type a)] -> [(Maybe [(Pos,b)],Type b)]
mapFields f = map (\(may,ty)-> (fmap (mapListSnd f) may, fmap f ty))

instance Functor Stmt where
  fmap f (StmtExp exp) = StmtExp (fmap f exp)
  fmap f (StmtBind e1 e2) = StmtBind (fmap f e1) (fmap f e2)
  fmap f (StmtLet decls) = StmtLet (fmap f decls)

instance Functor Exp where
  fmap f (ExpScc s e) = ExpScc s (fmap f e)
  fmap f (ExpDict e) = ExpDict (fmap f e)
  fmap f (ExpLambda pos pats e) = ExpLambda pos (map (fmap f) pats) (fmap f e)
  fmap f (ExpLet pos decls e) = ExpLet pos (fmap f decls) (fmap f e)
  fmap f (ExpDo pos stmts) = ExpDo pos (map (fmap f) stmts)
  fmap f (ExpCase pos e alts) = ExpCase pos (fmap f e) (map (fmap f) alts)
  fmap f (ExpFatbar e1 e2) = ExpFatbar (fmap f e1) (fmap f e2)
  fmap f ExpFail = ExpFail
  fmap f (ExpIf pos e1 e2 e3) = ExpIf pos (fmap f e1) (fmap f e2) (fmap f e3)
  fmap f (ExpType pos e contexts ty) = 
    ExpType pos (fmap f e) (map (fmap f) contexts) (fmap f ty)
  fmap f (ExpRecord e fields) = ExpRecord (fmap f e) (map (fmap f) fields)
  fmap f (ExpApplication pos es) = ExpApplication pos (map (fmap f) es)
  fmap f (ExpVar pos id) = ExpVar pos (f id)
  fmap f (ExpCon pos id) = ExpCon pos (f id)
  fmap f (ExpInfixList pos es) = ExpInfixList pos (map (fmap f) es)
  fmap f (ExpVarOp pos id) = ExpVarOp pos (f id)
  fmap f (ExpConOp pos id) = ExpConOp pos (f id)
  fmap f (ExpLit pos lit) = ExpLit pos lit
  fmap f (ExpList pos es) = ExpList pos (map (fmap f) es)
  fmap f (Exp2 pos id1 id2) = Exp2 pos (f id1) (f id2)
  fmap f (PatAs pos id pat) = PatAs pos (f id) (fmap f pat)
  fmap f (PatWildcard pos) = PatWildcard pos
  fmap f (PatIrrefutable pos pat) = PatIrrefutable pos (fmap f pat)
  fmap f (PatNplusK pos id1 id2 e1 e2 e3) =
    PatNplusK pos (f id1) (f id2) (fmap f e1) (fmap f e2) (fmap f e3)

instance Functor Field where
  fmap f (FieldExp pos id e) = FieldExp pos (f id) (fmap f e)
  fmap f (FieldPun pos id) = FieldPun pos (f id)

-- ----------------------------------------------------------------------------
-- End








