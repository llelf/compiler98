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
               ,getUnqualified
               ,tTokenCons,tTokenNil,tTokenGtGt,tTokenGtGtEq,tTokenFail
               ,tTokenAndAnd,tTokenEqualEqual,tTokenGreaterEqual,tTokenMinus)
import AuxFile (AuxiliaryInfo) -- needed only for hbc's broken import mechanism
import List (isPrefixOf,union,partition,nubBy)
import Char (isAlpha,digitToInt)
import Ratio (numerator,denominator)

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
    (tImpDecls modId impDecls)
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
  (DeclsParse decls',consts) = tDecls traced (mkRoot pos traced) decls
  

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
        ,ExpVar noPos tokengmain]))
      noDecls]
  where
  tokenmain = visible (reverse "main")
  tokengmain = nameTransVar (mkLambdaBound tmain)

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
tEntity (EntityVar pos id) = [EntityVar pos (nameTransVar id)]
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
    [EntityConClsSome pos (nameTransTyConCls id) (mapListSnd nameTransVar posIds)]
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
    unzip3 (map (tDecl2 traced parent) . combineFuns $ decls)

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
    tCaf traced parent pos id rhs localDecls
tDecl2 traced parent decl = tDecl traced parent decl


singleDecl :: Decl id -> ([Decl id],[a],ModuleConsts)
singleDecl decl = ([decl],[],emptyModuleConsts)

tDecl :: Bool -> Exp TokenId -> Decl TraceId 
      -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tDecl _ _ (DeclType lhsTy rhsTy) = 
  singleDecl $ DeclType (tSimple lhsTy) (tType rhsTy)
tDecl _ _ (DeclData sort contexts lhsTy constrs pClss) = 
  ([DeclData sort (tContexts contexts) (tSimple lhsTy) 
    (map tConstr constrs) []] 
    -- "derive" should be empty, because transformed classes cannot be derived
  ,instDecl:fieldSelectorDecls++deriveDecls
  ,foldr (uncurry addCon) (fieldSelectorConsts `merge` deriveConsts)
     (map getCon constrs))
  where
  (DeclsParse deriveDecls,deriveConsts) = 
     tDecls False (mkRoot noPos False) 
       (DeclsParse (derive contexts lhsTy constrs pClss))
  instDecl = wrapValInstDecl (getPos lhsTy) contexts lhsTy constrs
  (fieldSelectorDecls,fieldSelectorConsts) = mkFieldSelectors constrs
  getCon (Constr pos id _) = (pos,id)
  getCon (ConstrCtx _ _ pos id _) = (pos,id)
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
  -- Variables of arity 0 do not take SR and Trace argument, so that
  -- their values are shared. Note that type signatures for class methods
  -- are handled differently by tDecl2
  ((if null constVars then [] 
      else [DeclVarsType (tPosExps constVars) 
             (tContexts contexts) (wrapType (tType ty))])
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
  -- first rewrite as p = e, then
  -- xi = lazySat (case patId of (t,y1,..,yn) -> indir t yi) xti
  -- patId = case e' of 
  --           p' -> (t,y1,..,yn)
  --           _  -> fail noPos parent
  -- xti = mkConstVar p pos "xi"
  (map projDef patPosIds
  ,DeclFun noPos patId 
    [Fun [] 
      (Unguarded 
        (ExpCase noPos exp'
          [Alt pat'' (Unguarded tuple) noDecls
          ,Alt (PatWildcard noPos) (Unguarded (mkFailExp noPos parent)) noDecls
          ]))
      decls']
   :map (uncurry (mkConstDecl parent)) patPosIds
  ,foldr (uncurry addVar) 
    (emptyModuleConsts `withLocal` altConsts) patPosIds)
  where
  firstId = snd . head $ patPosIds
  patId = nameShare noPos firstId
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

  mkConstDecl :: Exp TokenId -> Pos -> TraceId -> Decl TokenId
  mkConstDecl parent pos id =
    DeclFun pos (nameTraceShared pos id)
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
          ,ExpVar pos (nameTraceShared pos id)]))
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
  tFuns traced pos id funs  -- a function does not use the static parent
tDecl _ _ (DeclFixity _) = ([],[],emptyModuleConsts) 
  -- fixity declarations have been processed before 
  -- not needed in output, because pretty printer produces unambiguous output
tDecl _ _ (DeclIgnore s) = ([DeclIgnore s],[],emptyModuleConsts)
tDecl _ _ _ = error "tDecl: unknown sort of declaration"


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
      if numOfArgs == 0 then consNm
        else ExpApplication pos .
               (ExpVar pos (tokenMkTAp numOfArgs) :) . (parent :) .
               (consNm :) . (++ [sr]) $ traces 
    consNm = 
      ExpApplication pos 
        [ExpVar pos tokenMkConst
        ,parent,ExpVar pos (nameTraceInfoCon consId),sr]
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
    ([DeclVarsType [(pos,nameTransVar fnId)] [] (wrapType (tType ty))
     ,DeclFun pos (nameTransVar fnId)
       [Fun []
         (Unguarded 
           (ExpApplication pos 
             [combSat pos False False
             ,ExpApplication pos 
               [expFrom pos ty,mkRoot pos False, ExpVar pos hasId]
	     ,useParent])) 
	 noDecls]]
    ,[DeclFun pos useParentId
       [Fun [] (Unguarded 
         (mkConstVar (mkRoot pos False) pos fnId False)) noDecls]]
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
--   hname p (R v _) = indir p (zname v)
mkFieldSelector :: Pos -> TraceId -> (Decl TokenId,ModuleConsts)
mkFieldSelector pos fieldId =
  (DeclFun pos (nameTransVar fieldId) 
    [Fun [sr,parent]
      (Unguarded
        (ExpApplication pos
          [combFun pos False 1
          ,ExpVar pos (nameTraceInfoVar pos fieldId)
          ,ExpVar pos wrappedId',sr,parent]))
      (DeclsParse 
        [DeclFun pos wrappedId' 
          [Fun [parent,wrapExp pos var (PatWildcard pos)] 
            (Unguarded 
              (ExpApplication pos 
                [ExpVar pos tokenIndir,parent
                ,ExpApplication pos [ExpVar pos (nameTransField fieldId),var]
                ])) 
            noDecls]])]
  ,addVar pos fieldId emptyModuleConsts)
  where
  sr = ExpVar pos (nameSR fieldId)
  parent = ExpVar pos (nameTrace fieldId)
  wrappedId' = nameWorker fieldId
  var = ExpVar pos varId
  varId:_ = nameArgs fieldId


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
  useParentId = nameTraceShared pos id
  (rhs',rhsConsts) = tRhs traced True useParent failContinuation rhs
  (localDecls',localDeclsConsts) = tDecls traced useParent localDecls


tFuns :: Bool -> Pos -> TraceId -> [Fun TraceId]
     -> ([Decl TokenId],[Decl TokenId],ModuleConsts)

tFuns traced pos id funs =
  ([DeclFun pos (nameTransVar id) 
     [Fun [sr,parent]
       (Unguarded
         (ExpApplication pos
           [combFun pos traced funArity
           ,ExpVar pos (nameTraceInfoVar pos id)
           ,ExpVar pos wrappedId',sr,parent]))
       (DeclsParse (DeclFun pos wrappedId' funs' : newDecls'))]]
       -- worker must be local, because if wrapper is within an instance,
       -- then worker cannot be in instance scope; neither can it be on
       -- top level scope, because there may be several instances for the
       -- same class and all workers have the same name.
       -- In same scope as possible type decl (which hasn't been implemented)
  ,[]
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
tFunClauses traced pos parent ids pVars funArity _ (fun@(Fun pats _ _) : funs)
  | not (null funs) && funCanFail fun =
    ([Fun (parent : pats'') rhs' decls'
     ,Fun (parent : vars) 
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
  (Fun (parent:pats') rhs' decls' : funs'
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
      (Fun pats' (Unguarded 
        (ExpCase pos cond
          [Alt (wrapExp pos guardValue guardTrace)
            (Unguarded
              (ExpIf pos guardValue rhs' 
                (continuationToExp newParent contExp)))
            (DeclsParse 
              (DeclFun pos newParentId
                [Fun [] (Unguarded (mkConstGuard pos parent guardTrace traced))
                  noDecls]
              :patsDecls'))]))
        decls'
      ,pos `addPos` condConsts `merge` patsDeclsConsts `merge` declsConsts 
       `withLocal` rhsConsts)  
      -- condConsts contains positions of the boolean expressions
      -- patsDeclsConsts contains positions of the bound variables
  where
  pos = getPos pats
  guardValue = ExpVar pos guardValueId
  guardTrace = ExpVar pos guardTraceId
  newParent = ExpVar pos newParentId
  (newParentId:guardValueId:guardTraceId:_) = namesFromPos pos
  (pats',conditions,patsDecls) = tPats pats
  (DeclsParse patsDecls',patsDeclsConsts) = 
    tDecls traced newParent (DeclsParse patsDecls) 
  (cond,condConsts) = tExp traced False parent (foldr1 andExp conditions)
  (rhs',rhsConsts) = 
    tRhs traced cr (if null conditions then parent else newParent) contExp rhs
  (decls',declsConsts) = tDecls traced parent decls
  andExp :: Exp TraceId -> Exp TraceId -> Exp TraceId
  andExp e1 e2 = ExpApplication pos [ExpVar pos tTokenAndAnd,e1,e2]


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
  ,pos `addPos` eConsts `merge` funConsts)
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
  (ExpType pos e' (tContexts contexts) (wrapType (tType ty))
  ,eConsts)
  where
  (e',eConsts) = tExp traced cr parent e
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
  tConApp traced parent e []
tExp traced cr parent (ExpLit pos litstr@(LitString _ s)) =
  -- the result is very large; should use special wrapper that
  -- transforms string in traced string instead
--  tExp traced cr parent (ExpList pos (map (ExpLit pos . LitChar Boxed) s))
  (ExpApplication pos
     [ExpVar pos tokenFromLitString,sr,parent,ExpLit pos litstr]
  ,pos `addPos` emptyModuleConsts)
  where
  sr = mkSRExp pos traced
tExp traced cr parent (ExpLit pos lit@(LitChar _ _)) =
  (ExpApplication pos 
    [ExpVar pos tokenConChar,mkSRExp pos traced,parent,ExpLit pos lit]
  ,pos `addPos` emptyModuleConsts)
tExp traced cr parent (ExpLit pos lit@(LitRational b r)) =
  -- desugar rational constant into explicit use of ":%",
  -- because Rational is not a primitive type but defined in PreludeBasic
  -- however, this way mkNTRational is not used at all
  (ExpApplication pos 
    [combApply pos False False 1,sr,parent
    ,ExpApplication pos [ExpVar pos tokenFromRational,sr,parent]
    ,ExpApplication pos 
      [ExpCon pos tokenR
      ,ExpApplication pos 
        [ExpCon pos tokenConRational
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,num]
        ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,denom]]
      ,mkConst parent 
        (ExpApplication pos 
          [ExpVar pos tokenMkAtomRational,ExpLit pos lit]) 
        pos traced]]
  ,pos `addPos` emptyModuleConsts)
  where
  num = ExpLit pos (LitInteger b (numerator r))
  denom = ExpLit pos (LitInteger b (denominator r))
  sr = mkSRExp pos traced
      
--  tExp traced cr parent 
--    (ExpApplication pos 
--      [ExpCon pos tokenConRational
--      ,ExpLit pos (LitInteger b (numerator r))
--      ,ExpLit pos (LitInteger b (denumerator r))
--      ])
tExp traced cr parent (ExpLit pos lit@(LitInteger _ _)) =
  (ExpApplication pos 
    [combApply pos False False 1,sr
    ,ExpApplication pos [ExpVar pos tokenMkTHidden,parent]
    ,ExpApplication pos [ExpVar pos tokenFromInteger,sr,parent]
    ,ExpApplication pos [ExpVar pos tokenConInteger,sr,parent,ExpLit pos lit]]
  ,pos `addPos` emptyModuleConsts)
  where
  sr = mkSRExp pos traced
tExp traced cr parent (ExpList pos es) =
  -- the result is very large; should use special wrapper that
  -- transforms list in traced list instead
--  tExp traced cr parent . mkTList pos $ es
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
             take consArity . repeat $ ExpVar pos tokenUndefined
  Just consArity = arity consId
  sr = mkSRExp pos traced
  (fields',fieldsConsts) = mapMerge2 (tField traced parent) fields
tExp traced cr parent (ExpRecord exp fields) = -- update
  (ExpApplication pos
    [combUpdate pos traced,sr,parent,exp'
    ,ExpLambda pos [var] (ExpRecord var fields')]
  ,pos `addPos` expConsts `merge` fieldsConsts)
  where
  var = ExpVar pos (nameFromPos pos)
  sr = mkSRExp pos traced
  pos = getPos fields
  (exp',expConsts) = tExp traced False parent exp
  (fields',fieldsConsts) = mapMerge2 (tField traced parent) fields
tExp _ _ _ _ = error "tExp: unknown sort of expression"


tField :: Bool -> Exp TokenId -> Field TraceId -> (Field TokenId,ModuleConsts)
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
tPat (ExpVar pos id) = (ExpVar pos (nameTransVar id),[],[])
tPat (ExpCon pos id) = 
  (wrapExp pos (ExpCon pos (nameTransCon id)) (PatWildcard pos),[],[])
tPat (ExpLit pos (LitString _ s)) =
  tPat . mkTList pos . map (ExpLit pos . LitChar Boxed) $ s
tPat (ExpLit pos lit@(LitChar _ _)) = 
  (wrapExp pos (ExpLit pos lit) (PatWildcard pos),[],[]) 
tPat e@(ExpLit pos lit) = -- only LitInteger and LitRational left
  (ExpVar pos (nameTransVar tid)
  ,[ExpApplication pos 
     [ExpVar pos tTokenEqualEqual,ExpVar pos tid,e]]
  ,[])
  where
  tid = mkLambdaBound (nameFromPos pos)
tPat (ExpList pos pats) = tPat . mkTList pos $ pats
tPat (PatAs pos id pat) = (PatAs pos (nameTransVar id) pat',patExps,patDecls)
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
  (ExpVar pos (nameTransVar tid2)
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
  (fmap (mapListSnd nameTransField) maybePosIds,wrapType (tType ty))


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
tPosExps = mapListSnd nameTransVar

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
nameTransField = prefixName 'b' '^'

nameTransVar :: TraceId -> TokenId
nameTransVar = prefixName 'g' '!'

-- internal, local names

-- refering to partially transformed expression
nameWorker :: TraceId -> TokenId
nameWorker = prefixName 'h' '*'

-- refering to original (unwrapped) foreign import
nameForeign :: TraceId -> TokenId
nameForeign = prefixName 'f' '&'

-- names for new variables in transformed expressions:
-- variable for sharing in transformation of pattern binding
nameShare :: Pos -> TraceId -> TokenId
nameShare = prefixPosName 's' '|'

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
--        (if n <= 2 then tracingModuleShort else transPreludeModule) 
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
  unqual = reverse . unpackPS

--             case reverse . unpackPS $ n of -- change predefined names
--               -- "->" -> "Fun"
--               ":" -> "Cons" 
--               "[]" -> "List" -- here both type and data constructor
--               s -> s


-- ----------------------------------------------------------------------------
-- hardwired Haskell combinators and other names used by transformed modules

mkRoot :: Pos -> Bool {- traced? = not hidden -} -> Exp TokenId
mkRoot pos traced = 
  ExpVar pos (if traced then tokenMkTRoot else tokenHiddenRoot)

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

combUpdate :: Pos -> Bool -> Exp TokenId
combUpdate pos traced = 
  ExpVar pos (if traced then tokenUpdate else tokenUUpdate)

-- apply data constructor R
wrapExp :: Pos -> Exp TokenId -> Exp TokenId -> Exp TokenId
wrapExp pos ev et = ExpApplication pos [ExpCon pos tokenR,ev,et]

-- hardwired tokens:

tracingModule :: PackedString
tracingModule = packString . reverse $ "Hat" -- name of module with combinators

tracingModuleShort :: PackedString
tracingModuleShort = packString . reverse $ "T"  -- abbreviation

tPreludeModule :: PackedString
tPreludeModule = packString . reverse $ "TPrelude"

mkTPreludeToken :: String -> TokenId
mkTPreludeToken s = Qualified tPreludeModule (packString . reverse $ s)

mkTracingToken :: String -> TokenId
mkTracingToken s = Qualified tracingModuleShort (packString . reverse $ s)

mkTracingTokenArity :: String -> Arity -> TokenId
mkTracingTokenArity s a = mkTracingToken (s ++ show a)

typeModule :: PackedString
typeModule = packString . reverse $ "TPreludeBuiltinTypes" 

mkTypeToken :: String -> TokenId
mkTypeToken s@"fromId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toId" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toIO" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple0" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"fromTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s@"toTuple2" = Qualified tracingModuleShort (packString . reverse $ s)
mkTypeToken s = Qualified typeModule (packString . reverse $ s)


-- tokens for trace constructors:

tokenMkTRoot :: TokenId
tokenMkTRoot = mkTracingToken "mkTRoot"

tokenHiddenRoot :: TokenId
tokenHiddenRoot = mkTracingToken "hiddenRoot"

tokenMkTHidden :: TokenId
tokenMkTHidden = mkTracingToken "mkTHidden"

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

tokenMkAtomRational :: TokenId
tokenMkAtomRational = mkTracingToken "mkNTRational"

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

tokenUpdate :: TokenId
tokenUpdate = mkTracingToken "update"
tokenUUpdate :: TokenId
tokenUUpdate = mkTracingToken "uupdate"

tokenIndir :: TokenId
tokenIndir = mkTracingToken "indir"

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
