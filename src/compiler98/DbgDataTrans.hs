{- ---------------------------------------------------------------------------
Transforms all type definitions and type annotations of a program 
for producing traces for debugging.
-}
module DbgDataTrans(dbgDataTrans) where

import Tree234
import Extra(trace,noPos,pair,snub,mixSpace,assocDef)
import IdKind
import TokenId
import DbgId
import IntState
import Syntax
import SyntaxPos(Pos,HasPos(getPos))
import StrSyntax(StrId,strType,strContexts,strVarsType,strSimple)
import Flags(Flags(sDbgTrans,sTraceFns))
import State
import NT
import Nice(niceCtxs, niceNT, mkAL)
import AssocTree
import PackedString(PackedString, unpackPS, packString)
import Id(Id)

data Inherited = Inherited 
                   ((TokenId, IdKind) -> Id) -- lookupPrel?
                   [(Int, Int)] 
                   Int 
                   (Tree (Int, Int)) -- reptree?
                   Bool              -- True if more debugging output
data Threaded = Threaded 
                   IntState    -- internal compiler state
                   [(Pos, Id)] -- defined data constructors

type DbgDataTransMonad a = State Inherited Threaded a Threaded


dbgDataTrans :: Flags                -- compiler flags (to test if debugging)
             -> IntState             -- internal compiler state
             -> Tree (Int,Int)       -- reptree?
             -> ((TokenId,IdKind) -> Id) -- lookupPrel?
             -> Decls Id         -- input declarations
             -> (Decls Id        -- modified declarations
                ,IntState            -- modified internal state
                ,Maybe [(Pos,Id)])   -- defined data constructors, 
                                     -- if transformation performed
                                     -- for SRIDTable

dbgDataTrans flags state reptree lookupPrel dptopdecls =
  if (sDbgTrans flags) 
    then
      case dTopDecls dptopdecls
	     (Inherited lookupPrel [] 0 reptree (sTraceFns flags)) 
             (Threaded state []) of
        (decls', Threaded state' constrs) -> 
          (decls', state', Just constrs)
    else
      (dptopdecls, state, Nothing)


dTopDecls :: Decls Id -> DbgDataTransMonad (Decls Id)

dTopDecls (DeclsParse ds) = 
    getArities ds >=>
    unitS DeclsParse =>>> 
    (mapS dTopDecl ds >>>= \dss -> unitS (concat dss))


dTopDecl :: Decl Int -> DbgDataTransMonad [Decl Id]

dTopDecl d@(DeclTypeRenamed pos id) = 
  lookupName pos id >>>= \(Just (InfoData _ _ _ nt _)) ->
  dNewType nt >>>= \nt' ->
  showNT nt >>>= \ntstr ->
  showNT nt' >>>= \ntpstr ->
  dTrace ("Type syn  " ++ ntstr ++ " changed to " ++ ntpstr) $
  updateSynType id nt' >>>
  unitS [d]

{-
--dTopDecl d@(DeclType (Simple tid _ _) t) = 
-- type synonym definitions are removed earlier by the compiler
-- and replaced by the following annotation
--   not anymore! so the following equation is probably superfluous
dTopDecl d@(DeclAnnot (DeclIgnore _) [AnnotArity (_, tid) _]) =
    lookupName noPos tid >>>= \(Just (InfoData _ _ _ nt _)) ->
    dNewType nt >>>= \nt' ->
    showNT nt >>>= \ntstr ->
    showNT nt' >>>= \ntpstr ->
    dTrace ("Type syn  " ++ ntstr ++ " changed to " ++ ntpstr) $
{-    
    dCtxType noPos [] t >>>= \(_, t') ->
    showTheType t >>>= \st1 ->
    showTheType t' >>>= \st2 ->
    showSimple tid >>>= \ssimple ->
    dTrace ("Type syn:\n" ++ ssimple ++ " = " ++ st1 ++ "\nchanged to:\n" 
            ++ ssimple ++ " = " ++ st2) $
-}
    updateSynType tid nt' >>>
    unitS [DeclIgnore "Type Synonym"]
-}

{- supperfluous, because DeclData is replaced by DeclConstrs by rename
dTopDecl (DeclData mb ctx simple constrs tycls) =
  dTrace ("DbgDataTrans.dTopDecl.DeclData") $
  unitS (:[])  
  =>>> (unitS (DeclData mb ctx) -- =>>> addCtx ctx simple 
        =>>> unitS simple 
        =>>> mapS dConstr constrs =>>> unitS tycls)
-}
dTopDecl d@(DeclConstrs pos id constrids) = 
  dTrace ("DbgDataTrans.dTopDecl.DeclConstrs" ++ show constrids) $
  lookupName noPos id >>>= \(Just idinfo) ->
    case idinfo of
      InfoData did tid ie nt dk ->
	dTrace ("InfoData: " ++ show tid) $
	case dk of
	  Data b constrs ->
	    mapS0 (addConstr pos) constrs >>>
	    mapS0 transformConstr constrs >>>
	    unitS [d]
	  DataNewType b constrs ->
	    mapS0 (addConstr pos) constrs >>>
	    mapS0 transformConstr constrs >>>
	    unitS [d]
	  _ -> error ("dk = " ++ show dk)
      _ -> error ("idinfo = " ++ show idinfo)
  --lookupNameStr id >>>= \idstr ->
  --error ("dDecl: DeclConstrs " ++ show pos ++ " " ++ idstr ++ " " 
  --       ++ show constrids ++ "\n" ++ show idinfo)
    where transformConstr constr =
              lookupName noPos constr >>>= \(Just info) ->
	          case info of
		      InfoConstr cid tid fix nt annot ty ->
		          dNewType nt >>>= \nt' ->
			  wrapRNewType nt' >>>= \nt'' ->
			  showNT nt'' >>>= \ntstr ->
		          dTrace ("InfoConstr: " ++ show tid ++ 
                                  " has new type " ++ ntstr) $
			  updateConstrType constr nt''
		      _ -> error ("info = " ++ show info)     
dTopDecl d@(DeclClass pos ctx id1 id2 decls) =
  -- Try to get class method types
  lookupName noPos id1 >>>= \clsinfo@(Just (InfoClass i tid ie nt ms ds at)) ->
  setClassVar id2 >=>
  mapS transformMethodType (zip ms ds) >>>= \hol ->    
  dNewType nt >>>= \nt' ->
  showNT nt' >>>= \ntstr ->
  dTrace ("*** Class type: " ++ ntstr ++ " ho: " ++ show (or hol)) $
  updateClassType i tid ie nt' ms ds at >>>
  dDecls decls >>>= \decls' ->
  unitS [DeclClass pos ctx id1 id2 decls']
  where 
  transformMethodType (m, d) =
    lookupName noPos m >>>= \(Just (InfoMethod im _ _ _ _ _)) ->
    lookupName noPos d >>>= \(Just (InfoDMethod id tid nt' (Just arity) _)) ->
    lookupNameStr id >>>= \mstr ->
    if doTransform tid 
      then
	showNT nt' >>>= \ntstr' ->
	dTrace ("Type for " ++ mstr ++ " : " ++ ntstr') $
	dMethodNewType (arity == 0) nt' >>>= \nt'' -> 
	updateMethodType im id nt'' >>>
	lookupName noPos m >>>= \(Just (InfoMethod _ _ _ nt''' _ _)) ->
	showNT nt''' >>>= \ntstr'' ->
	dTrace ("Transformed into " ++ ntstr'') $
	unitS (isHigherOrder id2 nt')
      else
	unitS (isHigherOrder id2 nt')
  -- Ignore method starting with '_'
  doTransform = ('_'/=) . last . unpackPS . extractV
dTopDecl (DeclInstance pos ctx id inst decls) = 
    dCtxType pos ctx inst >>>= \(_, inst') -> -- Don't change the context!!!
    dDecls decls >>>= \decls' ->
    unitS [DeclInstance pos ctx id inst' decls']
{- supperfluous, because DeclDataPrim is replaced by DeclConstrs by rename
dTopDecl (DeclDataPrim pos id size) = 
    lookupNameStr id >>>= \idstr ->
    error ("Cannot yet deal with primitive datatypes (" ++ idstr 
           ++ ", size=" ++ show size ++ ")")
-}
dTopDecl d = dDecl d


dDecls :: Decls Id -> DbgDataTransMonad (Decls Id)

dDecls (DeclsParse ds) = 
    getArities ds >=>
    unitS DeclsParse =>>> (mapS dDecl ds >>>= \dss -> unitS (concat dss))


dDecl :: Decl Id -> DbgDataTransMonad [Decl Id]

dDecl d@(DeclDefault tys) = unitS [d]
dDecl d@(DeclVarsType vars ctx ty) = 
  mapS lookupNameStr (map snd vars) >>>= \ids ->
  if "main" `elem` ids 
    then
      unitS [d]  -- Don't change the type of 'main'
    else
      dCtxType noPos ctx ty >>>= \(ctx', ty') ->
      wrapRT noPos ty' >>>= \ty'' ->
      showVarsType d >>>= \svt1 ->
--    showVarsType (DeclVarsType vars ctx' ty'') >>>= \svt2 ->
      let checkForCAF (pos, id) =
            getArity id >>>= \arity ->
            if False {-arity == 0-} {- assumes id is not a caf, wrong? -}
              then
       	        showVarsType (DeclVarsType vars ctx' ty'') >>>= \svt2 ->
                dTrace ("Signature:\n" ++ svt1 ++ "\nchanged to:\n" ++ svt2) $
                  unitS (DeclVarsType [(pos, id)] ctx' ty'')
	      else
	        addD ty'' >>>= \ty''' -> 
		addSR ty''' >>>= \ty'''' ->
		showVarsType (DeclVarsType vars ctx' ty'''') >>>= \svt2 ->
                dTrace ("Signature:\n" ++ svt1 ++ "\nchanged to:\n" ++ svt2) $
	          unitS (DeclVarsType [(pos, id)] ctx' ty'''')
      in  mapS checkForCAF vars
dDecl (DeclPat (Alt pat gdes decls)) = 
  unitS ((:[]) . DeclPat) =>>> 
  (unitS (Alt pat) =>>> mapS dGdEs gdes =>>> dDecls decls)
dDecl d@(DeclFun pos id fundefs) = 
  unitS ((:[]) . DeclFun pos id) =>>> mapS dFunClause fundefs
dDecl d@(DeclIgnore _) = unitS [d]
dDecl d@(DeclError _) = unitS [d]
dDecl d@(DeclAnnot _ _) = unitS [d]
dDecl d@(DeclFixity _) = unitS [d]
dDecl d@(DeclPrimitive pos id i ty1) =
    lookupNameStr id >>>= \idstr ->
    -- A hack to be able to have primitives with untransformed types
    if all (uncurry (==)) (zip "._tprim_" (snd (break ('.'==) idstr))) then
	unitS [d]
    else
        dType ty1 >>>= \ty2 ->    
	wrapRT pos ty2 >>>= \ty3 ->
	addD ty3 >>>= \ty4 -> 
	addSR ty4 >>>= \ty5 ->
	unitS [DeclPrimitive pos id 2 ty5]
--    lookupNameStr id >>>= \idstr ->
--    error ("dDecl: DeclPrimitive " ++ show pos ++ " " ++ idstr 
--           ++ " " ++ show i)
dDecl d@(DeclForeignImp pos cname id ar cast ty1 _) =
    lookupName noPos id >>>= \(Just info) ->
    addNewPrim info >>>= \id' ->	-- copy original prim to new location
    overwritePrim id >>>		-- write wrapper info over original
    dType ty1 >>>= \ty2 ->		-- calculate type of wrapper
    wrapRT pos ty2 >>>= \ty3 ->
    addD ty3 >>>= \ty4 -> 
    addSR ty4 >>>= \ty5 ->
    unitS [ DeclForeignImp pos cname id' ar cast ty1 id
          , DeclVarsType [(pos,id)] [] ty5]
--  unitS [d]
dDecl d@(DeclForeignExp _ _ _ _) = unitS [d]
dDecl x = error "Hmmm. No match in dbgDataTrans.dDecl"


dFunClause :: Fun Id -> DbgDataTransMonad (Fun Id)

dFunClause (Fun ps gdses decls) = 
    unitS (Fun ps) =>>> mapS dGdEs gdses =>>> dDecls decls


dGdEs :: (Exp Id,Exp Id) -> DbgDataTransMonad (Exp Id,Exp Id)

dGdEs (gd, e) = unitS pair =>>> dExp gd =>>> dExp e


dExps :: [Exp Id] -> DbgDataTransMonad [Exp Id]

dExps es = mapS dExp es


dExp :: Exp Id -> DbgDataTransMonad (Exp Id)

dExp (ExpLambda pos pats e) = 
  unitS (ExpLambda pos pats) =>>> dExp e
dExp (ExpLet pos decls e) = 
  unitS (ExpLet pos) =>>> dDecls decls =>>> dExp e
dExp (ExpCase pos e alts) = 
  unitS (ExpCase pos) =>>> dExp e =>>> mapS dAlt alts
dExp (ExpIf pos c e1 e2) = 
  unitS (ExpIf pos) =>>> dExp c =>>> dExp e1 =>>> dExp e2
dExp (ExpType pos e ctx t) =   
    dCtxType pos ctx t >>>= \(ctx', t') ->
    wrapRT pos t' >>>= \t'' ->
    dExp e >>>= \e' ->
    showTheType t'' >>>= \st ->
    --trace (show e' ++ " has type " ++ show ctx' ++ " => " ++ st) $
    unitS (ExpType pos e' ctx' t'')
dExp (ExpApplication pos es) = 
  unitS (ExpApplication pos) =>>> dExps es
dExp (ExpList pos es) = 
  unitS (ExpList pos) =>>> dExps es
dExp (ExpVar pos id) = 
  unitS (ExpVar pos id)
dExp (ExpDo pos stmts) = 
  dRemoveDo pos stmts
dExp (ExpRecord _ _) = 
  error "ExpRecord not supported when debugging"
dExp (ExpScc s e) = -- never used in compiler
  error "ExpScc not supported when debugging"
dExp (ExpFatbar _ _) =  -- never used in compiler
  error "ExpFatbar not supported when debugging"
dExp ExpFail = -- never used in compiler
  error "ExpFail not supported when debugging"
dExp (ExpInfixList _ _) = -- doesn't exist here anymore
  error "ExpInfixList not supported when debugging"
dExp (ExpVarOp _ _) =  -- doesn't exist here anymore
  error "ExpVarOp not supported when debugging"
dExp (ExpConOp _ _) = -- doesn't exist here anymore
  error "ExpConOp not supported when debugging"
dExp e = unitS e


dRemoveDo :: a -> [Stmt Id] -> DbgDataTransMonad (Exp Id)

dRemoveDo p [StmtExp exp] = dExp exp
dRemoveDo p (StmtExp exp:r) =
  let pos = getPos exp
  in 
    lookupId Var t_gtgt >>>= \ gtgt ->
    dExp exp >>>= \exp' ->
    dRemoveDo p r >>>= \ exp2 ->
    unitS (ExpApplication pos [ExpVar pos gtgt, exp', exp2])
dRemoveDo p (StmtLet decls :r) =
  let pos = getPos decls
  in 
    dDecls decls >>>= \decls' ->
    dRemoveDo p r >>>= \ exp2 ->
    unitS (ExpLet pos decls' exp2)
dRemoveDo p (StmtBind pat exp:r) =
  lookupId Var t_gtgteq >>>= \ gtgteq ->
  getState >>>= \ state ->
  dExp exp >>>= \exp' ->
  dRemoveDo p r >>>= \ exp2 ->
  let pos = getPos exp'
  in
    if nofail state pat
    then  
      unitS (ExpApplication pos 
              [ExpVar pos gtgteq, exp', ExpLambda pos [pat] exp2])
    else
      lookupId Var t_zero >>>= \ zero ->
      lookupId Con tTrue >>>= \ true ->
      newVar pos >>>= \ x ->
      let eTrue = ExpCon pos true
      in unitS (ExpApplication pos 
                  [ExpVar pos gtgteq
		  ,exp'
		  ,ExpLambda pos [x] (ExpCase pos x 
                    [Alt pat [(eTrue,exp2)] (DeclsScc [])
		    ,Alt (PatWildcard pos) [(eTrue,ExpVar pos zero)] 
                       (DeclsScc [])
		    ])
                  ])


{-
Will matching with the pattern given as second argument never fail?
(eg. single constructor, irrefutable pattern)
-}
nofail :: IntState -> Exp Id -> Bool

nofail state (ExpCon pos con) =
  case lookupIS state con of
    Just (InfoConstr unique tid fix nt fields iType) ->
      case lookupIS state iType of
	Just (InfoData unique tid exp nt dk) ->
	  case dk of
	    (DataNewType unboxed constructors) -> True
	    (Data unboxed  constrs) -> length constrs == 1
nofail state (ExpVar _ _) = True
nofail state (ExpApplication pos es) = all (nofail state) es
nofail state (PatWildcard _) = True
nofail state (PatAs _ _ pat) = nofail state pat
nofail state (PatIrrefutable pos pat) = True
nofail state _ = False


dAlt :: Alt Id -> DbgDataTransMonad (Alt Id)

dAlt (Alt pat gdexps decls) = 
    unitS (Alt pat) =>>> mapS dGdEs gdexps =>>> dDecls decls

{- ---------------------------------------------------------------------------
Type translating functions
-}

{-
Translate a type. Only modifies all embedded funtion types:
  t1 -> t2  ==> Trace -> (R t1 -> R t2)
-}
dType :: Type Id -> DbgDataTransMonad (Type Id)

dType t =
  lookupId TCon t_Arrow >>>= \arrow ->
  lookupId TSyn tTrace >>>= \trail ->
  let dt (TypeCons pos id ts) = 
        mapS dt ts >>>= \ts' ->
	if id == arrow 
          then
	    mapS (wrapRT pos) ts' >>>= \ts'' ->
	    unitS (TypeCons pos arrow 
                    [TypeCons pos trail [], TypeCons pos id ts''])
	  else
	    unitS (TypeCons pos id ts')
      dt (TypeApp t1 t2) = 
	unitS TypeApp =>>> dt t1 =>>> dt t2
      dt (TypeStrict pos t2) = error "not yet (dType: TypeStrict)"
      dt t@(TypeVar pos id) = unitS t
  in  dt t


{- unused:
wrapRTtvars :: Pos -> DbgDataTransMonad ((Type Id,a) -> (Type Id,a))

wrapRTtvars pos =
  lookupId TCon tR >>>= \rid ->
  unitS (\(t, tvs) -> (TypeCons pos rid [t], tvs))
-}

{- wrap data constructor R around given type. -}
wrapRT :: Pos -> Type Id -> DbgDataTransMonad (Type Id)

wrapRT pos t =
  lookupId TCon tR >>>= \rid ->
  unitS (TypeCons pos rid [t])


{-
Translate a type with context. 
All embedded funtion types are transformed.
Context is left unchanged.
-}
dCtxType :: Pos -> a -> Type Id -> DbgDataTransMonad (a,Type Id)

dCtxType pos ctx ty' =
  dType ty' >>>= \ty'' ->
  unitS (ctx, ty'')


dMethodNewType :: a -> NewType -> DbgDataTransMonad NewType

dMethodNewType isCaf (NewType free exist ctxs [nt]) =
--Y  lookupId TClass tDisplayable >>>= \dispid ->
  lookupId TClass tNum >>>= \numid ->
  mapS (\(c, v) -> isNumSubClass c >>>= \b -> 
        unitS (if b then [v] else [])) ctxs >>>= \vss ->
  let newctxs = [(numid, v) | v <- snub (concat vss)] 
--Y             ++
--Y             [(dispid, tid) | tid <- tail free, tid `notElem` map snd ctxs] 
  in
  dNT nt >>>= \nt' ->
  (if False {-isCaf-} then wrapNTRT else topLevelNT) nt' >>>= \nt'' ->
  unitS  (NewType free exist (ctxs ++ newctxs) [nt''])   


{- 
Translates a type (here NewType) similar to function dType.
-}
dNewType :: NewType -> DbgDataTransMonad NewType

dNewType (NewType free exist ctxs nts) =
    mapS dNT nts >>>= \nts' ->
    unitS  (NewType free exist ctxs nts')


{- 
Translates a type (here NT) similar to function dType.
-}
dNT :: NT -> DbgDataTransMonad NT

dNT t =
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tTrace >>>= \trail ->
    lookupId TCon tR >>>= \rt ->
{- superfluous
    lookupId TCon t_List >>>= \bilist ->
    lookupId TCon tList >>>= \list ->
    lookupId TSyn tString >>>= \string ->
    lookupId TCon tRString >>>= \rstring ->
-}
    let dt (NTcons id ts) = 
            mapS dt ts >>>= \ts' ->
	    if id == arrow then
		unitS (NTcons arrow [NTcons trail [], NTcons id (wrapNTs ts')])
	    else 
	        lookupName noPos id >>>= \(Just info) ->
	            unitS (NTcons id ts')
        dt (NTapp t1 t2) = unitS NTapp =>>> dt t1 =>>> dt t2
        dt (NTstrict t) = unitS NTstrict =>>> dt t
	dt t@(NTvar id) = unitS t
	dt t@(NTany id) = unitS t
        -- unused:
	-- isTuple (TupleId _) = True
	-- isTuple  _ = False
	wrapNTs = map (\nt -> NTcons rt [nt])
    in  dt t 


{- Wrap type constructor R around given type. -}
wrapNTRT :: NT -> DbgDataTransMonad NT

wrapNTRT nt =
  lookupId TCon tR >>>= \rt ->
  unitS (NTcons rt [nt])

{-
Wrap type constructor R around all argument but not the result type.
Arguments and result are assumed to be given in form of a list.
-}
wrapRNewType :: NewType -> DbgDataTransMonad NewType

wrapRNewType (NewType free exist ctxs ts) =
  let (t:rts) = reverse ts in
  lookupId TCon tR >>>= \rt ->
  unitS (NewType free exist ctxs 
           (reverse (map (\t -> NTcons rt [t]) rts) ++ [t]))


{- t  ==> SR -> (Trace -> R t) -}
topLevelNT :: NT -> DbgDataTransMonad NT

topLevelNT nt =
  lookupId TCon t_Arrow >>>= \arrow ->
  lookupId TCon tSR >>>= \sr ->
  lookupId TCon tTrace >>>= \d ->
  wrapNTRT nt >>>= \nt' ->
  unitS (NTcons arrow [NTcons sr [], NTcons arrow [NTcons d [], nt']])


{-
Apply type constructor R to type appearing in rhs of data/newtype 
type definition.
-}
dConstr :: Constr Id -> DbgDataTransMonad (Constr Id)

dConstr (Constr pos id ts) = 
    lookupId TCon tR >>>= \rid ->
    unitS (Constr pos id) =>>> 
    unitS (map (\(fieldnames, ty) -> (fieldnames, TypeCons pos rid [ty])) ts)


addSR :: Type Id -> DbgDataTransMonad (Type Id)

addSR t = 
    lookupId TCon t_Arrow >>>= \arrow ->
    lookupId TCon tSR >>>= \sr ->
    unitS (tc arrow [tc sr [], t])


addD :: Type Id -> DbgDataTransMonad (Type Id)

addD t =
    lookupId TCon tTrace >>>= \did ->
    lookupId TCon t_Arrow >>>= \arrow ->
    unitS (tc arrow [tc did [], t])

{-
Remove RT wrapper on the top level in type synonyms
-}
remR :: Type a -> DbgDataTransMonad (Type a)

remR (TypeCons pos id [t]) = unitS t


{-
Only returns True, if in the type an NTany type variable with id == 1
is applied to some type.
-}
isHigherOrder :: a -> NewType -> Bool

isHigherOrder cvar (NewType free exist ctxs ts) = 
  trace ("+++ " ++ concat (map snt ts)) $ or (map (isHO False) ts)
  where 
  isHO :: Bool -> NT -> Bool
  isHO nc (NTcons id ts) = or (map (isHO False) ts)
  isHO nc (NTapp t1 t2) = isHO True t1 || isHO False t2
  isHO nc (NTstrict t) = isHO nc t
  isHO nc (NTvar id) = False
  isHO nc (NTany id) = id == 1 && nc
  snt :: NT -> String
  snt (NTcons id ts) = "(NTcons " ++ show id 
                         ++ concat [' ' : snt t | t <- ts] ++ ")"
  snt (NTapp t1 t2) = "(NTapp " ++ snt t1 ++ " " ++ snt t2 ++ ")"
  snt (NTstrict t) = "(NTstrict " ++ snt t ++ ")"
  snt (NTvar id) = "(NTvar " ++ show id ++ ")"
  snt (NTany id) = "(NTany " ++ show id ++ ")"


-- Utility functions

{-
Determine Id for identifier given by kind and token
-}
lookupId :: IdKind -> TokenId -> DbgDataTransMonad (Id)

lookupId kind ident = 
  \(Inherited lookupPrel _ _ _ _) s -> (lookupPrel (ident, kind), s)


{-
Return info for given identifier
-}
lookupName :: a {-Pos-} -> Id -> DbgDataTransMonad (Maybe Info)

lookupName pos ident = 
  \inh s@(Threaded state _) -> (lookupIS state ident, s)

{-
Return name for given identifier
-}
lookupNameStr :: Id -> DbgDataTransMonad String

lookupNameStr ident  = 
  \inh s@(Threaded state _) -> (strIS state ident, s)


-- Used for debugging

showTheType :: (Show a, StrId a) => Type a -> DbgDataTransMonad String

showTheType t     = 
  \inh s@(Threaded state _) -> (strType False state t, s)
showContext ctxs  = 
  \inh s@(Threaded state _) -> (strContexts False state ctxs, s)
showVarsType vt   = 
  \inh s@(Threaded state _) -> (strVarsType False state vt, s)
showSimple simple = 
  \inh s@(Threaded state _) -> (strSimple False state simple, s)
showNT (NewType free exist ctxs nts)  = 
  \(Inherited lookupPrel _ cv reptree ot) s@(Threaded state _) -> 
    (niceCtxs Nothing state al ctxs 
     ++ mixSpace (map (niceNT Nothing state al) nts), s)
  where 
  al = arg ++ zip (map snd ctxs) (map (('_':).(:[])) ['a'..'z']) 
       -- a-z is too short!
  arg = mkAL free


getArity :: Id -> DbgDataTransMonad Id

getArity id = \(Inherited _ alist _ _ _) s -> (assocDef alist (-1) id, s)
--  (assocDef alist (error ("Internal error: Can't find arity for id #" 
--                          ++ show id)) id, s)


getArities :: [Decl Id] -> DbgDataTransMonad Inherited

getArities ds = \(Inherited lookupPrel _ cv reptree ot) s ->
    let ga (DeclFun pos id (Fun pat _ _:_)) = [(id, length pat)]
        ga d = []
    in  (Inherited lookupPrel (concat (map ga ds)) cv reptree ot, s)


updateMethodType :: Id -> Id -> NewType -> a -> Threaded -> Threaded

updateMethodType im id nt = 
  \inh (Threaded (IntState unique rps st errors) constrs) ->
    case lookupAT st id of
      Just (InfoDMethod u tid _ annots cls) ->
	let st' = updateAT st id (\_ -> InfoDMethod u tid nt annots cls) in
	case lookupAT st im of
	  Just (InfoMethod u tid fix _ annots cls) ->
	    let st'' = updateAT st' im 
                         (\_ -> InfoMethod u tid fix nt annots cls) in
	    Threaded (IntState unique rps st'' errors) constrs


updateClassType :: Id
                -> TokenId 
                -> IE 
                -> NewType 
                -> [Id] 
                -> [Id] 
                -> Tree (Int,([Int],[(Int,Int)])) 
                -> a 
                -> Threaded -> Threaded

updateClassType i tid ie nt ms ds at = 
  \inh (Threaded (IntState unique rps st errors) constrs) ->
    let st' = updateAT st i (\_ -> InfoClass i tid ie nt ms ds at) in
    Threaded (IntState unique rps st' errors) constrs


updateSynType :: Id -> NewType -> a -> Threaded -> Threaded

updateSynType tid nt = 
  \inh (Threaded (IntState unique rps st errors) constrs) ->
    case lookupAT st tid of
        Just (InfoData u rtid ie _ k) ->
	    let st' = updateAT st tid (\_ -> InfoData u rtid ie nt k) in
	    Threaded (IntState unique rps st' errors) constrs


{-
Set new type in symboltable info for given data constructor
-}
updateConstrType :: Id -> NewType -> a -> Threaded -> Threaded

updateConstrType id nt = 
  \inh (Threaded (IntState unique rps st errors) constrs) ->
    case lookupAT st id of
        Just (InfoConstr cid tid fix _ annot ty) ->
	    let st' = updateAT st id 
                        (\_ -> InfoConstr cid tid fix nt annot ty) in
	    Threaded (IntState unique rps st' errors) constrs

{-
Add given data constructor with position to list in threaded state.
Here *only* place where this list is modified in the transformation.
-}
addConstr :: Pos -> Id -> a -> Threaded -> Threaded

addConstr pos id = \inh (Threaded is constrs) ->
    Threaded is ((pos, id):constrs)


newVar :: Pos -> DbgDataTransMonad (Exp Id)

newVar pos = \_ (Threaded istate cs) ->
                 case uniqueIS istate of
	             (i, is') -> (ExpVar pos i, Threaded is' cs)


getState :: DbgDataTransMonad IntState

getState = \_ t@(Threaded is _) -> (is, t)


setClassVar :: Id -> DbgDataTransMonad Inherited

setClassVar id = \(Inherited lookupPrel alist _ reptree ot) s ->
    (Inherited lookupPrel alist id reptree ot, s)


getClassVar :: DbgDataTransMonad Id

getClassVar = \(Inherited _ _ cv _ _) s -> (cv, s)


isNumSubClass :: Id -- class id
                 -> DbgDataTransMonad Bool

isNumSubClass c = \(Inherited lookupPrel _ _ _ _) s@(Threaded is  _) ->
    let dcnum  = lookupPrel (tDNum, TClass)
	scof c = case lookupIS is c of
	             Just info -> any (dcnum==) sc || any scof sc
		         where sc = superclassesI info
    in (scof c, s)


dTrace :: String -> (Inherited -> a -> b) -> Inherited -> a -> b

dTrace str c = \i@(Inherited _ _ cv _ ot) s -> 
                 (if ot then trace str else id) (c i s)


{- construct type from type constructor and types as arguments -}
tc :: Id -- type constructor
      -> [Type Id] 
      -> Type Id

tc c ts = TypeCons noPos c ts


{- unused:
nubEq p [] = []
nubEq p (x:xs) = x : nubEq p (filter ((p x /=) . p) xs)
-}

-- Malcolm's additions:
{-
Create a new primitive identifier with given Info, changing just the
location in the table (i.e. the lookup key).
-}
addNewPrim :: Info -> DbgDataTransMonad Id

addNewPrim (InfoVar _ (Qualified m nm) fix ie nt ar) = 
  \_ (Threaded istate idt) ->
    case uniqueIS istate of
      (i, istate') -> 
        let newNm = Qualified m (packString ('\'':unpackPS nm))
            info' = InfoVar i newNm fix IEnone NoType ar
            istate'' = addIS i info' istate'
        in (i, Threaded istate'' idt)


{-
Overwrite the original primitive identifier with new Info, reflecting
the change in type and arity.
-}
overwritePrim :: Int -> a -> Threaded -> Threaded

overwritePrim i = 
  \_ (Threaded istate idt) ->
      let updI (InfoVar i nm fix ie _ _) = InfoVar i nm fix ie NoType (Just 2)
      in Threaded (updateIS istate i updI) idt



{- End Module DbgDataTrans -------------------------------------------------}
