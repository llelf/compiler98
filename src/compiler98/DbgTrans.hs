{- ---------------------------------------------------------------------------
Transforms all value definitions of a program 
for producing traces for debugging.
-}
module DbgTrans(SRIDTable,debugTrans, dbgAddImport) where

import Extra(Pos, noPos, pair, fromPos, strPos, dropJust, trace)
import IdKind(IdKind(Con,Var))
import TokenId
import DbgId(t_R,t_mkTRoot,t_mkTNm 
            ,t_rseq,t_fatal
            ,t_lazySat,t_fun,t_primn,t_prim,t_ap,t_rap,t_cn,t_con,t_pa,t_indir
            ,t_if,t_guard
            ,t_mkSR',t_mkNTId',t_mkNTConstr',t_mkNTLambda,t_mkNTCase
            ,t_conInt,t_conChar,t_conInteger,t_conRational,t_conDouble
            ,t_conFloat
            ,t_fromConInteger,t_fromConRational
            ,t_patFromConInteger,t_patFromConRational)
import IntState(IntState(IntState),addIS,arityIS,arityVI,lookupIS,strIS
               ,uniqueIS,uniqueISs,IE(IEnone),ntI,updateIS
               ,Info(InfoVar,InfoClass,InfoMethod,InfoDMethod,InfoIMethod))
import Syntax
import SyntaxPos
import NT
import State
import AssocTree
import PackedString(PackedString, unpackPS, packString)
import Id(Id)
import Info(typeSynonymBodyI)
import TypeSubst(substNT)
import Nice(niceNewType)
import List  -- (zipWith3)


{- table for source references and identifiers refered to from the trace -}
type SRIDTable = Maybe ((Int,[Pos])        -- source reference table
                       ,[(Pos,Id)]         -- identifier table
                       ,[ImpDecl TokenId]  -- import declarations
                       ,String)            -- module name


-- newNameVar, nameArity, hsFromInt, addInt

data Inherited = Inherited 
                   (Exp Id) 
                   (Exp Id) -- `fail', to be called in case of failure
                   ((TokenId, IdKind) -> Id) -- lookupPrel

data Threaded = Threaded 
                  IntState 
                  (Int, [Pos]) -- source reference table, 
                               -- accumulated for SRIDTable
                               -- first is number of current source reference
                               -- second is list of encoded source references
                  [(Pos, Id)]  -- identifier table, accumulated for SRIDTable


type DbgTransMonad a = State Inherited Threaded a Threaded

type TraceExp = Exp Id    -- expression of type Trace
type NmTypeExp = Exp Id   -- expression of type NmType
type SRExp = Exp Id       -- expression of type SR


{- obtain the internal state -}

getIntState :: DbgTransMonad IntState

getIntState inherited threaded@(Threaded intState _ _) = (intState,threaded)
 

{-
Used to add the special prelude for debugging to the import list.
Now it serves no purpose and is just the identity function.
-}
dbgAddImport :: Bool -> Module a -> Module a

dbgAddImport dodbg m@(Module pos id exports imports fixities decls) =
    Module pos id exports (dbgAI dodbg imports) fixities decls
    where dbgAI True impdecls = 
              {-ImportQ (noPos, tDbgPrelude) (Hiding []) :-} impdecls
{-
	  dbgAI True True impdecls = 
	      {-ImportQ (noPos, tDbgPreludeCore) (Hiding []) :-} impdecls
-}
	  dbgAI False impdecls = impdecls


{-
Transforms all value definitions for producing traces for debugging.
-}
debugTrans :: a 
           -> IntState 
           -> ((TokenId,IdKind) -> Id) 
           -> PackedString 
           -> b 
           -> [ImpDecl TokenId]  
           -> Decls Id 
           -> Maybe [(Pos,Id)] -- data constructors defined by data/newtype
           -> (Decls Id        -- transformed declarations
              ,IntState
              ,SRIDTable)

debugTrans flags istate lookupPrel modidl modid impdecls decls (Just constrs) =
  initDebugTranslate (dTopDecls decls) istate lookupPrel
  where 
  initDebugTranslate f istate lookupPrel = 
    case f (Inherited start_d fatal lookupPrel) 
           (Threaded istate (1, [0]) constrs) of
      (decls', Threaded istate' srt idt) -> 
        (decls', istate', Just (srt, idt, impdecls, reverse (unpackPS modidl)))
  start_d :: Exp Id
  start_d = ExpVar noPos (lookupPrel (t_mkTRoot, Var)) 
            -- data constructor Root of the R type 
  fatal :: Exp Id
  fatal = ExpCon noPos (lookupPrel (t_fatal, Var))
          -- variable ? fatal
          -- wrong default initialisation that should never be used 


dTopDecls :: Decls Int -> DbgTransMonad (Decls Int)

dTopDecls (DeclsParse ds) = 
    mapS dTopDecl ds >>>= \dss -> 
       unitS (DeclsParse (concat dss))


dTopDecl :: Decl Int -> DbgTransMonad [Decl Int]

dTopDecl d@(DeclType id t) = unitS [d]
dTopDecl d@(DeclData mb ctx id contrs tycls) = unitS [d]
dTopDecl d@(DeclConstrs pos id constrids) = unitS [d]
dTopDecl (DeclClass pos ctx id1 id2 decls) =
    dDecls decls >>>= \decls' ->
    lookupName id1 >>>= \(Just (InfoClass i tid ie nt ms ds at)) ->
    mapS0 fixMethodArity (zip ms ds) >>>
    unitS [DeclClass pos ctx id1 id2 decls']
    where fixMethodArity (m, d) =
              lookupName d >>>= \(Just (InfoDMethod _ _ _ (Just arity) _)) ->
	      setArity 2 {-arity-} m
dTopDecl d@(DeclInstance pos ctx id inst decls) = 
  unitS  ((:[]) . DeclInstance pos ctx id inst) =>>> dDecls decls
dTopDecl d = dDecl d


dDecls :: Decls Id -> DbgTransMonad (Decls Id)

dDecls (DeclsParse ds) = 
    mapS dDecl ds >>>= \dss -> 
    unitS (DeclsParse (concat dss))


-- dDecl :: Decl Id -> DbgTransMonad [Decl Id]

dDecl d@(DeclTypeRenamed _ _) = unitS [d]
dDecl d@(DeclDefault tys) = unitS [d]
dDecl d@(DeclVarsType vars ctx ty) = unitS [d] 
dDecl (DeclPat (Alt pat rhs decls)) =
    addNewName 0 True "_pv" NoType >>>= \patid ->
    --trace ("patid = " ++ show patid) $
    setArity 0 patid >>>
    patVars pat >>>= \(pat', bvsnvs) ->
    let bvsposids = map fst bvsnvs -- (pos,id)s of original var 
        (bvspos,bvsids) = unzip bvsposids 
        pos = head bvspos in
    mapS0 (setArity 2) bvsids >>>  
    mapS0 addId bvsposids >>>
    mapS makeSourceRef bvspos >>>= \srs ->
    dRhs rhs failContinuation >>>= \rhs' ->
    dPat pat' >>>= \pat'' ->
    let ExpApplication _ [r,_,tresult] = pat'' in
    dDecls decls >>>= \decls' ->
--    lookupId Var t_lazySat >>>= \lazySat ->
--    lookupId Var t_patvar >>>= \pvid ->
--    lookupCon noPos tNTId >>>= \ntid ->
--    lookupCon noPos t_Nm >>>= \nm ->
    getD >>>= \redex ->
    mkFailExpr pos 
      -- the error message "Pattern match failure" could be more specific 
      >>>= \fe ->
    let evars = map snd bvsnvs in 
    makeTuple noPos evars >>>= \etup ->

    zipWithS makeNTId bvspos bvsids >>>= \ntIds ->
    zipWithS (makeNm pos redex) ntIds srs >>>= \nms ->
    lookupVar pos t_lazySat >>>= \lazySat ->
    addNewName 0 True "nt" NoType >>>= \t' ->
    let
        pcase = ExpCase noPos rhs' 
                  [Alt pat'' (Unguarded etup) (DeclsParse [])
	          ,Alt (PatWildcard noPos)   (Unguarded fe)  (DeclsParse [])]
	pfun = DeclFun noPos patid [Fun [] (Unguarded pcase) decls']
	vpat p pv i sr = vcase p pv
        vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup (Unguarded pv) (DeclsParse [])]
        vfun ((p, i), pv) sr nte = 
          DeclFun p i 
            [Fun [PatWildcard p, PatWildcard p] 
               (Unguarded (ExpApplication pos 
                            [lazySat, (vpat p pv i sr), ExpVar p t'] )) 
               (DeclsParse 
                 [DeclFun p t' [Fun [] (Unguarded nte) (DeclsParse [])]])
            ]
    in unitS (pfun : zipWith3 vfun bvsnvs srs nms)
{- 
The following code is an experiment to create useful redex trails
for pattern bindings. Unfortunately viewing an unevaluted pattern
variable in the browser leads to a segmentation fault of the program.
It seems that Sats sometimes don't work correctly, when the expression
isn't yet evaluated. 
    let epat = ExpApplication noPos [r,etup,tresult] in

    let orgPos = map (fst . fst) bvsnvs in
    mapS 
      (\p -> addNewName 0 True "const" NoType)
      orgPos
      >>>= \constIds ->

    mapS 
      (\p -> addNewName 0 True "share" NoType)
      orgPos
      >>>= \shareIds ->

    addNewName 0 True "local" NoType >>>= \local ->

    newVar noPos >>>= \test' ->
    let ExpVar _ test = test' in 

    let mkNTId p i = ExpApplication p [ntid,ExpLit p (LitInt Boxed i)]
        mkNm p nt sr = ExpApplication p [nm, redex, nt, sr] 

{-
        mkLazySat a t = ExpCase noPos a
                          [Alt (PatIrrefutable noPos 
                                 (ExpApplication noPos [r,v,vt]))
                               (Unguarded 
                                 (ExpApplication noPos [r,v,
                                   ExpApplication noPos [sat,t,vt]]))
                               (DeclsParse [])]
-}

        pcase = ExpCase noPos rhs' 
                  [Alt pat'' (Unguarded epat) (DeclsParse [])
	          ,Alt (PatWildcard noPos)   (Unguarded fe)  (DeclsParse [])]
	pfun = DeclFun noPos patid [Fun [] (Unguarded pcase) decls']

	vpat p pv i sr = 
          ExpApplication p [r, ExpLit p (LitChar Boxed 'c'), redex]
{-
          ExpCase p (ExpVar p test) -- patid)
            [Alt epat (Unguarded $ fe)
--              ExpApplication p [ExpVar p pvid 
--	                       ,mkNTId p i
--			       ,pv
--                               ,sr
--                               ,tresult])
              (DeclsParse [])]
-}

        vconst p i s c sr = DeclFun p c
                                 [Fun []
                                 (Unguarded $ ExpApplication p
                                   [ExpVar p lazySat
                                   ,ExpVar p s
--                                     ,fe
--                                   ,vpat p pv i sr
                                   ,ExpVar p local -- mkNm p (mkNTId p i) sr
                                   ])
                                 (DeclsParse [
                                   DeclFun p local
                                     [Fun []
                                     (Unguarded $ mkNm p (mkNTId p i) sr)
                                     (DeclsParse [])]
                                   ])]

        vshare p s pv i sr = 
          DeclFun p s [Fun [] (Unguarded $ vpat p pv i sr) (DeclsParse [])]

        vfun p i c = 
          DeclFun p i [Fun [PatWildcard p, PatWildcard p] 
                        (Unguarded (ExpVar p c)) (DeclsParse [])] 

{- very old:
	vpat p pv i sr = ExpApplication p [ExpVar p pvid, 
	                                   ExpApplication p 
                                             [ntid 
					     ,ExpLit p (LitInt Boxed i)],
					   vcase p pv{-, sr, redex-}]
        vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup (Unguarded pv) (DeclsParse [])]
        vfun ((p, i), pv) sr = DeclFun p i 
                                 [Fun [] (Unguarded (vpat p pv i sr)) 
                                 (DeclsParse [])]
-}
    in unitS (pfun 
             : zipWith3 vfun orgPos bvsids constIds 
             ++ zipWith5 vshare orgPos shareIds evars bvsids srs
             ++ zipWith5 vconst orgPos bvsids shareIds constIds srs)
-}
dDecl d@(DeclFun pos id fundefs) = 
    addId (pos, id) >>>
    lookupName id >>>= \(Just info) ->
    lookupNameStr id >>>= \funName ->
    --trace ("DeclFun: funName = " ++ funName ++ ", arity " ++ show info) $
    if isCMethod info then
      dMethod info pos id funName fundefs
    else
      getIntState >>>= \intState ->
      let arity = getArity fundefs
      in case arity of
           0 -> dCaf pos id funName fundefs 
                  (unwrapNT intState 0 True (ntI info))
           _ -> dFun pos id funName arity fundefs 
                  (unwrapNT intState arity False (ntI info))
dDecl d@(DeclIgnore _) = unitS [d]
dDecl d@(DeclError _) = unitS [d]
dDecl d@(DeclAnnot _ _) = unitS [d]
dDecl d@(DeclFixity _) = unitS [d]
dDecl d@(DeclPrimitive pos id i ty) = unitS [d]
dDecl d@(DeclForeignImp pos cname id' arity cast typ id) =
    addId (pos, id) >>>
    lookupName id' >>>= \(Just info) ->
--  lookupNameStr id' >>>= \funName ->
--  trace ("DeclForeignImp: " ++ funName ++", info=\n" ++ show info) $
    -- generate code for the wrapper
    dForeignImp pos id id' arity >>>= \code->
    -- get real cname from primed hname (f'), if needed
    let (InfoVar _ (Qualified _ f) _ _ _ _) = info
        cname' = if null cname then reverse (tail (unpackPS f)) else cname
    in
    unitS [ DeclForeignImp pos cname' id' arity cast typ id
          , DeclFun pos id [code] ]
dDecl d@(DeclForeignExp pos cname id typ) =
	error ("Can't trace foreign exports yet. "++strPos pos)



dMethod info@(InfoDMethod _ tid nt (Just arity) _) pos id funName fundefs = 
    --trace ("InfoD: " ++show info) $
--    if arity == 0 then
--        dCaf pos id funName fundefs
-- dCaf does not work, 
-- because shared constants need to be defined outside class/instance
    if getArity fundefs == arity then
        dFun pos id funName arity fundefs NoType --(unwrapNT False nt)
    else
        case fundefs of
            -- does this recognise compiler-produced undefined methods?
            -- you don't want to see the explicit call of error in the trace
            [Fun [] (Unguarded (ExpApplication p1 [ExpVar p2 te, emsg])) 
              (DeclsParse [])] ->
	        lookupId Var t_error >>>= \errorid ->
		if te == errorid then
		    setArity 2 id >>>
	            lookupId Var t_fatal >>>= \fatal ->
		    newVars pos 2 >>>= \[sr, redex] ->
		    unitS [DeclFun pos id [Fun [sr, redex]  
		                          (Unguarded
                                            (ExpApplication p1 
                                              [ExpVar p2 fatal, redex]))
					  (DeclsParse [])]]
		 else
		     dFun pos id funName arity fundefs NoType 
	    _ -> dFun pos id funName (getArity fundefs) fundefs NoType 
--	    _ -> dFun pos id funName arity fundefs NoType --(unwrapNT False nt)
dMethod info@(InfoIMethod _ tid nt (Just arity) _) pos id funName fundefs = 
    --trace ("InfoI: " ++show info) $
    -- if (getArity fundefs) {-arity-} == 0 then  --- Wrong !!!!
--        dCaf pos id funName fundefs
-- dCaf does not work, 
-- because shared constants need to be defined outside class/instance
--    else 
        case fundefs of
           [Fun [] (Unguarded (ExpVar p1 d)) (DeclsParse [])] ->
               -- This must(?) be a wrapper to the method of the superclass
               -- or default(?)
               -- you don't want to see the explicit call in the trace
               setArity 2 id >>>
	       newVars pos 2 >>>= \[sr, redex] ->
	       unitS [DeclFun pos id [Fun [sr, redex]
	                              (Unguarded 
                                        (ExpApplication p1 
                                          [ExpVar p1 d, sr, redex]))
				      (DeclsParse [])]]
	   _ -> dFun pos id funName (getArity fundefs) fundefs NoType--(unwrapNT False nt)


dCaf :: Pos -> Id -> String -> [Fun Id] -> NewType -> DbgTransMonad [Decl Id]

dCaf pos id cafName [Fun [] rhs localDecls] nt =
  setArity 2 id >>>
  addNewName 0 True "nt" NoType >>>= \t' ->
  addNewName 0 True cafName nt >>>= \nid ->
  getD >>>= \redex ->                  -- Use the surrounding redex
  setD (ExpVar pos t') >=>
  dRhs rhs failContinuation >>>= \rhs' ->
  dDecls localDecls >>>= \(DeclsParse localDeclsList') ->
  makeSourceRef pos >>>= \sr ->
  makeNTId pos id >>>= \ntId ->
  makeNm pos redex ntId sr >>>= \nte ->
  lookupVar pos t_lazySat >>>= \lazySat ->
  unitS [DeclFun pos id 
          [Fun [PatWildcard pos, PatWildcard pos] 
             (Unguarded (ExpVar pos nid)) (DeclsParse [])
          ]
        -- id _ _ = nid
        ,DeclFun pos nid 
           [Fun [] 
             (Unguarded (ExpApplication pos [lazySat, rhs', ExpVar pos t']))
	     (DeclsParse
	        (DeclFun pos t' [Fun [] (Unguarded nte) (DeclsParse [])] 
                :localDeclsList'
                )
             )
           ]
        {- 
        id = lazySat e t'
          where
          t' = Nm redex (NTId id) sr
          decls
        -}
        ]
dCaf pos id cafName _ nt =
  error ("Variable " ++ cafName ++ " multiple defined.")
  -- actually nhc should produce such an error already before;
  -- however, currently accepts such definitions. 

{-
dFun :: Pos -> Id -> String -> Int -> [Fun Id] -> NewType
     -> DbgTransMonad [Decl Id]
-}
   
dFun pos id funName arity fundefs nt =
  lookupVar pos (t_fun arity) >>>= \fun ->
  setArity 2 id >>>
  newVar pos >>>= \redex ->
  newVar pos >>>= \sr ->
  -- buildFun pos id (arity+1) [redex] funName fun redex fundefs
  addNewName (arity+1) True funName nt >>>= \wrappedfun ->	    
  newVars pos (arity+1) >>>= \(newredex:fp) ->
  setD newredex >=>
  lookupId Con tTrue >>>= \true ->
  makeNTId pos id >>>= \ntid ->
  lookupId Var t_otherwise >>>= \otherwise ->
  checkPrimitive fundefs >>>= \prim ->
  (case prim of
    Nothing -> dFunClauses funName arity true otherwise fundefs
--  Nothing -> mapS dFunClause fundefs
    Just fundefs -> unitS (fundefs, [])
--  Just fundefs -> unitS (fundefs)
  ) >>>= \(fundefs', newdecls) ->
--) >>>= \(fundefs') ->
  getIntState >>>= \intState ->
  unitS ([DeclFun pos id [Fun [sr, redex]
    (Unguarded 
      (ExpApplication pos 
        [fun, ntid, ExpVar pos wrappedfun, sr, redex]
      ))
      (DeclsParse 
        (prependTypeSigIfExists intState pos wrappedfun
          (DeclFun pos wrappedfun fundefs' : newdecls)))
    ]])
--     (DeclsParse [DeclFun pos wrappedfun (fundefs'++[fpclause])])]])
  where
  --prependTypeSigIfExists :: IntState -> Pos -> Id -> ([Decl Id] -> [Decl Id])
  prependTypeSigIfExists intState pos wrappedFun =
    case nt of
      NoType -> \x->x
      _      -> ( DeclIgnore ("Type signature " ++ niceNewType intState nt) :)
                -- a bit of a hack
                -- type signatures do not exist any more in this phase in
                -- the syntax tree, but the information is useful for
                -- debugging (also necessary for polymorphic recursion?)

{-
For each clause of the function definition e, return a transformed
definition e', and possibly declare a new auxiliary function to handle
failure across guards.
Note, the parent trace in the monad is slightly misused for passing
a variable.
-}
--dFunClauses :: String -> Int -> Id -> Id -> [Fun Id] 
--            -> DbgTransMonad ([Fun Id],[Decl Id])

dFunClauses funName arity true otherwise [] = 
  -- catch the case that all patterns and guards fail
  getD >>>= \parent ->
  let vars = parent : replicate arity (PatWildcard noPos) in
  mkFailExpr noPos >>>= \fpexp ->
  unitS ([Fun vars (Unguarded fpexp) (DeclsParse [])],[])
-- No guards is the easiest case.
--     f pat1 pat2 ... = e  where decls
-- ==>
--     f t pat1' pat2' ... = e' where decls'
dFunClauses funName arity true otherwise (Fun pats (Unguarded e) decls : fcs) =
       getD >>>= \t ->
       dPats pats >>>= \pats' ->
       dExp False e >>>= \e' ->
       dDecls decls >>>= \decls' ->
       dFunClauses funName arity true otherwise fcs >>>= \(mfs, nfs) ->
       unitS (Fun (t:pats') (Unguarded e') decls' : mfs, nfs)

dFunClauses funName arity true otherw (Fun pats (Guarded ges) decls : fcs)
  | not (null fcs) && canFail true otherw ges =
    -- for the remaining clauses a new function has to be defined
    -- which is called if all guards fail;
    -- necessary, because the trace which registers all failed guards
    -- has to be passed to this function to not to lose this information
    mapS namePat pats >>>= \namedpats ->
    let (patnames, pats') = unzip namedpats in
    addNewName (arity + 1) True funName NoType >>>= \f ->
    dPats pats' >>>= \pats'' ->
    newVar noPos >>>= \t ->
    setD t >=>
    let continuation = functionContinuation f patnames in
    continuationToExp continuation t >>>= \contExp ->
    dGuardedExprs ges continuation >>>= \expr ->
    dDecls decls >>>= \decls' ->
    let failclause = Fun (t:patnames) (Unguarded contExp) (DeclsParse []) in
    dFunClauses funName arity true otherw fcs >>>= \(mfs, nfs) ->
    unitS ([Fun (t:pats'') 
            (Unguarded (ExpApplication noPos [expr, t])) decls'
            , failclause]
          , DeclFun noPos f mfs:nfs)
  | otherwise =
    -- guards cannot fail or last clause
    getD >>>= \t ->
    dPats pats >>>= \pats' ->
    dGuardedExprs ges failContinuation >>>= \e ->
    dDecls decls >>>= \decls' ->
    dFunClauses funName arity true otherw fcs >>>= \(mfs, nfs) ->
    let fs = Fun (t:pats') 
               (Unguarded (ExpApplication noPos [e, t])) decls' in
    unitS (fs:mfs, nfs)


{-
To correctly create the trace within guards, a continuation is used.
The type ContExp should be abstract. Its implementation is only used in 
the following three functions.
-}
data ContExp = Fail | Function Id [Exp Id]

failContinuation :: ContExp

failContinuation = Fail


functionContinuation :: Id -> [Exp Id] -> ContExp

functionContinuation = Function


continuationToExp :: ContExp -> TraceExp -> DbgTransMonad (Exp Id)

continuationToExp Fail t =
  lookupVar noPos t_fatal >>>= \fail ->
  unitS $ ExpApplication noPos [fail, t]
continuationToExp (Function fun args) t =
  unitS $ ExpApplication noPos (ExpVar noPos fun : t : args)


{-
Create body of wrapper for imported foreign function.
id is the new function we are declaring; id' is the real foreign function
-}
dForeignImp :: Pos -> Id -> Id -> Int -> DbgTransMonad (Fun Id)

dForeignImp pos id id' arity =
    lookupVar pos (t_primn arity) >>>= \primn ->
    makeNTId pos id >>>= \ntid ->
    setArity 2 id >>>
    newVar pos >>>= \redex ->
    newVar pos >>>= \sr ->
    unitS (Fun [sr, redex]
               (Unguarded
                 (ExpApplication pos 
                    [primn, ntid, ExpVar pos id', sr, redex]))
               (DeclsParse []))


{-
The continuation is used if all guards fail.
-}
dGuardedExprs :: [(Exp Id,Exp Id)] -> ContExp -> DbgTransMonad (Exp Id)

dGuardedExprs [] cont = 
    newVar noPos >>>= \t ->
    continuationToExp cont t >>>= \contExp ->
    unitS (ExpLambda noPos [t] contExp)
dGuardedExprs ((g, e):ges) cont = 
    let pos = getPos g in
    dGuardedExprs ges cont >>>= \ges' ->
    dExp False g >>>= \g' ->
    newVar pos >>>= \t ->
    setD t >=>
    dExp False e >>>= \e' ->
    lookupVar pos t_guard >>>= \guard ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [guard, sr, g', ExpLambda pos [t] e', ges'])


{- Used for combining fail continuation with a trace -}
addAppTrace :: Exp Id -> TraceExp -> Exp Id

addAppTrace (ExpApplication pos (f:es)) t = ExpApplication pos (f:t:es)
addAppTrace f t = ExpApplication noPos [f, t]


{- 
obtain a variable that names the given pattern;
easy for variable pattern or as pattern; otherwise produces as pattern
-}
namePat :: Pat Id -> DbgTransMonad (Exp Id,Pat Id)

namePat e@(ExpVar p v) = unitS (e, e)
namePat e@(PatAs p v pat) = unitS (ExpVar p v, e)
namePat pat = 
    newVar noPos >>>= \e@(ExpVar _ v) -> unitS (e, PatAs noPos v pat)


{-
Returns False only if the one of the guards definitely has value True.
-}
canFail :: Id  -- of constructor True
        -> Id  -- of variable otherwise
        -> [(Exp Id, Exp Id)] -- guarded expressions
        -> Bool

canFail true otherwise [] = True
canFail true otherwise ((ExpCon _ cid, _):gdes) = 
    (true /= cid) && canFail true otherwise gdes
canFail true otherwise ((ExpVar _ cid, _):gdes) = 
    (otherwise /= cid) && canFail true otherwise gdes
canFail true otherwise (_:gdes) = canFail true otherwise gdes


checkPrimitive :: [Fun Id] -> DbgTransMonad (Maybe [Fun Id])

checkPrimitive 
  [Fun ps (Unguarded (ExpApplication pos (ExpVar p id:f:es))) decls] =
    lookupId Var t_prim >>>= \primid ->
    -- does this really refer to "_prim" in DebugPrelude ?
    if id == primid then
        getD >>>= \redex ->
	lookupVar pos t_rseq >>>= \rseq ->
--	let expr = ExpApplication pos (f:redex:es)
	let expr = foldr (\n e -> ExpApplication pos [rseq, n, e]) 
	                 (ExpApplication pos (f:redex:es))
			 es
        in unitS (Just [Fun (redex:ps) (Unguarded expr) decls])
    else
        unitS Nothing
checkPrimitive _ = unitS Nothing


mkFailExpr :: Pos -> DbgTransMonad (Exp Id)

mkFailExpr pos =
    lookupVar pos t_fatal >>>= \fatal ->
    getD >>>= \redex ->
    unitS (ExpApplication pos [fatal, redex])


dRhs :: Rhs Id -> ContExp -> DbgTransMonad (Exp Id)

dRhs (Unguarded exp) cont = dExp False exp
dRhs (Guarded gdExps) cont = 
  getD >>>= \t ->
  dGuardedExprs gdExps cont >>>= \expr -> 
  unitS (ExpApplication noPos [expr, t])

{-
First argument True iff the context of the expression is such if the
expression is just a variable, the expression including context is
not a projection.
-}

dExps :: Bool -> [Exp Id] -> DbgTransMonad [Exp Id]

dExps cr es = mapS (dExp cr) es


dExp :: Bool -> Exp Id -> DbgTransMonad (Exp Id)

dExp cr (ExpLambda pos pats e) = 
    newVar pos >>>= \redex ->
    newVars pos (length pats) >>>= \npats ->
    getD >>>= \oldredex ->
    setD redex >=>
    dExp False e >>>= \e' ->
    lookupVar pos (t_fun (length pats)) >>>= \fun ->
    makeNTLambda pos >>>= \lambda ->
    dPats pats >>>= \pats' ->
    newVar pos >>>= \fpat ->
    mkFailExpr pos {-Wrong or misleading error message -} >>>= \fpexp ->
    makeSourceRef pos >>>= \sr ->
    makeTuple pos npats >>>= \npatstup ->
    makeTuple pos pats' >>>= \patstup ->
    let lamexp = ExpLambda pos (redex:npats) (ExpCase pos npatstup alts)
	alts = [Alt patstup (Unguarded e') (DeclsParse []),
	        Alt fpat (Unguarded fpexp) (DeclsParse [])] 
    in unitS (ExpApplication pos [fun, lambda, lamexp, sr, oldredex])
dExp cr (ExpLet pos decls e) = 
  unitS (ExpLet pos) =>>> dDecls decls =>>> dExp False e
  -- shouldn't there be cr instead of False?
dExp cr (ExpCase pos e alts) =  
  let alt2Fun :: Alt a -> Fun a
      alt2Fun (Alt pat rhs decls) = Fun [pat] rhs decls in
  lookupId Con tTrue >>>= \true ->
  lookupId Var t_otherwise >>>= \otherw ->
  lookupVar pos (t_ap 1) >>>= \apply ->
  lookupVar pos (t_fun 1) >>>= \fun ->
  makeNTCase pos >>>= \casenm ->
  dExp cr e >>>= \e' ->
  getD >>>= \oldredex ->
  newVar noPos >>>= \t ->
  setD t >=>
  makeSourceRef pos >>>= \sr ->
  dFunClauses "case" 1 true otherw (map alt2Fun alts) >>>= \(fun', defs') ->
  addNewName 2 True "case" NoType >>>= \fid ->
  unitS $
    ExpApplication pos 
      [apply, sr, oldredex
      ,ExpApplication pos 
	[fun 
	,casenm
        ,ExpLet pos (DeclsParse (DeclFun pos fid fun' : defs')) 
          (ExpVar pos fid)
	,sr,oldredex]
      ,e']
dExp cr (ExpIf pos c e1 e2) = 
    dExp cr c >>>= \c' ->
    getD >>>= \oldredex ->
    newVar pos >>>= \t ->
    setD t >=>
    dExp False e1 >>>= \e1' ->
    dExp False e2 >>>= \e2' ->
    makeSourceRef pos >>>= \sr ->
    lookupVar pos t_if >>>= \tif ->
    unitS (ExpApplication pos [tif, sr, c', ExpLambda pos [t] e1', 
                               ExpLambda pos [t] e2', oldredex])
dExp cr (ExpType pos e ctx t) = 
  unitS (\e' -> ExpType pos e' ctx t) =>>> dExp cr e
dExp cr (ExpApplication pos (f:es))     = 
    case f of
        ExpCon _ _ ->            
	    saturateConstr f es
	_ ->
	    lookupVar pos ((if cr then t_ap else t_rap) (length es)) >>>= \apply ->
	    makeSourceRef pos >>>= \sr -> 
	    dExps True (f:es) >>>= \fes ->
	    getD >>>= \trail ->
            unitS (ExpApplication pos (apply:sr:trail:fes))
dExp cr e@(ExpCon pos id) = saturateConstr e []
dExp cr e@(ExpVar pos id) = 
--    lookupName id >>>= \name ->
    getIdArity id >>>= \arity ->
    case arity of
        Nothing -> -- Must be a lambdabound variable
	    if cr then 
	        unitS e
	    else
	        getD >>>= \redex ->
	        lookupVar pos t_indir >>>= \indir ->
	        unitS (ExpApplication pos [indir, redex, e])
--	Just 0  -> unitS e       -- A CAF or a letbound variable with arity 0
	Just n  ->		     -- A letbound or global function
	    makeSourceRef pos >>>= \sr ->
	    getD >>>= \redex ->
	    unitS (ExpApplication pos [e, sr, redex]) 
dExp cr e@(ExpLit pos (LitString _ s)) = -- mkLitString pos e
    -- This is somewhat expensive. But it works...
    lookupCon pos t_Colon >>>= \consid ->
    lookupCon pos t_List >>>= \nilid ->
    let rs = (map (ExpLit pos . LitChar Boxed) s) in
    dExp True (foldr (\c cs -> ExpApplication pos [consid, c, cs]) nilid rs)
dExp cr e@(ExpLit pos (LitInteger b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConInteger >>>= \fci ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fci, sr, d, e])
dExp cr e@(ExpLit pos (LitRational b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConRational >>>= \fcr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fcr, sr, d, e])
dExp cr e@(ExpLit pos lit) = 	-- at a guess, this clause is obsolete.
    dLit lit >>>= \constr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [constr, sr, d, e])
    where dLit (LitInt _ _) = lookupVar pos t_conInt
          dLit (LitChar _ _) = lookupVar pos t_conChar
          dLit (LitInteger _ _) = lookupVar pos t_conInteger
	  dLit (LitRational _ _) = lookupVar pos t_conRational
	  dLit (LitDouble _ _) = lookupVar pos t_conDouble
	  dLit (LitFloat _ _) = lookupVar pos t_conFloat

dExp cr (ExpList pos []) = lookupCon pos t_List >>>= \nil-> wrapConst nil []
dExp cr (ExpList pos es) = 
    lookupCon pos t_Colon >>>= \consid ->
    dExps True es >>>= \es' ->
    foldS 
        (\e es -> wrapConst consid [e, es])
        (lookupCon pos t_List >>>= \nil -> wrapConst nil []) es'
dExp cr e = error ("dExp: no match")


{-
Transform data constructor application.
Number of arguments may be smaller than arity of the data constructor.
-}
saturateConstr :: Exp Id      -- data constructor
               -> [Exp Id]    -- arguments of the data constructor
               -> DbgTransMonad (Exp Id) -- transformed constructor application

saturateConstr c@(ExpCon pos id) args =
  --trace ("<<< " ++ show id' ++ " -> " ++ show id) $
  dExps True args >>>= \args' ->
  getConArity id >>>= \arity ->
  --trace ("Arity for " ++ show id ++ " is " ++ show arity) $
  if arity > length args' then -- Unsaturated constructor
    makeNTConstr pos id >>>= \ntconstr ->
    lookupVar pos (t_cn (arity - length args')) >>>= \cn ->
    lookupVar pos (t_pa (length args')) >>>= \pan ->
    getD >>>= \redex ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos (pan:c:cn:sr:redex:ntconstr:args'))
   else
    wrapConst c args'


{-
Transform constructor application where number of arguments
equals arity of constructor. The arguments have already been transformed.
-}
wrapConst :: Exp Id -> [Exp Id] -> DbgTransMonad (Exp Id)

wrapConst c@(ExpCon pos cid) args =
    getD >>>= \d ->
    lookupVar pos (t_con (length args)) >>>= \con ->
    makeNTConstr pos cid >>>= \ntconstr ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos (con:sr:d:c:ntconstr:args))


{- Unused, string is transformed on a per character basis 
mkLitString :: Pos -> Exp Id -> DbgTransMonad (Exp Id)

mkLitString pos s =
    getD >>>= \d ->
    lookupVar pos t_stringConst >>>= \stringConst  ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [stringConst, sr, d, s])
-}


dPats :: [Pat Id] -> DbgTransMonad [Pat Id] 

dPats ps = mapS dPat ps


dPat :: Pat Id -> DbgTransMonad (Pat Id)

dPat (ExpApplication pos (c:ps)) = 
  wrapR pos =>>> 
  (unitS (ExpApplication pos) =>>> (unitS (c:) =>>> dPats ps))
dPat p@(ExpCon pos id)              = wrapR pos =>>> unitS p
dPat p@(ExpVar pos id)              = unitS p 
dPat p@(ExpLit pos (LitInteger b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConInteger >>>= \pfci ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfci, sr, d, p])
dPat p@(ExpLit pos (LitRational b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConRational >>>= \pfcr ->
    getD >>>= \d ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfcr, sr, d, p])
dPat p@(ExpLit pos (LitString _ s)) = 
    foldPatList pos (map (ExpLit pos . LitChar Boxed) s)
dPat p@(ExpLit pos lit)             = wrapR pos =>>> unitS p
dPat p@(ExpList pos [])             = wrapR pos =>>> unitS p
dPat (ExpList pos ps)               = foldPatList pos ps
dPat (PatAs pos id p)               = unitS (PatAs pos id) =>>> dPat p
dPat p@(PatWildcard pos)            = unitS p 
dPat (PatIrrefutable pos p)         = unitS (PatIrrefutable pos) =>>> dPat p
dPat e                              = error ("dPat: no match ")

foldS f z []     = z 
foldS f z (x:xs) = foldS f z xs >>>= f x 
---f x =>>> foldS f z xs


{- 
Replace all list constructors in a list expression
by the constructors of the wrapped list and wrap it.
-}
foldPatList :: Pos -> [Exp Id] -> DbgTransMonad (Exp Id)

foldPatList pos [] =  wrapR pos =>>> lookupCon pos t_List
foldPatList pos (p:ps) = 
    lookupCon pos t_Colon >>>= \consid ->
    wrapR pos =>>> 
    (unitS (\c cs -> ExpApplication pos [consid, c, cs]) 
           =>>> dPat p =>>> foldPatList pos ps) 


{- Wrap a pattern with R constructor and new variable as trace argument -}
wrapR :: Pos -> DbgTransMonad (Pat Id -> Pat Id)

wrapR pos =
    lookupCon pos t_R >>>= \r ->
    newVar pos >>>= \wc ->
    unitS (\p -> ExpApplication pos [r, p, wc])
    

-- Rename the variables in a pattern. Return the new pattern
-- and an association list from the old names to the new ones.
-- Used in transformation of pattern bindings.
patVars :: Pat Id -> DbgTransMonad (Pat Id, [((Pos,Id),Pat Id)])

patVars (ExpApplication pos ps) = unzipConc (ExpApplication pos) ps
patVars p@(ExpCon pos id)       = unitS (p, [])
patVars (ExpVar pos id)         = newVar pos >>>= \nide -> 
                                  unitS (nide, [((pos, id), nide)])
patVars p@(ExpLit pos lit)      = unitS (p, [])
patVars (ExpList pos ps)        = unzipConc (ExpList pos) ps
patVars (PatAs pos id p)        = newVar pos >>>= \nide@(ExpVar _ nid) ->
                                  patVars p >>>= \(p', vs) ->
				  unitS (PatAs pos nid p', ((pos, id), nide):vs)
patVars p@(PatWildcard pos)     = unitS (p, []) 
patVars (PatIrrefutable pos p)  = unzipConc (PatIrrefutable pos . head) [p]
patVars e                       = error ("patVars: no match")


unzipConc :: ([Exp Id] -> a) -> [Exp Id] 
          -> DbgTransMonad (a,[((Pos,Id),Pat Id)])
unzipConc f ps = 
    mapS patVars ps >>>= \psvss ->
    let (ps', vss) = unzip psvss
    in unitS (f ps', concat vss)
    

{- Utility functions --------------------------------------------------------}

makeTuple pos [e] = unitS e
makeTuple pos es = 
    lookupId Con (t_Tuple (length es)) >>>= \tup ->
    unitS (ExpApplication pos (ExpCon pos tup : es))


lookupId kind ident = 
  \(Inherited _ _ lookupPrel) s -> (lookupPrel (ident, kind), s)
lookupVar pos ident =  
  \(Inherited _ _ lookupPrel) s -> (ExpVar pos (lookupPrel (ident, Var)), s)
lookupCon pos ident =  
  \(Inherited _ _ lookupPrel) s -> (ExpCon pos (lookupPrel (ident, Con)), s)
lookupName ident = 
  \_ s@(Threaded istate _ _) -> (lookupIS istate ident, s)
lookupNameStr ident = 
  \_ s@(Threaded istate _ _) -> (strIS istate ident, s)


{- Create a new variable with given position -}
newVar :: Pos -> DbgTransMonad (Exp Id)

newVar pos = \_ (Threaded istate srt idt) ->
                 case uniqueIS istate of
	             (i, istate') -> (ExpVar pos i, Threaded istate' srt idt)


{- Create a list of n new variables, all with the same given position -}
newVars :: Pos -> Int -> DbgTransMonad [Exp Id]

newVars pos n = \_ (Threaded istate srt idt) ->
                    case uniqueISs istate [1..n] of
	                (is, istate') -> (map (ExpVar pos . snd) is, 
			                  Threaded istate' srt idt)

{-
Make info for a variable with given Id, name, arity and type.
Right Id: Id is added to name
Left Id: name used as given 
-}
mkInfo :: Either Id Id -> String -> Int -> NewType -> Info

mkInfo (Right u) str arity nt = 
    InfoVar u (visImpRev (str ++ "_" ++ show u)) (InfixDef, 9) 
      IEnone nt (Just arity)

mkInfo (Left u) str arity nt = 
    InfoVar u (visImpRev str) (InfixDef, 9) IEnone nt (Just arity)


{-
Wraps off most of the SRs, Traces and Rs.
e.g. 
unwrapNT 0 True (SR -> Trace -> R Bool) = R Bool
unwrapNT 0 False (SR -> Trace -> R Bool) = Trace -> R Bool
unwrapNT 1 False (SR -> Trace -> R(Trace -> R Int -> R Bool)) = 
  Trace -> R Int -> R Bool
unwrapNT 2 False 
  (SR -> Trace -> R(Trace -> R Int -> R(Trace -> R Char -> R Int))) =
  Trace -> R Int -> R Char -> R Int

Assumes that input type has form
  SR -> Trace -> R (tyn)
  tyn = Trace -> R (any type) -> R (ty(n-1))
  ty0 = any type
Expands type synonyms if necessary to obtain this type
-}
unwrapNT :: IntState -> Int -> Bool -> NewType -> NewType

unwrapNT intState arity isCaf nt@NoType = nt
unwrapNT intState arity isCaf 
  (NewType free exist ctxs [NTcons arrow [sr, NTcons _ [t, rt]]]) = 
    NewType free exist ctxs (if isCaf 
                               then [dStripR arity rt] 
                               else [NTcons arrow [t, dStripR arity rt]])
  where 
  dStripR 0 t = t
  dStripR n (NTcons rt [NTcons a1 [t, NTcons a2 [a, b]]]) 
    | a1 == arrow && a1 == a2 = NTcons arrow [a, dStripR (n-1) b]
  dStripR n (NTcons rt [NTcons tysyn tys]) = 
    -- type may contain type synonym instead of the function arrow
    dStripR n (NTcons rt [expand nt tys])
    where
    nt = dropJust . typeSynonymBodyI . dropJust . lookupIS intState $ tysyn

    -- the expand version in TypeUnify does not work here because it uses
    -- idempotent closure of the substitution
    expand :: NewType -> [NT] -> NT
    expand (NewType free [] ctxs [nt]) ts = substNT (zip free ts) nt
  dStripR n t = error ("dStripR: strange type: " ++ show t)
unwrapNT intState arity isCaf nt = 
  error ("unwrapNT: strange type: " ++ show nt)


{-
Create a new identifier with given arity, name and type.
Boolean argument decides if Id is appended to name.
-}
addNewName :: Int -> Bool -> String -> NewType -> DbgTransMonad Id

addNewName arity addIdNr str nt = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let info = mkInfo (if addIdNr then Right i else Left i) str arity nt
	    istate'' = addIS i info istate'
	in (i, Threaded istate'' srt idt)

{-
Create a new primitive identifier with given Info, changing just the
location in the table (i.e. the lookup key).
-}
addNewPrim :: Info -> DbgTransMonad Id

addNewPrim (InfoVar _ (Qualified m nm) fix ie nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let newNm = Qualified m (packString (unpackPS nm++"'"))
            info' = InfoVar i newNm fix IEnone NoType ar
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)
addNewPrim (InfoVar _ nm fix ie nt ar) = 
  error ("In tracing transformation: foreign import has unqualified name?")

{-
Create a new identifier for the prim-wrapper, given prim Info, in a fresh
location in the table
-}
addNewWrapper :: Info -> DbgTransMonad Id

addNewWrapper (InfoVar _ nm fix ie nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let info' = InfoVar i nm fix ie NoType (Just 2)
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)

{-
Overwrite the original primitive identifier with new Info, reflecting
the change in type and arity.
-}
overwritePrim :: Id -> Inherited -> Threaded -> Threaded

overwritePrim i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i nm fix ie _ _) = InfoVar i nm fix ie NoType (Just 2)
      in Threaded (updateIS istate i updI) srt idt

{-
Overwrite the original primitive identifier with new name.
-}
overwriteOrigName :: Id -> Inherited -> Threaded -> Threaded
overwriteOrigName i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i (Qualified m f) fix ie nt ar) =
                InfoVar i (Qualified m (packString ('\'': unpackPS f)))
                                          fix ie nt ar
      in Threaded (updateIS istate i updI) srt idt



setD :: Exp Id -> DbgTransMonad Inherited

setD d = \(Inherited _ f lookupPrel) s -> 
           (Inherited d f lookupPrel, s) 


getD :: DbgTransMonad (Exp Id)

getD = \(Inherited d _ _) s -> (d, s)


{- old, were not used anywhere
getModStr = \(Inherited _ _ _ modstr) s@(Threaded istate _ _) -> (modstr, s)
setModStr modstr = \(Inherited d f lp _) s -> (Inherited d f lp modstr, s)
-}


getConArity id = \_ s@(Threaded istate _ _) -> (arityIS istate id, s)

getIdArity id = 
    \_ s@(Threaded istate _ _) ->
    case lookupIS istate id of
        Nothing -> (Nothing, s)
	Just info -> (Just (arityVI info){-(arityIS istate id)-}, s)



getArity :: [Fun a] -> Int

getArity (Fun pats _ _ : _) = length pats


setArity :: Int -> Int -> a -> Threaded -> Threaded

setArity arity id  = \inh (Threaded (IntState unique rps st errors) srt idt) ->
  let newid = case lookupAT st id of
                Just (InfoMethod u tid fix nt _ cls) ->
                      InfoMethod u tid fix nt (Just arity) cls
                Just (InfoDMethod u tid nt _ cls) ->
                      InfoDMethod u tid nt (Just arity) cls
       	        Just (InfoIMethod u tid nt _ cls) ->
	              InfoIMethod u tid nt (Just arity) cls
	        Just (InfoVar u tid fix exp nt _) ->
	             InfoVar u tid fix exp nt (Just arity)
  in Threaded (IntState unique rps (updateAT st id (\_ -> newid)) errors) 
       srt idt
{-
setArity arity id = \_ (Threaded istate srs) ->
                        Threaded (updVarArity noPos id arity istate) srs
-}


getPositions :: DbgTransMonad (Int,[Int])
getPositions = \_ s@(Threaded _ srt _) -> (srt, s)


setPositions :: (Int,[Int]) -> a -> Threaded -> Threaded
setPositions srt = \_ s@(Threaded istate _ idt) -> Threaded istate srt idt


{-
Make a node expression of type Trace, NmType or SR.
-}

makeSourceRef :: Pos -> DbgTransMonad (SRExp)

makeSourceRef p (Inherited _ _ lookupPrel) s@(Threaded is (nsr, srs) idt) =
  if rowcol == head srs then
    (ExpApplication p [ExpVar p sr3, ExpLit p (LitInt Boxed nsr)], s)
  else
    let nsr' = nsr+1
    in seq nsr' (ExpApplication p [ExpVar p sr3, ExpLit p (LitInt Boxed nsr')],
                 Threaded is (nsr', rowcol:srs) idt)
  where 
  (row, col) = fromPos p 
  rowcol = 10000*row + col -- this should probably be done by some function
                           -- mkSR :: Int -> Int -> SR
  sr3 = lookupPrel (t_mkSR', Var)



makeNTId :: Pos -> Id -> DbgTransMonad (NmTypeExp)

makeNTId pos id = 
  lookupVar pos t_mkNTId' >>>= \mkNTId' ->
  unitS $ ExpApplication pos [mkNTId',ExpLit pos (LitInt Boxed id)]
        

makeNTConstr :: Pos -> Id -> DbgTransMonad (NmTypeExp)

makeNTConstr pos id =
  lookupVar pos t_mkNTConstr' >>>= \mkNTConstr' ->
  unitS $ ExpApplication pos [mkNTConstr',ExpLit pos (LitInt Boxed id)]


makeNTLambda :: Pos -> DbgTransMonad (NmTypeExp)

makeNTLambda pos = lookupVar pos t_mkNTLambda


makeNTCase :: Pos -> DbgTransMonad (NmTypeExp)

makeNTCase pos = lookupVar pos t_mkNTCase


makeNm :: Pos -> TraceExp -> NmTypeExp -> SRExp -> DbgTransMonad (TraceExp)

makeNm p parent name sr =
  lookupVar noPos t_mkTNm >>>= \nm ->
  unitS $ ExpApplication p [nm, parent, name, sr] 

{-
Add identifier with position to threaded list.
-}
addId :: (Pos,Id) -> a -> Threaded -> Threaded

addId pid inh (Threaded is srt idt) = Threaded is srt (pid:idt)


{- is it a class method? -}
isCMethod :: Info -> Bool

isCMethod (InfoIMethod _ _ _ _ _) = True
isCMethod (InfoDMethod _ _ _ _ _) = True
isCMethod _ = False

{- End Module DbgTrans ------------------------------------------------------}
