{- ---------------------------------------------------------------------------
-- Transforms all value definitions of a program 
-- to produce traces for debugging.
-}
module DbgTrans(SRIDTable, debugTrans, dbgAddImport, LevelId(..)) where

import Extra(Pos, noPos, pair, fromPos, strPos, dropJust, trace)
import IdKind(IdKind(Con,Var,TCon))
import TokenId
import DbgId(t_R,tSR,tTrace,t_mkTRoot,t_mkTNm 
            ,t_rseq,t_myseq,t_fatal,t_rPatBool
            ,t_lazySat,t_lazySatLonely,t_eagerSat
            ,t_fun,t_tfun,t_primn,t_tprimn
            ,t_prim,t_ap,t_rap,t_tap,t_trap
            ,t_cn,t_con,t_pa,t_tcon,t_tpa,t_indir
            ,t_mkNoSR,t_mkSR',t_mkNTId',t_mkNTConstr',t_mkNTLambda,t_mkNTCase
            ,t_mkNTGuard,t_mkNTIf
            ,t_mkTHidden,t_mkTAp
            ,t_conInt,t_conChar,t_conInteger,t_conRational,t_conDouble
            ,t_conFloat,t_conCons,t_tfun
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
import Info(typeSynonymBodyI,IE(IEsel))
import TypeSubst(substNT)
import Nice(niceNewType)
import Remove1_3(mkSel,translateExpRecord) -- for records
import Flags(Flags(sDbgTrusted))
import Remove1_3(mkSel)
import List  -- (zipWith3)


{- table for source references and identifiers refered to from the trace -}
type SRIDTable = Maybe ((Int,[Pos])        -- source reference table
                       ,[LevelId (Pos,Id)] -- identifier table
                       ,[ImpDecl TokenId]  -- import declarations
                       ,String)            -- module name

{- distinguish top-level identifiers from local definitions -}
data LevelId a = TopId a | LocalId a


-- newNameVar, nameArity, hsFromInt, addInt

data Inherited = Inherited 
                   ((TokenId, IdKind) -> Id) -- lookupPrel

data Threaded = Threaded 
                  IntState 
                  (Int, [Pos]) -- source reference table, 
                               -- accumulated for SRIDTable
                               -- first is number of current source reference
                               -- second is list of encoded source references
                  [LevelId (Pos, Id)]
                               -- identifier table, accumulated for SRIDTable


type DbgTransMonad a = State Inherited Threaded a Threaded

type TraceExp = Exp Id    -- expression of type Trace
type NmTypeExp = Exp Id   -- expression of type NmType
type SRExp = Exp Id       -- expression of type SR


{- obtain the internal state -}
getIntState :: DbgTransMonad IntState
getIntState inherited threaded@(Threaded intState _ _) = (intState,threaded)
 
setIntState :: IntState -> Inherited -> Threaded -> Threaded
setIntState intState inherited threaded@(Threaded _ srt idt) = 
  Threaded intState srt idt

{-
-- Used to add the special prelude for debugging to the import list.
-- Now it serves no purpose and is just the identity function.
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
-- Transform all value definitions to produce traces for debugging.
-}
debugTrans :: Flags
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
  initDebugTranslate (dTopDecls (sDbgTrusted flags) root decls) istate 
    lookupPrel
  where 
  initDebugTranslate f istate lookupPrel = 
    case f (Inherited lookupPrel) 
           (Threaded istate (1, [0]) (map TopId constrs)) of
      (decls', Threaded istate' srt idt) -> 
        (decls', istate', Just (srt, idt, impdecls, reverse (unpackPS modidl)))
  root :: Exp Id
  root = ExpVar noPos (lookupPrel (t_mkTRoot, Var)) 
         -- data constructor Root of the R type 
           

dTopDecls :: Bool -> Exp Id -> Decls Int -> DbgTransMonad (Decls Int)

dTopDecls trusted root (DeclsParse ds) = 
  mapS (dTopDecl trusted root) ds >>>= \dss -> 
  unitS (DeclsParse (concat dss))


dTopDecl :: Bool -> Exp Id -> Decl Int -> DbgTransMonad [Decl Int]

dTopDecl trusted root (DeclClass pos ctx id1 id2 (DeclsParse decls)) =
  mapS ((if trusted then dTrustDecl else dSuspectDecl) True root) decls 
    >>>= \decls' ->
  lookupName id1 >>>= \(Just (InfoClass i tid ie nt ms ds at)) ->
  mapS0 fixMethodArity (zip ms ds) >>>
  unitS (DeclClass pos ctx id1 id2 (DeclsParse (concatMap fst decls'))
        :concatMap snd decls')
  where 
  fixMethodArity (m, d) =
    lookupName d >>>= \(Just (InfoDMethod _ _ _ (Just arity) _)) ->
    setArity 2 {-arity-} m
dTopDecl trusted root d@(DeclInstance pos ctx id inst (DeclsParse decls)) = 
  mapS ((if trusted then dTrustDecl else dSuspectDecl) True root) decls 
    >>>= \decls' ->
  unitS (DeclInstance pos ctx id inst (DeclsParse (concatMap fst decls'))
        :concatMap snd decls')
dTopDecl _ _ d@(DeclType id t) = unitS [d]
dTopDecl _ _ d@(DeclData mb ctx id contrs tycls) = unitS [d]
dTopDecl trusted root d@(DeclConstrs pos id constrids) = 
  -- derive field selector definitions as usual and subsequently transform them
  getIntState >>>= \intState ->
  case mapS mkSel constrids () ([],intState) of
    (selDecls,(_,intState')) -> 
      setIntState intState' >>>
      mapS ((if trusted then dTrustDecl else dSuspectDecl) True root) selDecls 
        >>>= \selDeclss' ->
      unitS (DeclConstrs pos id [] 
            :concatMap (\(ds',ads') -> ds'++ads') selDeclss')
dTopDecl True root (DeclFun pos id [Fun [] rhs localDecls]) =
  -- for top-level trusted caf create a name node,
  -- because it might be called from suspected code;
  -- this is not expensive, because it is only done once per caf.
  addTopId (pos, id) >>>
  lookupName id >>>= \(Just info) ->
  lookupNameStr id >>>= \cafName ->
  getIntState >>>= \intState ->
  let nt = (unwrapNT intState 0 True True (ntI info)) in
  setArity 2 id >>>
  addNewName 0 True cafName nt >>>= \nid ->
  addNewName 0 True cafName nt >>>= \nid2 ->
  addNewName 0 True "nt" NoType >>>= \useParentId ->
  let useParent = ExpVar pos useParentId in
  addNewName 0 True "nt" NoType >>>= \hidUseParentId ->
  let hidUseParent = ExpVar pos hidUseParentId in
  lookupId Var t_otherwise >>>= \otherw ->
  lookupId Con tTrue >>>= \true ->
  dTrustRhs False useParent hidUseParent true otherw rhs >>>= \rhs' ->
  dTrustDecls False hidUseParent localDecls >>>= \localDecls' ->
  makeSourceRef noPos >>>= \noSR ->
  makeNTId pos id >>>= \ntId ->
  makeNm pos root ntId noSR >>>= \nte ->
  lookupVar pos t_lazySat >>>= \lazySat ->
  lookupVar pos t_mkTHidden >>>= \mkTHidden ->
  unitS [-- id _ _ = nid
         DeclFun pos id 
           [Fun [PatWildcard pos, PatWildcard pos] 
              (Unguarded (ExpVar pos nid)) (DeclsParse [])]
        ,-- nid = lazySat nid2 t'
         DeclFun pos nid
           [Fun [] 
             (Unguarded (ExpApplication pos 
                           [lazySat, (ExpVar pos nid2), useParent]))
             (DeclsParse [])
           ]
        ,{- 
          nid2 = rhs'
            where
            decls'
          -}
         DeclFun pos nid2
           [Fun [] rhs' localDecls']
        ,{- t' = Nm redex (NTId id) sr -}
         DeclFun pos useParentId
           [Fun [] (Unguarded nte) (DeclsParse [])]
        ,{- t'' = mkTHidden t' -}
         DeclFun pos hidUseParentId
           [Fun [] (Unguarded (ExpApplication pos [mkTHidden,useParent])) 
             (DeclsParse [])]
        ]
dTopDecl True root (DeclPat (Alt pat rhs decls)) =
  -- for top-level trusted pattern bindings create name node for
  -- each variable, because it might be called from suspected code.
  -- here no indirections, because the fact that the trusted variables
  -- are defined by pattern binding should not be visible.
  addNewName 0 True "_pv" NoType >>>= \patid ->
  --trace ("patid = " ++ show patid) $
  setArity 0 patid >>>
  patVars pat >>>= \(pat', bvsnvs) ->
  let bvsposids = map fst bvsnvs -- (pos,id)s of original var 
      (bvspos,bvsids) = unzip bvsposids 
      pos = head bvspos in
  mapS0 (setArity 2) bvsids >>>  
  mapS0 addTopId bvsposids >>>
  lookupId Var t_otherwise >>>= \otherw ->
  lookupId Con tTrue >>>= \true ->
  dTrustRhs True root root true otherw rhs >>>= \rhs' ->
  dPat root pat' >>>= \pat'' ->
  dTrustDecls False root decls >>>= \decls' ->
  mkFailExpr pos root >>>= \fe ->
  let evars = map snd bvsnvs in 
  makeTuple noPos evars >>>= \etup ->
  zipWithS makeNTId bvspos bvsids >>>= \ntIds ->
  makeSourceRef noPos >>>= \noSR ->
  zipWithS (makeNm pos root) ntIds (repeat noSR) >>>= \nms ->
  addNewName 0 True "nt" NoType >>>= \t' ->
  lookupVar pos t_lazySat >>>= \lazySat ->
  addNewName 0 True "rhs" NoType >>>= \rhsId ->
  let
    prhs = DeclFun noPos rhsId [Fun [] rhs' (DeclsParse [])]
    pcase = ExpCase noPos (ExpVar noPos rhsId)
                  [Alt pat'' (Unguarded etup) (DeclsParse [])
	          ,Alt (PatWildcard noPos) (Unguarded fe) (DeclsParse [])]
    pfun = DeclFun noPos patid [Fun [] (Unguarded pcase) decls']
    vpat p pv i = vcase p pv
    vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup (Unguarded pv) (DeclsParse [])]
    vfun ((p, i), pv) nte = 
          DeclFun p i 
            [Fun [PatWildcard p, PatWildcard p] 
               (Unguarded (ExpApplication pos 
                            [lazySat, (vpat p pv i), ExpVar p t'] )) 
               (DeclsParse
                 [DeclFun p t' [Fun [] (Unguarded nte) (DeclsParse [])]])
            ]
  in unitS (zipWith vfun bvsnvs nms ++ [pfun,prhs])
dTopDecl trusted root d = 
  (if trusted then dTrustDecl else dSuspectDecl) True root d
      >>>= \(ds',auxDs') ->
  unitS (ds' ++ auxDs') 


dSuspectDecls :: Bool -> Exp Id -> Decls Id -> DbgTransMonad (Decls Id)

dSuspectDecls toplevel parent (DeclsParse ds) = 
  mapS (dSuspectDecl toplevel parent) ds >>>= \(dss) -> 
  unitS (DeclsParse (concatMap (\(ds',ads') -> ds'++ads') dss))
dSuspectDecls toplevel parent (DeclsScc []) =
  unitS (DeclsParse [])			-- introduced by translateRecordExp


dTrustDecls :: Bool -> Exp Id -> Decls Id -> DbgTransMonad (Decls Id)

dTrustDecls toplevel parent (DeclsParse ds) = 
  mapS (dTrustDecl toplevel parent) ds >>>= \dss -> 
  unitS (DeclsParse (concatMap (\(ds',ads') -> ds'++ads') dss))
dTrustDecls toplevel parent (DeclsScc []) =  -- introduced by translateRecordExp
  unitS (DeclsParse [])


{-
-- A definition is transformed into definition(s) that define 
-- the same variable(s) as the original one, 
-- and possibly additional auxilary definitions
-- that are needed to enable sharing. The auxilary definitions are returned
-- in the second declaration list.
-}
dSuspectDecl :: Bool -> Exp Id -> Decl Id -> DbgTransMonad ([Decl Id],[Decl Id])

dSuspectDecl toplevel parent (DeclPat (Alt v@(ExpVar pos id) rhs decls)) =
  -- may occur because of the next equation
  dSuspectDecl toplevel parent (DeclFun pos id [Fun [] rhs decls])
dSuspectDecl toplevel parent (DeclPat (Alt (PatAs pos id p) rhs decls)) =
  dSuspectDecl toplevel parent (DeclFun pos id [Fun [] rhs decls])
    >>>= \(dc1,dc2) ->
  dSuspectDecl toplevel parent 
    (DeclPat (Alt p (Unguarded (ExpVar pos id)) (DeclsParse [])))
    >>>= \(dp1,dp2) ->
  unitS (dc1++dp1,dc2++dp2)
dSuspectDecl toplevel parent (DeclPat (Alt pat rhs decls)) =
  addNewName 0 True "_pv" NoType >>>= \patid ->
  --trace ("patid = " ++ show patid) $
  setArity 0 patid >>>
  patVars pat >>>= \(pat', bvsnvs) ->
  let bvsposids = map fst bvsnvs -- (pos,id)s of original var 
      (bvspos,bvsids) = unzip bvsposids 
      pos = head bvspos in
  mapS0 (setArity 2) bvsids >>>  
  mapS0 (if toplevel then addTopId else addId) bvsposids >>>
  mapS makeSourceRef bvspos >>>= \srs ->
  dSuspectRhs True parent rhs failContinuation >>>= \rhs' ->
  dPat parent pat' >>>= \pat'' ->
  let ExpApplication _ [r,_,tresult] = pat'' in
  dSuspectDecls False parent decls >>>= \decls' ->
  mkFailExpr pos parent >>>= \fe ->
  let evars = map snd bvsnvs in 
  makeTuple noPos evars >>>= \etup ->
  let wrapetup = ExpApplication noPos [r,etup,tresult] in
  zipWithS makeNTId bvspos bvsids >>>= \ntIds ->
  zipWithS (makeNm pos parent) ntIds srs >>>= \nms ->
  lookupVar pos t_lazySat >>>= \lazySat ->
  lookupVar pos t_indir >>>= \indir ->
  addNewName 0 True "nt" NoType >>>= \t' ->
  let
    pcase = ExpCase noPos rhs' 
                  [Alt pat'' (Unguarded wrapetup) (DeclsParse [])
	          ,Alt (PatWildcard noPos)   (Unguarded fe)  (DeclsParse [])]
    pfun = DeclFun noPos patid [Fun [] (Unguarded pcase) decls']
    vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt wrapetup 
                         (Unguarded (ExpApplication noPos [indir,tresult,pv]))
                         (DeclsParse [])]
    vfun ((p, i), pv) sr nte = 
          DeclFun p i 
            [Fun [PatWildcard p, PatWildcard p] 
               (Unguarded (ExpApplication pos 
                            [lazySat, (vcase p pv), ExpVar p t'] )) 
               (DeclsParse 
                 [DeclFun p t' [Fun [] (Unguarded nte) (DeclsParse [])]])
            ]
  in unitS (zipWith3 vfun bvsnvs srs nms,[pfun])
dSuspectDecl toplevel parent d@(DeclFun pos id fundefs) = 
  (if toplevel then addTopId else addId) (pos, id) >>>
  lookupName id >>>= \(Just info) ->
  lookupNameStr id >>>= \funName ->
  --trace ("DeclFun: funName = " ++ funName ++ ", arity " ++ show info) $
  if isCMethod info 
    then dMethod parent False info pos id funName fundefs
    else
      getIntState >>>= \intState ->
      let arity = getArity fundefs
      in case arity of
           0 -> dSuspectCaf parent pos id funName fundefs 
                  (unwrapNT intState 0 True False (ntI info))
           _ -> dSuspectFun pos id funName arity fundefs 
                  (unwrapNT intState arity False False (ntI info))
dSuspectDecl toplevel parent d@(DeclForeignImp pos cname id' arity cast typ id)=
  (if toplevel then addTopId else addId) (pos, id) >>>
  lookupName id' >>>= \(Just info) ->
--lookupNameStr id' >>>= \funName ->
--trace ("DeclForeignImp: " ++ funName ++", info=\n" ++ show info) $
  -- generate code for the wrapper
  dSuspectForeignImp pos id id' arity >>>= \code->
  -- get real cname from primed hname (f'), if needed
  let (InfoVar _ (Qualified _ f) _ _ _ _) = info
      cname' = if null cname then reverse (tail (unpackPS f)) else cname
  in unitS ([DeclFun pos id [code]]
           ,[ DeclForeignImp pos cname' id' arity cast typ id])
dSuspectDecl toplevel parent decl = dDecl decl


{- trusted version of dSuspectDecl -}
dTrustDecl :: Bool -> Exp Id -> Decl Id -> DbgTransMonad ([Decl Id],[Decl Id])

dTrustDecl toplevel hidParent (DeclPat (Alt pat rhs decls)) =
  -- here no indirections, because the fact that the trusted variables
  -- are defined by pattern binding should not be visible.
  addNewName 0 True "_pv" NoType >>>= \patid ->
  --trace ("patid = " ++ show patid) $
  setArity 0 patid >>>
  patVars pat >>>= \(pat', bvsnvs) ->
  let bvsposids = map fst bvsnvs -- (pos,id)s of original var 
      (bvspos,bvsids) = unzip bvsposids 
      pos = head bvspos in
  mapS0 (setArity 2) bvsids >>>  
  mapS0 (if toplevel then addTopId else addId) bvsposids >>>
  lookupId Var t_otherwise >>>= \otherw ->
  lookupId Con tTrue >>>= \true ->
  dTrustRhs True hidParent hidParent true otherw rhs >>>= \rhs' ->
  dPat hidParent pat' >>>= \pat'' ->
  dTrustDecls False hidParent decls >>>= \decls' ->
  mkFailExpr pos hidParent >>>= \fe ->
  let evars = map snd bvsnvs in 
  makeTuple noPos evars >>>= \etup ->
  lookupVar pos t_lazySatLonely >>>= \lazySat ->
  addNewName 0 True "rhs" NoType >>>= \rhsId ->
  let
    prhs = DeclFun noPos rhsId [Fun [] rhs' (DeclsParse [])]
    pcase = ExpCase noPos (ExpVar noPos rhsId)
                  [Alt pat'' (Unguarded etup) (DeclsParse [])
	          ,Alt (PatWildcard noPos) (Unguarded fe) (DeclsParse [])]
    pfun = DeclFun noPos patid [Fun [] (Unguarded pcase) decls']
    vpat p pv i = vcase p pv
    vcase p pv = ExpCase p (ExpVar p patid) 
                       [Alt etup (Unguarded pv) (DeclsParse [])]
    vfun ((p, i), pv) = 
          DeclFun p i 
            [Fun [PatWildcard p, PatWildcard p] 
               (Unguarded (ExpApplication pos 
                            [lazySat, (vpat p pv i), hidParent] )) 
               (DeclsParse [])
            ]
  in unitS (map vfun bvsnvs,[pfun,prhs])
dTrustDecl toplevel hidParent d@(DeclFun pos id fundefs) = 
  (if toplevel then addTopId else addId) (pos, id) >>>
  lookupName id >>>= \(Just info) ->
  lookupNameStr id >>>= \funName ->
  --trace ("DeclFun: funName = " ++ funName ++ ", arity " ++ show info) $
  if isCMethod info 
    then dMethod hidParent True info pos id funName fundefs
    else
      getIntState >>>= \intState ->
      lookupId Var t_undef >>>= \undefined ->
      let arity = getArity fundefs
      in case arity of
           0 | id /= undefined -> dTrustCaf hidParent pos id funName fundefs 
                                    (unwrapNT intState 0 True True (ntI info))
             -- don't consider `undefined' as caf to ensure that it
             -- has the caller as parent
             -- sharing not important anyway
           _ -> dTrustFun pos id funName arity fundefs 
                  (unwrapNT intState arity False True (ntI info))
dTrustDecl toplevel _ d@(DeclForeignImp pos cname id' arity cast typ id) =
  (if toplevel then addTopId else addId) (pos, id) >>>
  lookupName id' >>>= \(Just info) ->
--lookupNameStr id' >>>= \funName ->
--trace ("DeclForeignImp: " ++ funName ++", info=\n" ++ show info) $
  -- generate code for the wrapper
  dTrustForeignImp pos id id' arity >>>= \code->
  -- get real cname from primed hname (f'), if needed
  let (InfoVar _ (Qualified _ f) _ _ _ _) = info
      cname' = if null cname then reverse (tail (unpackPS f)) else cname
  in unitS ([DeclFun pos id [code]]
           ,[DeclForeignImp pos cname' id' arity cast typ id])
dTrustDecl toplevel _ decl = dDecl decl


{- The common trivial cases: -}
dDecl :: Decl Id -> DbgTransMonad ([Decl Id],[Decl Id])

dDecl d@(DeclTypeRenamed _ _) = unitS ([d],[])
dDecl d@(DeclDefault tys) = unitS ([d],[])
dDecl d@(DeclVarsType vars ctx ty) = unitS ([d],[]) 
dDecl d@(DeclIgnore _) = unitS ([d],[])
dDecl d@(DeclError _) = unitS ([d],[])
dDecl d@(DeclAnnot _ _) = unitS ([d],[])
dDecl d@(DeclFixity _) = unitS ([d],[])
dDecl d@(DeclPrimitive pos id i ty) = unitS ([d],[])
dDecl d@(DeclForeignExp pos cname id typ) =
  error ("Can't trace foreign exports yet. "++strPos pos)


dMethod :: Exp Id -> Bool -> Info -> Pos -> Id -> String -> [Fun Id] 
        -> DbgTransMonad ([Decl Id],[Decl Id])

dMethod parent trusted info@(InfoIMethod _ tid nt (Just arity) _) pos id 
  funName [Fun [] (Unguarded (ExpVar p1 d)) (DeclsParse [])] = 
  -- This must(?) be a wrapper to the method of the superclass
  -- or default(?)
  -- you don't want to see the explicit call in the trace
  -- this pattern matching is definitly dangerous:
  -- it may succeed even for user defined methods
    setArity 2 id >>>
    newVars pos 2 >>>= \[sr, useParent] ->
    unitS ([DeclFun pos id [Fun [sr, useParent]
                               (Unguarded 
                                 (ExpApplication p1 
                                   [ExpVar p1 d, sr, useParent]))
           		       (DeclsParse [])]]
          ,[])
dMethod parent trusted info pos id funName fundefs = 
  let arity = getArity fundefs
  in case arity of
       0 -> (if trusted then dTrustCaf else dSuspectCaf)
              parent pos id funName fundefs NoType
       _ -> (if trusted then dTrustFun else dSuspectFun) 
              pos id funName (getArity fundefs) fundefs NoType


dSuspectCaf :: Exp Id -> Pos -> Id -> String -> [Fun Id] -> NewType 
            -> DbgTransMonad ([Decl Id],[Decl Id])

dSuspectCaf parent pos id cafName [Fun [] rhs localDecls] nt =
  setArity 2 id >>>
  addNewName 0 True "nt" NoType >>>= \useParentId ->
  let useParent = ExpVar pos useParentId in
  addNewName 0 True cafName nt >>>= \nid ->
  dSuspectRhs False useParent rhs failContinuation >>>= \rhs' ->
  dSuspectDecls False useParent localDecls >>>= \(DeclsParse localDeclsList') ->
  makeSourceRef pos >>>= \sr ->
  makeNTId pos id >>>= \ntId ->
  makeNm pos parent ntId sr >>>= \nte ->
  lookupVar pos t_lazySat >>>= \lazySat ->
  unitS (-- id _ _ = nid
         [DeclFun pos id 
           [Fun [PatWildcard pos, PatWildcard pos] 
              (Unguarded (ExpVar pos nid)) (DeclsParse [])
           ]]
        ,{- 
         id = lazySat e t'
           where
           t' = Nm redex (NTId id) sr
           decls
         -}
         [DeclFun pos nid 
            [Fun [] 
              (Unguarded (ExpApplication pos [lazySat, rhs', useParent]))
	      (DeclsParse
	         (DeclFun pos useParentId 
                    [Fun [] (Unguarded nte) (DeclsParse [])] 
                 :localDeclsList'
                 )
              )
            ]
         ])
dSuspectCaf parent pos id cafName _ nt =
  error ("Variable " ++ cafName ++ " multiple defined.")
  -- actually nhc should produce such an error already before;
  -- however, currently accepts such definitions. 


dTrustCaf :: Exp Id -> Pos -> Id -> String -> [Fun Id] -> NewType 
          -> DbgTransMonad ([Decl Id],[Decl Id])

dTrustCaf hidParent pos id cafName [Fun [] rhs localDecls] nt =
  setArity 2 id >>>
  addNewName 0 True cafName nt >>>= \nid ->
  addNewName 0 True cafName nt >>>= \nid2 ->
  lookupId Var t_otherwise >>>= \otherw ->
  lookupId Con tTrue >>>= \true ->
  dTrustRhs False hidParent hidParent true otherw rhs >>>= \rhs' ->
  dTrustDecls False hidParent localDecls >>>= \localDecls' ->
  lookupVar pos t_lazySatLonely >>>= \lazySat ->
  unitS (-- id _ _ = nid
         [DeclFun pos id 
           [Fun [PatWildcard pos, PatWildcard pos] 
              (Unguarded (ExpVar pos nid)) (DeclsParse [])
           ]]
        ,-- nid = lazySat nid2 t'
         [DeclFun pos nid
           [Fun [] 
             (Unguarded (ExpApplication pos 
                           [lazySat, (ExpVar pos nid2), hidParent]))
             (DeclsParse [])
           ]
         ,{- 
          nid2 = rhs'
            where
            decls'
          -}
          DeclFun pos nid2
           [Fun [] rhs' localDecls']
        ])
dTrustCaf hidParent pos id cafName _ nt =
  error ("Variable " ++ cafName ++ " multiple defined.")
  -- actually nhc should produce such an error already before;
  -- however, currently accepts such definitions. 


dSuspectFun :: Pos -> Id -> String -> Int -> [Fun Id] -> NewType
            -> DbgTransMonad ([Decl Id],[Decl Id])

dSuspectFun pos id funName arity fundefs nt =
  lookupVar pos (t_fun arity) >>>= \fun ->
  setArity 2 id >>>
  newVar pos >>>= \parent ->
  newVar pos >>>= \sr ->
  addNewName (arity+1) True funName nt >>>= \wrappedfun ->	    
  newVars pos (arity+1) >>>= \(newParent:fp) ->
  makeNTId pos id >>>= \ntid ->
  lookupId Var t_otherwise >>>= \otherw ->
  lookupId Con tTrue >>>= \true ->
  checkSuspectPrimitive newParent fundefs >>>= \prim ->
  (case prim of
    Nothing -> dSuspectFunClauses newParent funName arity true otherw
                 fundefs
    Just fundefs -> unitS (fundefs, [])
  ) >>>= \(fundefs', newdecls) ->
  getIntState >>>= \intState ->
  unitS ([DeclFun pos id [Fun [sr, parent]
           (Unguarded 
             (ExpApplication pos 
               [fun, ntid, ExpVar pos wrappedfun, sr, parent]
           ))
           (DeclsParse 
             (prependTypeSigIfExists intState pos wrappedfun
               (DeclFun pos wrappedfun fundefs' : newdecls)))
         ]],[])
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


dTrustFun :: Pos -> Id -> String -> Int -> [Fun Id] -> NewType
            -> DbgTransMonad ([Decl Id],[Decl Id])

dTrustFun pos id funName arity fundefs nt =
  lookupVar pos (t_tfun arity) >>>= \fun ->
  setArity 2 id >>>
  newVar pos >>>= \parent ->
  newVar pos >>>= \sr ->
  addNewName (arity+2) True funName nt >>>= \wrappedfun ->	    
  newVars pos (arity+2) >>>= \(useParent:hidUseParent:fp) ->
  makeNTId pos id >>>= \ntid ->
  lookupId Con tTrue >>>= \true ->
  lookupId Var t_otherwise >>>= \otherwise ->
  checkTrustPrimitive useParent hidUseParent fundefs >>>= \prim ->
  (case prim of
    Nothing -> 
      dTrustFunClauses useParent hidUseParent arity true otherwise fundefs
    Just fundefs -> unitS fundefs
  ) >>>= \fundefs' ->
  getIntState >>>= \intState ->
  unitS ([DeclFun pos id [Fun [sr, parent]
    (Unguarded 
      (ExpApplication pos 
        [fun, ntid, ExpVar pos wrappedfun, sr, parent]
      ))
      (DeclsParse 
        (prependTypeSigIfExists intState pos wrappedfun
          [DeclFun pos wrappedfun fundefs']))
    ]],[])
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


dTrustFunClauses :: Exp Id -> Exp Id -> Int -> Id -> Id -> [Fun Id] 
                 -> DbgTransMonad [Fun Id]

dTrustFunClauses parent hidParent arity true otherw funs =
  mapS dTrustFun funs >>>= \funs' ->
  let vars = parent : hidParent : replicate arity (PatWildcard noPos) in
  mkFailExpr noPos parent >>>= \fpexp ->
  unitS (funs' ++ [Fun vars (Unguarded fpexp) (DeclsParse [])])
  where
  dTrustFun (Fun pats rhs decls) =
    dPats hidParent pats >>>= \pats' ->
    dTrustRhs False parent hidParent true otherw rhs >>>= \rhs' ->
    dTrustDecls False hidParent decls >>>= \decls' ->
    unitS (Fun (parent:hidParent:pats') rhs' decls') 


{-
-- For each clause of the function definition e, return a transformed
-- definition e', and possibly declare a new auxiliary function to handle
-- failure across guards.
-- Note, the parent trace is slightly misused for passing a variable.
-}
dSuspectFunClauses :: Exp Id -> String -> Int -> Id -> Id -> [Fun Id] 
                   -> DbgTransMonad ([Fun Id],[Decl Id])

dSuspectFunClauses parent funName arity true otherw [] = 
  -- catch the case that all patterns and guards fail
  let vars = parent : replicate arity (PatWildcard noPos) in
  mkFailExpr noPos parent >>>= \fpexp ->
  unitS ([Fun vars (Unguarded fpexp) (DeclsParse [])],[])
-- No guards is the easiest case.
--     f pat1 pat2 ... = e  where decls
-- ==>
--     f t pat1' pat2' ... = e' where decls'
dSuspectFunClauses parent funName arity true otherw
  (Fun pats (Unguarded e) decls : fcs) =
    dPats parent pats >>>= \pats' ->
    dSuspectExp False parent e >>>= \e' ->
    dSuspectDecls False parent decls >>>= \decls' ->
    dSuspectFunClauses parent funName arity true otherw fcs 
      >>>= \(mfs, nfs) ->
    unitS (Fun (parent:pats') (Unguarded e') decls' : mfs, nfs)
dSuspectFunClauses parent funName arity true otherw 
  (Fun pats (Guarded ges) decls : fcs)
  | not (null fcs) && canFail true otherw ges =
    -- for the remaining clauses a new function has to be defined
    -- which is called if all guards fail;
    -- necessary, because the trace which registers all failed guards
    -- has to be passed to this function to not to lose this information
    mapS namePat pats >>>= \namedpats ->
    let (patnames, pats') = unzip namedpats in
    addNewName (arity + 1) True funName NoType >>>= \f ->
    newVar noPos >>>= \newParent ->
    dPats newParent pats' >>>= \pats'' ->
    let continuation = functionContinuation f patnames in
    continuationToExp continuation newParent >>>= \contExp ->
    dSuspectGuardedExprs False newParent ges continuation >>>= \expr ->
    dSuspectDecls False newParent decls >>>= \decls' ->
    let failclause = Fun (newParent:patnames) (Unguarded contExp) 
                         (DeclsParse []) in
    dSuspectFunClauses parent funName arity true otherw fcs >>>= \(mfs, nfs) ->
    unitS ([Fun (newParent:pats'') 
            (Unguarded expr) decls'
            , failclause]
          , DeclFun noPos f mfs:nfs)
  | otherwise =
    -- guards cannot fail or last clause
    dPats parent pats >>>= \pats' ->
    dSuspectGuardedExprs False parent ges failContinuation >>>= \e ->
    dSuspectDecls False parent decls >>>= \decls' ->
    dSuspectFunClauses parent funName arity true otherw fcs >>>= \(mfs, nfs) ->
    let fs = Fun (parent:pats') 
               (Unguarded e) decls' in
    unitS (fs:mfs, nfs)


{-
-- To correctly create the trace within guards, a continuation is used.
-- The type ContExp should be abstract. Its implementation is only used in 
-- the following three functions.
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
-- Create body of wrapper for imported foreign function.
-- id is the new function we are declaring; id' is the real foreign function
-}
dSuspectForeignImp :: Pos -> Id -> Id -> Int -> DbgTransMonad (Fun Id)

dSuspectForeignImp pos id id' arity =
  lookupVar pos (t_primn arity) >>>= \primn ->
  makeNTId pos id >>>= \ntid ->
  setArity 2 id >>>
  newVar pos >>>= \useParent ->
  newVar pos >>>= \sr ->
  unitS (Fun [sr, useParent]
             (Unguarded
               (ExpApplication pos 
                 [primn, ntid, ExpVar pos id', sr, useParent]))
             (DeclsParse []))


dTrustForeignImp :: Pos -> Id -> Id -> Int -> DbgTransMonad (Fun Id)

dTrustForeignImp pos id id' arity =
  lookupVar pos (t_tprimn arity) >>>= \primn ->
  makeNTId pos id >>>= \ntid ->
  setArity 2 id >>>
  newVar pos >>>= \useParent ->
  newVar pos >>>= \sr ->
  unitS (Fun [sr, useParent]
             (Unguarded
               (ExpApplication pos 
                 [primn, ntid, ExpVar pos id', sr, useParent]))
             (DeclsParse []))


{-
-- The continuation is used if all guards fail.
-}
dSuspectGuardedExprs :: Bool -> Exp Id -> [(Exp Id,Exp Id)] -> ContExp 
                     -> DbgTransMonad (Exp Id)

dSuspectGuardedExprs cr parent [] cont = 
  continuationToExp cont parent
dSuspectGuardedExprs cr parent ((g, e):ges) cont = 
  let pos = getPos g in
  newVar pos >>>= \newParent ->
  let ExpVar _ newParentId = newParent in
  dSuspectGuardedExprs cr newParent ges cont >>>= \ges' -> 
  -- newParent good? probably conditions should have function app as parent
  dSuspectExp True parent g >>>= \g' ->
  dSuspectExp cr newParent e >>>= \e' ->
  makeSourceRef pos >>>= \sr ->
  lookupCon pos t_R >>>= \r ->
  newVar pos >>>= \gr ->
  newVar pos >>>= \gt ->
  lookupVar pos (t_mkTAp 2) >>>= \mkTAp2 ->
  lookupVar pos t_mkTNm >>>= \mkTNm ->
  lookupVar pos t_mkNTGuard >>>= \mkNTGuard ->
  lookupVar pos t_myseq >>>= \myseq ->
  unitS (ExpCase pos g' 
          [Alt (ExpApplication pos [r,gr,gt])
            (Unguarded 
              (ExpLet pos 
                (DeclsParse 
                  [DeclFun pos newParentId 
                    [Fun [] 
                      (Unguarded 
                        (ExpApplication pos 
                          [mkTAp2,parent
                          ,ExpApplication pos [mkTNm,parent,mkNTGuard,sr]
                          ,gt,parent,sr]))
                      (DeclsParse [])]])
                (ExpApplication pos [myseq,newParent,ExpIf pos gr e' ges'])))
            (DeclsParse [])])
  -- case [[g]]^True_parent of
  --   R gr gt -> 
  --     let newParent = mkTAp2 parent (mkTNm parent mkNTGuard sr) gt parent sr
  --     in newParent `myseq` if gr 
  --                             then [[e]]^cr_newParent 
  --                             else [[ges]]^cont_newParent



{- Used for combining fail continuation with a trace -}
addAppTrace :: Exp Id -> TraceExp -> Exp Id
addAppTrace (ExpApplication pos (f:es)) t = ExpApplication pos (f:t:es)
addAppTrace f t = ExpApplication noPos [f, t]


{- 
-- obtain a variable that names the given pattern;
-- easy for variable pattern or as pattern; otherwise produces as pattern
-}
namePat :: Pat Id -> DbgTransMonad (Exp Id,Pat Id)

namePat e@(ExpVar p v) = unitS (e, e)
namePat e@(PatAs p v pat) = unitS (ExpVar p v, e)
namePat pat = 
    newVar noPos >>>= \e@(ExpVar _ v) -> unitS (e, PatAs noPos v pat)


{-
-- Returns False only if the one of the guards definitely has value True.
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


{- call of a C function which is of wrapped type -}

checkSuspectPrimitive :: Exp Id -> [Fun Id] -> DbgTransMonad (Maybe [Fun Id])

checkSuspectPrimitive parent
  [Fun ps (Unguarded (ExpApplication pos (ExpVar p id:f:es))) decls] =
    lookupId Var t_prim >>>= \primid ->
    -- does this really refer to "_prim" in DebugPrelude ?
    if id == primid then
	lookupVar pos t_rseq >>>= \rseq ->
--	let expr = ExpApplication pos (f:redex:es)
	let expr = foldr (\n e -> ExpApplication pos [rseq, n, e]) 
	                 (ExpApplication pos (f:parent:es))
			 es
        in unitS (Just [Fun (parent:ps) (Unguarded expr) decls])
    else
        unitS Nothing
checkSuspectPrimitive _ _ = unitS Nothing


checkTrustPrimitive :: Exp Id -> Exp Id -> [Fun Id] 
                    -> DbgTransMonad (Maybe [Fun Id])

checkTrustPrimitive parent hidParent
  [Fun ps (Unguarded (ExpApplication pos (ExpVar p id:f:es))) decls] =
    lookupId Var t_prim >>>= \primid ->
    -- refers to "_prim" in DebugPrelude
    if id == primid then
	lookupVar pos t_rseq >>>= \rseq ->
	let expr = foldr (\n e -> ExpApplication pos [rseq, n, e]) 
	                 (ExpApplication pos (f:parent:es))
			 es
        in unitS (Just [Fun (parent:hidParent:ps) (Unguarded expr) decls])
    else
        unitS Nothing
checkTrustPrimitive _ _ _ = unitS Nothing


mkFailExpr :: Pos -> Exp Id -> DbgTransMonad (Exp Id)
mkFailExpr pos parent =
  lookupVar pos t_fatal >>>= \fatal ->
  unitS (ExpApplication pos [fatal, parent])


dSuspectRhs :: Bool -> Exp Id -> Rhs Id -> ContExp -> DbgTransMonad (Exp Id)
dSuspectRhs cr parent (Unguarded exp) cont = dSuspectExp cr parent exp
dSuspectRhs cr parent (Guarded gdExps) cont = 
  dSuspectGuardedExprs cr parent gdExps cont


{-
-- First argument False iff the parent is equal to this expression, i.e.,
-- the result of this expression is the same as the result of the parent.
-}


dSuspectExp :: Bool -> Exp Id -> Exp Id -> DbgTransMonad (Exp Id)

dSuspectExp cr parent (ExpLambda pos pats e) = 
  newVar pos >>>= \lambdaParent ->
  newVars pos (length pats) >>>= \npats ->
  dSuspectExp False lambdaParent e >>>= \e' ->
  lookupVar pos (t_fun (length pats)) >>>= \fun ->
  makeNTLambda pos >>>= \lambda ->
  dPats lambdaParent pats >>>= \pats' ->
  newVar pos >>>= \fpat ->
  mkFailExpr pos lambdaParent
    {-Wrong or misleading error message -} >>>= \fpexp ->
  makeSourceRef pos >>>= \sr ->
  makeTuple pos npats >>>= \npatstup ->
  makeTuple pos pats' >>>= \patstup ->
  let lamexp = 
        if neverFailingPats pats 
          then ExpLambda pos (lambdaParent:pats') e'
          else ExpLambda pos (lambdaParent:npats) (ExpCase pos npatstup alts)
      alts = [Alt patstup (Unguarded e') (DeclsParse []),
              Alt fpat (Unguarded fpexp) (DeclsParse [])] 
  in unitS (ExpApplication pos [fun, lambda, lamexp, sr, parent])
dSuspectExp cr parent (ExpLet pos decls e) = 
  unitS (ExpLet pos) =>>> 
    dSuspectDecls False parent decls =>>> 
    dSuspectExp cr parent e
dSuspectExp cr parent (ExpCase pos e alts) =  
  lookupId Con tTrue >>>= \true ->
  lookupId Var t_otherwise >>>= \otherw ->
  lookupVar pos ((if cr then t_ap else t_rap) 1) >>>= \apply ->
  lookupVar pos (t_fun 1) >>>= \fun ->
  makeNTCase pos >>>= \casenm ->
  dSuspectExp True parent e >>>= \e' ->
  newVar noPos >>>= \caseParent ->
  makeSourceRef pos >>>= \sr ->
  dSuspectFunClauses caseParent "case" 1 true otherw 
    (map alt2Fun alts) >>>= \(fun', defs') ->
  addNewName 2 True "case" NoType >>>= \fid ->
  unitS $
    ExpApplication pos 
      [apply, sr, parent
      ,ExpApplication pos 
	[fun 
	,casenm
        ,ExpLet pos (DeclsParse (DeclFun pos fid fun' : defs')) 
          (ExpVar pos fid)
	,sr,parent]
      ,e']
dSuspectExp cr parent (ExpIf pos co e1 e2) = 
  dSuspectExp True parent co >>>= \co' ->
  newVar pos >>>= \ifParent ->
  let ExpVar _ ifParentId = ifParent in
  dSuspectExp False ifParent e1 >>>= \e1' ->
  dSuspectExp False ifParent e2 >>>= \e2' ->
  makeSourceRef pos >>>= \sr ->
  lookupCon pos t_R >>>= \r ->
  newVar pos >>>= \cor ->
  newVar pos >>>= \cot ->
  lookupVar pos (t_mkTAp 2) >>>= \mkTAp2 ->
  lookupVar pos t_mkTNm >>>= \mkTNm ->
  lookupVar pos t_mkNTIf >>>= \mkNTIf ->
  lookupVar pos (if cr then t_lazySat else t_eagerSat) >>>= \makeSat ->
  unitS (ExpCase pos co' 
          [Alt (ExpApplication pos [r,cor,cot])
            (Unguarded 
              (ExpLet pos 
                (DeclsParse 
                  [DeclFun pos ifParentId 
                    [Fun [] 
                      (Unguarded 
                        (ExpApplication pos 
                          [mkTAp2,parent
                          ,ExpApplication pos [mkTNm,parent,mkNTIf,sr]
                          ,cot,parent,sr]))
                      (DeclsParse [])]])
                (ExpApplication pos [makeSat,ExpIf pos cor e1' e2',ifParent])))
            (DeclsParse [])])
  -- case [[g]]^True_parent of
  --   R gr gt -> 
  --     let ifParent = mkTAp2 parent (mkTNm parent mkNTGuard sr) gt parent sr
  --     in makeSat 
  --          (if gr 
  --             then [[e1]]^False_ifParent 
  --             else [[e2]]^False_ifParent)
  --          ifParent
dSuspectExp cr parent (ExpType pos e ctx t) = 
  unitS (\e' -> ExpType pos e' ctx t) =>>> dSuspectExp cr parent e
dSuspectExp cr parent (ExpApplication pos (f:es)) = 
  case f of
    ExpCon _ _ ->            
      dSuspectExps parent es >>>= \es' ->
      saturateSuspectConstr parent f es'
    _ ->
      lookupVar pos ((if cr then t_ap else t_rap) (length es)) >>>= \apply ->
      makeSourceRef pos >>>= \sr -> 
      dSuspectExps parent (f:es) >>>= \fes ->
      unitS (ExpApplication pos (apply:sr:parent:fes))
dSuspectExp cr parent (ExpList pos es) = 
  lookupCon pos t_Colon >>>= \consid ->
  dSuspectExps parent es >>>= \es' ->
  foldS 
    (\e es -> wrapSuspectConst parent consid [e, es])
    (lookupCon pos t_List >>>= \nil -> wrapSuspectConst parent nil []) es'
dSuspectExp cr parent e@(ExpCon pos id) = saturateSuspectConstr parent e []
dSuspectExp cr parent e@(ExpVar pos id) = 
  getIdArity id >>>= \arity ->
  case arity of
    Nothing -> -- Must be a lambdabound variable
      if cr 
        then unitS e
	else -- the parent is a projection
	  lookupVar pos t_indir >>>= \indir ->
	  unitS (ExpApplication pos [indir, parent, e])
    Just n -> -- A letbound or global function
      patchFieldSelectorType id >>>
      makeSourceRef pos >>>= \sr ->
      unitS (ExpApplication pos [e, sr, parent]) 
dSuspectExp cr parent (ExpRecord exp fields) =
  getIntState >>>= \state ->
  case translateExpRecord exp fields state of
    (Right transExp,state') -> setIntState state' >>> 
                               dSuspectExp cr parent transExp
    (Left errorMsg,state') -> error errorMsg  
dSuspectExp cr parent (PatWildcard pos) = 
  -- introduced by translateExpRecord (meaning = undefined)
  lookupVar pos t_undef >>>= \undefined ->
  dSuspectExp cr parent undefined
dSuspectExp cr parent e@(ExpLit pos (LitString _ s)) = 
  -- calling a combinator `litString pos s' impossible, because
  -- the list data type (s) cannot be used there.
  -- The following is somewhat expensive. But it works...
  -- At least the list constructor (:) is shared in the trace.
  lookupCon pos t_Colon >>>= \cons ->
  lookupCon pos t_List >>>= \nil ->
  lookupVar pos t_conCons >>>= \conCons ->
  makeSourceRef pos >>>= \sr -> 
  let ExpCon _ consId = cons in
  makeNTConstr pos consId >>>= \ntconstr ->
  makeNm pos parent ntconstr sr >>>= \consName ->
  addNewName 0 True "consTrace" NoType >>>= \consTrId ->
  dSuspectExp cr parent nil >>>= \nil' ->
  let cons' :: Char -> Exp Id -> Exp Id
      cons' c rs = ExpApplication pos [conCons, sr, parent, cons
                                      ,ExpVar pos consTrId
                                      ,ExpLit pos (LitChar Boxed c), rs]
  in unitS $ ExpLet pos 
               (DeclsParse [DeclFun pos consTrId 
                             [Fun [] (Unguarded consName) (DeclsParse []) ]]) 
               (foldr cons' nil' s)
{- old: Simpler, but leads to slightly larger expressions.
    lookupCon pos t_Colon >>>= \consid ->
    lookupCon pos t_List >>>= \nilid ->
    let rs = (map (ExpLit pos . LitChar Boxed) s) in
    dExp True (foldr (\c cs -> ExpApplication pos [consid, c, cs]) nilid rs)
-}
dSuspectExp cr parent e@(ExpLit pos (LitInteger b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConInteger >>>= \fci ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fci, sr, parent, e])
dSuspectExp cr parent e@(ExpLit pos (LitRational b i)) = 
    -- Remove this after typechecking
    lookupVar pos t_fromConRational >>>= \fcr ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [fcr, sr, parent, e])
dSuspectExp cr parent e@(ExpLit pos lit) = 
    dLit lit >>>= \constr ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [constr, sr, parent, e])
    where dLit (LitInt _ _) = lookupVar pos t_conInt
          dLit (LitChar _ _) = lookupVar pos t_conChar
          dLit (LitInteger _ _) = lookupVar pos t_conInteger
	  dLit (LitRational _ _) = lookupVar pos t_conRational
	  dLit (LitDouble _ _) = lookupVar pos t_conDouble
	  dLit (LitFloat _ _) = lookupVar pos t_conFloat
dSuspectExp cr parent e = error ("dExp: no match")


dSuspectExps :: Exp Id -> [Exp Id] -> DbgTransMonad [Exp Id]

dSuspectExps parent es = mapS (dSuspectExp True parent) es


{-
-- First argument False, if the unevaluated expression never appears as
-- an argument in the trace. Hence not Sat needs to be created.
-- False does not imply that the expression is equal to its parent
-- expression, unlike for the suspected case.
-}

dTrustExp :: Bool -> Exp Id -> Exp Id -> Exp Id -> DbgTransMonad (Exp Id)

dTrustExp cr parent hidParent (ExpLambda pos pats e) = 
  newVar pos >>>= \lambdaParent ->
  newVar pos >>>= \lambdaHidParent ->
  newVars pos (length pats) >>>= \npats ->
  dTrustExp False lambdaParent lambdaHidParent e >>>= \e' ->
  lookupVar pos (t_tfun (length pats)) >>>= \fun ->
  makeNTLambda pos >>>= \lambda ->
  dPats lambdaHidParent pats >>>= \pats' ->
  newVar pos >>>= \fpat ->
  mkFailExpr pos lambdaParent 
    {-Wrong or misleading error message -} >>>= \fpexp ->
  makeSourceRef pos >>>= \sr ->
  makeTuple pos npats >>>= \npatstup ->
  makeTuple pos pats' >>>= \patstup ->
  let lamexp = 
        if neverFailingPats pats 
          then ExpLambda pos (lambdaParent:lambdaHidParent:pats') e'
          else ExpLambda pos (lambdaParent:lambdaHidParent:npats) 
                 (ExpCase pos npatstup alts)
      alts = [Alt patstup (Unguarded e') (DeclsParse []),
              Alt fpat (Unguarded fpexp) (DeclsParse [])] 
  in unitS (ExpApplication pos [fun, lambda, lamexp, sr
                               ,if cr then hidParent else parent])
     {- actually know that lambda expr appears in trusted code and
        hence no name has to be created; however, would need another
        set of combinators t_ttfun that do not test trustedness of parent -}
dTrustExp cr parent hidParent (ExpLet pos decls e) = 
  unitS (ExpLet pos) =>>> 
    dTrustDecls False hidParent decls =>>> 
    dTrustExp cr parent hidParent e
dTrustExp cr parent hidParent (ExpCase pos e alts) = 
  dTrustExp cr parent hidParent e >>>= \e' ->
  lookupId Con tTrue >>>= \true ->
  lookupId Var t_otherwise >>>= \otherw ->
  mapS (dTrustAlt cr parent hidParent true otherw) alts >>>= \alts' ->
  mkFailExpr pos (if cr then hidParent else parent)
    {-Wrong or misleading error message -} >>>= \fpexp ->
  unitS $ ExpCase pos e' (alts' ++ [Alt (PatWildcard pos) 
                                     (Unguarded fpexp) (DeclsParse [])])
dTrustExp cr parent hidParent (ExpIf pos c e1 e2) = 
  dTrustExp cr parent hidParent c >>>= \c' ->
  dTrustExp cr parent hidParent e1 >>>= \e1' ->
  dTrustExp cr parent hidParent e2 >>>= \e2' ->
  lookupCon pos tTrue >>>= \true ->
  lookupCon pos tFalse >>>= \false ->
  lookupCon pos t_R >>>= \r ->
  let expCase = ExpCase pos c' 
                  [Alt (ExpApplication pos [r,true,PatWildcard pos]) 
                       (Unguarded e1') (DeclsParse [])
                  ,Alt (ExpApplication pos [r,false,PatWildcard pos])
                       (Unguarded e2') (DeclsParse [])] in
  lookupVar pos t_lazySatLonely >>>= \lazySat ->
  unitS 
    (if cr then (ExpApplication pos [lazySat,expCase,hidParent]) else expCase)
dTrustExp cr parent hidParent (ExpType pos e ctx t) = 
  unitS (\e' -> ExpType pos e' ctx t) =>>> dTrustExp cr parent hidParent e
dTrustExp cr parent hidParent (ExpApplication pos (f:es)) = 
  case f of
    ExpCon _ _ ->            
      dTrustExps parent hidParent es >>>= \es' ->
      saturateTrustConstr (if cr then hidParent else parent) f es'
    _ ->
      lookupVar pos ((if cr then t_tap else t_trap) (length es)) >>>= \apply ->
      dTrustExps parent hidParent (f:es) >>>= \fes ->
      unitS (ExpApplication pos 
               (apply:(if cr then hidParent else parent):fes))
dTrustExp cr parent hidParent (ExpList pos es) = 
  lookupCon pos t_Colon >>>= \consid ->
  dTrustExps parent hidParent es >>>= \es' ->
  foldS 
    (\e es -> wrapTrustConst hidParent consid [e, es])
    (lookupCon pos t_List >>>= \nil -> wrapTrustConst hidParent nil []) es'
dTrustExp cr parent hidParent e@(ExpCon pos id) = 
  saturateTrustConstr (if cr then hidParent else parent) e []
dTrustExp cr parent hidParent e@(ExpVar pos id) = 
  getIdArity id >>>= \arity ->
  case arity of
    Nothing -> -- Must be a lambdabound variable
      if cr 
        then unitS e
	else 
	  lookupVar pos t_indir >>>= \indir ->
	  unitS (ExpApplication pos [indir, parent, e])
    Just n -> -- A letbound or global function
      patchFieldSelectorType id >>>
      makeSourceRef noPos >>>= \noSR ->
      unitS (ExpApplication pos 
              [e, noSR, if cr then hidParent else parent]) 
dTrustExp cr parent hidParent (ExpRecord exp fields) =
  getIntState >>>= \state ->
  case translateExpRecord exp fields state of
    (Right transExp,state') -> setIntState state' >>> 
                               dTrustExp cr parent hidParent transExp
    (Left errorMsg,state') -> error errorMsg  
dTrustExp cr parent hidParent (PatWildcard pos) = 
  -- introduced by translateExpRecord (meaning = undefined)
  lookupVar pos t_undef >>>= \undefined ->
  dTrustExp cr parent hidParent undefined
dTrustExp cr parent hidParent e@(ExpLit pos (LitString _ s)) = 
  -- calling a combinator `litString pos s' impossible, because
  -- the list data type (s) cannot be used there.
  -- The following is somewhat expensive. But it works...
  -- At least the list constructor (:) is shared in the trace.
  lookupCon pos t_Colon >>>= \cons ->
  lookupCon pos t_List >>>= \nil ->
  lookupVar pos t_conCons >>>= \conCons ->
  let ExpCon _ consId = cons in
  makeNTConstr pos consId >>>= \ntconstr ->
  makeSourceRef noPos >>>= \noSR ->
  let tParent = if cr then hidParent else parent in
  makeNm pos tParent ntconstr noSR >>>= \consName ->
  addNewName 0 True "consTrace" NoType >>>= \consTrId ->
  dTrustExp cr parent hidParent nil >>>= \nil' ->
  let cons' :: Char -> Exp Id -> Exp Id
      cons' c rs = ExpApplication pos [conCons, noSR, tParent, cons
                                      ,ExpVar pos consTrId
                                      ,ExpLit pos (LitChar Boxed c), rs]
  in unitS $ ExpLet pos 
               (DeclsParse [DeclFun pos consTrId 
                             [Fun [] (Unguarded consName) (DeclsParse []) ]]) 
               (foldr cons' nil' s)
{- old: Simpler, but leads to slightly larger expressions.
    lookupCon pos t_Colon >>>= \consid ->
    lookupCon pos t_List >>>= \nilid ->
    let rs = (map (ExpLit pos . LitChar Boxed) s) in
    dExp True (foldr (\c cs -> ExpApplication pos [consid, c, cs]) nilid rs)
-}
dTrustExp cr parent hidParent e@(ExpLit pos (LitInteger b i)) = 
  -- Remove this after typechecking
  lookupVar pos t_fromConInteger >>>= \fci ->
  makeSourceRef noPos >>>= \noSR ->
  unitS (ExpApplication pos [fci, noSR, if cr then hidParent else parent, e])
dTrustExp cr parent hidParent e@(ExpLit pos (LitRational b i)) = 
  -- Remove this after typechecking
  lookupVar pos t_fromConRational >>>= \fcr ->
  makeSourceRef noPos >>>= \noSR ->
  unitS (ExpApplication pos [fcr, noSR, if cr then hidParent else parent, e])
dTrustExp cr parent hidParent e@(ExpLit pos lit) = 
    dLit lit >>>= \constr ->
    makeSourceRef noPos >>>= \noSR ->
    unitS (ExpApplication pos 
             [constr, noSR, if cr then hidParent else parent, e])
    where dLit (LitInt _ _) = lookupVar pos t_conInt
          dLit (LitChar _ _) = lookupVar pos t_conChar
          dLit (LitInteger _ _) = lookupVar pos t_conInteger
	  dLit (LitRational _ _) = lookupVar pos t_conRational
	  dLit (LitDouble _ _) = lookupVar pos t_conDouble
	  dLit (LitFloat _ _) = lookupVar pos t_conFloat
dTrustExp cr parent hidParent e = error ("dExp: no match")



dTrustExps :: Exp Id -> Exp Id -> [Exp Id] -> DbgTransMonad [Exp Id]
dTrustExps parent hidParent es = mapS (dTrustExp True parent hidParent) es


dTrustAlt :: Bool -> Exp Id -> Exp Id -> Id -> Id -> Alt Id
                -> DbgTransMonad (Alt Id)

dTrustAlt cr parent hidParent true otherw (Alt pat rhs decls) =
  dPat hidParent pat >>>= \pat' ->
  dTrustRhs cr parent hidParent true otherw rhs >>>= \rhs' ->
  dTrustDecls False hidParent decls >>>= \decls' ->
  unitS (Alt pat' rhs' decls')


dTrustRhs :: Bool -> Exp Id -> Exp Id -> Id -> Id -> Rhs Id 
          -> DbgTransMonad (Rhs Id)

dTrustRhs cr parent hidParent true otherw (Unguarded exp) = 
  dTrustExp cr parent hidParent exp >>>= \exp' ->
  unitS (Unguarded exp')
dTrustRhs cr parent hidParent true otherw (Guarded gdExps) = 
  mapS dTrustGuardedExpr gdExps >>>= \gdExps' ->
  unitS (Guarded gdExps')
  where
  dTrustGuardedExpr :: (Exp Id, Exp Id) -> DbgTransMonad (Exp Id, Exp Id)
  dTrustGuardedExpr (cond@(ExpCon _ id), e) | id == true =
    dTrustExp cr parent hidParent e >>>= \e' ->
    unitS (cond, e')
  dTrustGuardedExpr (cond@(ExpVar pos id), e) | id == otherw =
    dTrustExp cr parent hidParent e >>>= \e' ->
    unitS (ExpCon pos true, e')  -- transformed "otherwise" has different type
  dTrustGuardedExpr (cond,e) =
    dTrustExp cr parent hidParent cond >>>= \cond' ->
    dTrustExp cr parent hidParent e >>>= \e' ->
    lookupVar noPos t_rPatBool >>>= \rPatBool ->
    unitS (ExpApplication noPos [rPatBool,cond'],e')


{- conversion of case alternative into function definition alternative -}
alt2Fun :: Alt a -> Fun a
alt2Fun (Alt pat rhs decls) = Fun [pat] rhs decls 


{-
-- Transform data constructor application.
-- Number of arguments may be smaller than arity of the data constructor.
-}
saturateSuspectConstr :: 
                  Exp Id      -- parent
               -> Exp Id      -- data constructor
               -> [Exp Id]    -- arguments (already transformed)
               -> DbgTransMonad (Exp Id) -- transformed constructor application

saturateSuspectConstr parent c@(ExpCon pos id) args =
  --trace ("<<< " ++ show id' ++ " -> " ++ show id) $
  getConArity id >>>= \arity ->
  --trace ("Arity for " ++ show id ++ " is " ++ show arity) $
  if arity > length args then -- Unsaturated constructor
    makeNTConstr pos id >>>= \ntconstr ->
    lookupVar pos (t_cn (arity - length args)) >>>= \cn ->
    lookupVar pos (t_pa (length args)) >>>= \pan ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos (pan:c:cn:sr:parent:ntconstr:args))
   else
    wrapSuspectConst parent c args


saturateTrustConstr :: 
                  Exp Id      -- parent
               -> Exp Id      -- data constructor
               -> [Exp Id]    -- arguments (already transformed)
               -> DbgTransMonad (Exp Id) -- transformed constructor application

saturateTrustConstr parent c@(ExpCon pos id) args =
  --trace ("<<< " ++ show id' ++ " -> " ++ show id) $
  getConArity id >>>= \arity ->
  --trace ("Arity for " ++ show id ++ " is " ++ show arity) $
  if arity > length args then -- Unsaturated constructor
    makeNTConstr pos id >>>= \ntconstr ->
    lookupVar pos (t_cn (arity - length args)) >>>= \cn ->
    lookupVar pos (t_tpa (length args)) >>>= \pan ->
    unitS (ExpApplication pos (pan:c:cn:parent:ntconstr:args))
   else
    wrapTrustConst parent c args


{-
-- Transform constructor application where number of arguments
-- equals arity of constructor. The arguments have already been transformed.
-}
wrapSuspectConst :: Exp Id -> Exp Id -> [Exp Id] -> DbgTransMonad (Exp Id)
wrapSuspectConst parent c@(ExpCon pos cid) args =
  lookupVar pos (t_con (length args)) >>>= \con ->
  makeNTConstr pos cid >>>= \ntconstr ->
  makeSourceRef pos >>>= \sr ->
  unitS (ExpApplication pos (con:sr:parent:c:ntconstr:args))


wrapTrustConst :: Exp Id -> Exp Id -> [Exp Id] -> DbgTransMonad (Exp Id)
wrapTrustConst parent c@(ExpCon pos cid) args =
  lookupVar pos (t_tcon (length args)) >>>= \con ->
  makeNTConstr pos cid >>>= \ntconstr ->
  unitS (ExpApplication pos (con:parent:c:ntconstr:args))


{- Unused, string is transformed on a per character basis 
mkLitString :: Pos -> Exp Id -> DbgTransMonad (Exp Id)
mkLitString pos s =
    getD >>>= \d ->
    lookupVar pos t_stringConst >>>= \stringConst  ->
    makeSourceRef pos >>>= \sr ->
    unitS (ExpApplication pos [stringConst, sr, d, s])
-}


dPats :: Exp Id -> [Pat Id] -> DbgTransMonad [Pat Id] 
dPats parent ps = mapS (dPat parent) ps


dPat :: Exp Id -> Pat Id -> DbgTransMonad (Pat Id)
dPat parent (ExpApplication pos (c:ps)) = 
  wrapR pos =>>> 
  (unitS (ExpApplication pos) =>>> (unitS (c:) =>>> dPats parent ps))
dPat _ p@(ExpCon pos id)              = wrapR pos =>>> unitS p
dPat _ p@(ExpVar pos id)              = unitS p 
dPat parent p@(ExpLit pos (LitInteger b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConInteger >>>= \pfci ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfci, sr, parent, p])
dPat parent p@(ExpLit pos (LitRational b i))= 
    -- Remove this after typechecking
    lookupVar pos t_patFromConRational >>>= \pfcr ->
    makeSourceRef pos >>>= \sr -> 
    unitS (ExpApplication pos [pfcr, sr, parent, p])
dPat parent p@(ExpLit pos (LitString _ s)) = 
    foldPatList parent pos (map (ExpLit pos . LitChar Boxed) s)
dPat _ p@(ExpLit pos lit)           = wrapR pos =>>> unitS p
dPat _ p@(ExpList pos [])           = wrapR pos =>>> unitS p
dPat parent (ExpList pos ps)        = foldPatList parent pos ps
dPat parent (PatAs pos id p)        = unitS (PatAs pos id) =>>> dPat parent p
dPat _ p@(PatWildcard pos)          = unitS p 
dPat parent (PatIrrefutable pos p)  = 
  dPat parent p >>>= \(ExpApplication pos' [r,p',t']) ->
  unitS (ExpApplication pos' [r, PatIrrefutable pos p', t'])
dPat parent (ExpRecord con@(ExpCon pos id) fieldPats) =
  wrapR pos =>>> (unitS (ExpRecord con) =>>> mapS dField fieldPats)
  where
  dField :: Field Id -> DbgTransMonad (Field Id)
  dField (FieldExp pos id pat) = unitS (FieldExp pos id) =>>> dPat parent pat
dPat _ e                            = error ("dPat: no match ")

foldS f z []     = z 
foldS f z (x:xs) = foldS f z xs >>>= f x 
---f x =>>> foldS f z xs


{- 
-- Replace all list constructors in a list expression
-- by the constructors of the wrapped list and wrap it.
-}
foldPatList :: Exp Id -> Pos -> [Exp Id] -> DbgTransMonad (Exp Id)
foldPatList _ pos [] =  wrapR pos =>>> lookupCon pos t_List
foldPatList parent pos (p:ps) = 
  lookupCon pos t_Colon >>>= \cons ->
  wrapR pos =>>> 
  (unitS (\c cs -> ExpApplication pos [cons, c, cs]) 
         =>>> dPat parent p =>>> foldPatList parent pos ps) 


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


neverFailingPat :: Pat Id -> Bool
neverFailingPat (ExpVar _ _) = True
neverFailingPat (PatAs _ _ pat) = neverFailingPat pat
neverFailingPat (PatIrrefutable _ _) = True
neverFailingPat (PatWildcard _ ) = True
neverFailingPat _ = False

neverFailingPats :: [Pat Id] -> Bool
neverFailingPats = all neverFailingPat


{- Utility functions --------------------------------------------------------}

makeTuple pos [e] = unitS e
makeTuple pos es = 
    lookupId Con (t_Tuple (length es)) >>>= \tup ->
    unitS (ExpApplication pos (ExpCon pos tup : es))


lookupId kind ident = 
  \(Inherited lookupPrel) s -> (lookupPrel (ident, kind), s)
lookupVar pos ident =  
  \(Inherited lookupPrel) s -> (ExpVar pos (lookupPrel (ident, Var)), s)
lookupCon pos ident =  
  \(Inherited lookupPrel) s -> (ExpCon pos (lookupPrel (ident, Con)), s)
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
-- Make info for a variable with given Id, name, arity and type.
-- Right Id: Id is added to name
-- Left Id: name used as given 
-}
mkInfo :: Either Id Id -> String -> Int -> NewType -> Info
mkInfo (Right u) str arity nt = 
    InfoVar u (visImport (str++"_"++show u)) IEnone (InfixDef, 9) 
            nt (Just arity)

mkInfo (Left u) str arity nt = 
    InfoVar u (visImport str) IEnone (InfixDef, 9) nt (Just arity)


{-
-- Wraps off most of the SRs, Traces and Rs.
-- e.g. 
-- unwrapNT 0 True  False (SR -> Trace -> R Bool) = R Bool
-- unwrapNT 0 False False (SR -> Trace -> R Bool) = Trace -> R Bool
-- unwrapNT 1 False False (SR -> Trace -> R(Trace -> R Int -> R Bool)) = 
--   Trace -> R Int -> R Bool
-- unwrapNT 2 False False
--   (SR -> Trace -> R(Trace -> R Int -> R(Trace -> R Char -> R Int))) =
--   Trace -> R Int -> R Char -> R Int
-- 
-- Assumes that input type has form
--   SR -> Trace -> R (tyn)
--   tyn = Trace -> R (any type) -> R (ty(n-1))
--   ty0 = any type
-- Expands type synonyms if necessary to obtain this type
-}
unwrapNT :: IntState -> Int -> Bool -> Bool -> NewType -> NewType

unwrapNT intState arity isCaf trusted nt@NoType = nt
unwrapNT intState arity isCaf trusted
  (NewType free exist ctxs [NTcons arrow [sr, NTcons _ [t, rt]]]) = 
    NewType free exist ctxs 
      (if isCaf 
         then [dStripR arity rt] 
         else if trusted 
                then [NTcons arrow [t, NTcons arrow [t, dStripR arity rt]]]
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
unwrapNT intState arity isCaf trusted nt = 
  error ("unwrapNT: strange type: " ++ show nt)


{-
-- Create a new identifier with given arity, name and type.
-- Boolean argument decides if Id is appended to name.
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
-- Create a new primitive identifier with given Info, changing just the
-- location in the table (i.e. the lookup key).
-}
addNewPrim :: Info -> DbgTransMonad Id
addNewPrim (InfoVar _ (Qualified m nm) ie fix nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let newNm = Qualified m (packString (unpackPS nm++"'"))
            info' = InfoVar i newNm IEnone fix NoType ar
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)
addNewPrim (InfoVar _ nm ie fix nt ar) = 
  error ("In tracing transformation: foreign import has unqualified name?")

{-
-- Create a new identifier for the prim-wrapper, given prim Info, in a fresh
-- location in the table
-}
addNewWrapper :: Info -> DbgTransMonad Id
addNewWrapper (InfoVar _ nm ie fix nt ar) = 
  \_ (Threaded istate srt idt) ->
    case uniqueIS istate of
      (i, istate') -> 
	let info' = InfoVar i nm ie fix NoType (Just 2)
	    istate'' = addIS i info' istate'
	in (i, Threaded istate'' srt idt)

{-
-- Overwrite the original primitive identifier with new Info, reflecting
-- the change in type and arity.
-}
overwritePrim :: Id -> Inherited -> Threaded -> Threaded
overwritePrim i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i nm ie fix _ _) = InfoVar i nm ie fix NoType (Just 2)
      in Threaded (updateIS istate i updI) srt idt

{-
-- Overwrite the original primitive identifier with new name.
-}
overwriteOrigName :: Id -> Inherited -> Threaded -> Threaded
overwriteOrigName i = 
  \_ (Threaded istate srt idt) ->
      let updI (InfoVar i (Qualified m f) ie fix nt ar) =
                InfoVar i (Qualified m (packString ('\'': unpackPS f)))
                                          ie fix nt ar
      in Threaded (updateIS istate i updI) srt idt


getConArity id = \_ s@(Threaded istate _ _) -> (arityIS istate id, s)


getIdArity id = 
    \_ s@(Threaded istate _ _) ->
    case lookupIS istate id of
        Nothing -> (Nothing, s)
	Just info -> (Just (arityVI info){-(arityIS istate id)-}, s)


{-
-- A bit of a hack.
-- The type of an imported field selector is derived by nhc from the
-- data type definition. However, the transformed selector has a different
-- type. Hence we test if a used variable is a selector. If it is, then
-- we transform the type (only the first time we come across it).
-}
patchFieldSelectorType :: Id -> Inherited -> Threaded -> Threaded
patchFieldSelectorType id =
  \(Inherited lookupPrel) s@(Threaded istate srt idt) ->
  case lookupIS istate id of
    Just info@(InfoVar un tok ie fix (NewType all ex ctx [rec,res]) _) ->
        -- id is field selector of record defined in imported module
        -- (IExtract.importField constructs the type of the selector
        --  in this form; there seem to be no types of other variables
        --  of this form)
      let arrow = lookupPrel (t_Arrow,TCon)
          sr = lookupPrel (tSR,TCon)
          trace = lookupPrel (tTrace,TCon)
          r = lookupPrel (t_R,TCon)
      in
      Threaded 
        (updateIS istate id 
          (\_ -> InfoVar un tok ie fix 
                   (NewType all ex ctx 
                     [NTcons arrow 
                       [NTcons sr [],NTcons arrow 
                         [NTcons trace [], NTcons r 
                           [NTcons arrow [NTcons trace [],NTcons arrow 
                             [NTcons r [rec],res]]]]]]) 
                    (Just 2))) srt idt
    _ -> s


getArity :: [Fun a] -> Int
getArity (Fun pats _ _ : _) = length pats


setArity :: Int -> Int -> a -> Threaded -> Threaded
setArity arity id  = \inh (Threaded (IntState unique rps st errors) srt idt) ->
  let newid = case lookupAT st id of
                Just (InfoMethod u tid ie fix nt _ cls) ->
                      InfoMethod u tid ie fix nt (Just arity) cls
                Just (InfoDMethod u tid nt _ cls) ->
                      InfoDMethod u tid nt (Just arity) cls
       	        Just (InfoIMethod u tid nt _ cls) ->
	              InfoIMethod u tid nt (Just arity) cls
	        Just (InfoVar u tid exp fix nt _) ->
	             InfoVar u tid exp fix nt (Just arity)
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


{- Make a node expression of type Trace, NmType or SR.  -}
makeSourceRef :: Pos -> DbgTransMonad (SRExp)
makeSourceRef p i@(Inherited lookupPrel) s@(Threaded is (nsr, srs) idt) 
  | p == noPos = lookupVar noPos t_mkNoSR i s
  | rowcol == head srs =
    (ExpApplication p [ExpVar p sr3, ExpLit p (LitInt Boxed nsr)], s)
  | otherwise =
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


{- Add identifier with position to threaded list.  -}
addId :: (Pos,Id) -> a -> Threaded -> Threaded
addId pid inh (Threaded is srt idt) = Threaded is srt (LocalId pid:idt)

addTopId :: (Pos,Id) -> a -> Threaded -> Threaded
addTopId pid inh (Threaded is srt idt) = Threaded is srt (TopId pid:idt)

{- is it a class method? -}
isCMethod :: Info -> Bool
isCMethod (InfoIMethod _ _ _ _ _) = True
isCMethod (InfoDMethod _ _ _ _ _) = True
isCMethod _ = False

{- End Module DbgTrans ------------------------------------------------------}


{- 
The following code is an experiment to create useful redex trails
for pattern bindings. Unfortunately viewing an unevaluted pattern
variable in the browser leads to a segmentation fault of the program.
It seems that Sats sometimes don't work correctly, when the expression
isn't yet evaluated. 

dDecl (DeclPat (Alt pat rhs decls)) =
    ...
--    lookupId Var t_lazySat >>>= \lazySat ->
--    lookupId Var t_patvar >>>= \pvid ->
--    lookupCon noPos tNTId >>>= \ntid ->
--    lookupCon noPos t_Nm >>>= \nm ->
    ...

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


{- more comments about other stuff:
dMethod info@(InfoDMethod _ _ _ _ _) pos id funName fundefs = 
  dFun pos id funName (getArity fundefs) fundefs NoType
  -- only disadvantage of this simple transformation:
  -- if a method is called for which no (default) definition exists,
  -- then the trace shows a redex `error "no default ..."' which does
  -- not exist in the program.
-}
{- old definition; problem:
   if a method is called for which no (default) definition exists,
   then the misleading error message: No match in pattern. is given.
   The trace is not correctly produced, because the call to fatal is
   not wrapped with R; hence use of such a method without evaluation
   even leads to segmentation fault.
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
-}
