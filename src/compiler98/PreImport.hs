{- ---------------------------------------------------------------------------
-- imported by Main and Need
-}
module PreImport (HideDeclIds,qualRename,preImport) where

import List(partition,nub,intersect,(\\))
import MergeSort
import TokenId(TokenId(..),tPrelude,t_Arrow,ensureM,forceM,dropM,
               rpsPrelude,rpsBinary,t_List)
import PackedString(PackedString,packString,unpackPS)
import Syntax
import IdKind
import AssocTree
import Memo
import Tree234(treeMapList)
import Extra
import ParseI
import Flags
import OsOnly
import Scc
import IExtract
import Info
import NeedLib(NeedTable)
import ImportState(ImportState)
import PreImp(HideDeclIds,HideDeclType,HideDeclData,HideDeclDataPrim
             ,HideDeclClass,HideDeclInstance,HideDeclInstance
             ,HideDeclVarsType)


-- internal, fully coalesced import declaration
type IntImpDecl = (TokenId {-module name-}, ImportedNamesInScope)


-- There are two sets of names in scope, the NQ-set and the Q-set.
-- For every imported module, an individual import decl can enlarge
-- either both sets together, or just the Q-set.  When the two sets are
-- identical by construction, our representation takes a short-cut and
-- stores only one set, calling it NQ.  When the NQ-set is empty, we store
-- only the Q-set.  When the two sets are different and non-empty, we
-- store both, but the Q-set is always equal to or larger than the NQ-set.
data ImportedNamesInScope =
      NQ NameSetSpec	-- represents  Q-set  = NQ-set.
    | Q  NameSetSpec	-- represents  NQ-set = empty.
    | Both NameSetSpec{-notQ-} NameSetSpec{-Q-}
			-- invariant:  Q-set >= NQ-set.

-- The representation of a name-set is a mixture of intension and extension.
--    Deny []      means everything found in the exporting module
--    Deny xs      means everything excluding the named entities
--    Allow xs     means only the named entities
--    Allow []     means no entities, probably a very rare specification
-- Hence,   Deny []  >  Deny xs  >  Allow xs  >  Allow []
data NameSetSpec =
      Allow [(TokenId,IE)]
    | Deny [(TokenId,IE)]


-- Assuming that import decls have been converted to nameset specifications,
-- the 'combine' function joins two given specifications.  Imports are
-- cumulative, so a nameset can only get larger.

combine (Deny [])  _          = Deny []
combine _          (Deny [])  = Deny []
combine (Allow xs) (Deny ys)  = Deny (ys\\xs)
combine (Allow xs) (Allow ys) = Allow (nub (xs++ys))
combine (Deny xs)  (Deny ys)  = Deny (intersect xs ys)
combine (Deny xs)  (Allow ys) = Deny (xs\\ys)


-- The rules for combining different imports of the same module are complex.
-- The second argument to the 'joinNames' function is the accumulated
-- state of all imports so far, and can thus have the Both constructor,
-- representing differing NQ- and Q-sets.  The first argument is the
-- additional import decl, and can enlarge either both sets (NQ) or just the
-- Q-set, but cannot enlarge the two sets separately (no Both constructor).

joinNames (NQ new)        (NQ old)    = NQ (combine old new)
joinNames (Q new)         (Q old)     = Q  (combine old new)
joinNames (Q q)           (NQ nq)     = joinNames (NQ nq) (Q q)

joinNames (NQ (Deny []))  (Q old)     = NQ   (Deny [])
joinNames (NQ new)        (Q old)     = Both new (combine old new)

joinNames (NQ (Deny []))  (Both nq q) = NQ   (Deny [])
joinNames (NQ new)        (Both nq q) = Both (combine nq new) (combine q new)
joinNames (Q new)         (Both nq q) = Both nq (combine q new)


-- Finally, we need a lookup function that can tell us whether a name
-- is in the permissible set of names specified by the import decls.

nameInScope :: ImportedNamesInScope -> TokenId -> Bool
nameInScope (NQ nameset) tid = tid `inScope` nameset
nameInScope (Q nameset)  tid = tid `inScope` nameset
nameInScope (Both nq q)  tid = (tid `inScope` nq) || (tid `inScope` q)

inScope :: TokenId -> NameSetSpec -> Bool
inScope tid (Deny [])  = True
inScope tid (Deny xs)  = not (tid `elem` (map fst xs))
inScope tid (Allow xs) =     (tid `elem` (map fst xs))

-- 'mustQualify' assumes a separate test for inclusion in the permissible names
mustQualify :: ImportedNamesInScope -> TokenId -> Bool
mustQualify (NQ nameset) tid = False
mustQualify (Q nameset)  tid = tid `inScope` nameset
mustQualify (Both nq q)  tid = (tid `inScope` q) && not (tid `inScope` nq)

----

qualRename :: [ImpDecl TokenId] -> TokenId -> [TokenId]

qualRename impdecls = qualRename' qTree 
 where
  qualRename' t q@(Qualified t1 t2) =
    case lookupAT t t1 of
	Nothing -> [q]
	Just ts -> map (\t'-> Qualified t' t2) ts
  qualRename' t v = [v]

  qTree = foldr qualR initAT impdecls

  qualR (Import    _ _)  t = t
  qualR (ImportQ   _ _)  t = t
  qualR (ImportQas (_,Visible id) (_,Visible id') _) t = addAT t (++) id' [id]
  qualR (Importas  (_,Visible id) (_,Visible id') _) t = addAT t (++) id' [id]


---- ===================================
-- shorten rpsl = if (isPrelude . reverse . unpackPS) rpsl then rpsPrelude else rpsl

preImport :: Flags -> TokenId -> Tree (TokenId,IdKind) 
          -> Maybe [Export TokenId] -> [ImpDecl TokenId] 
          -> Either String
               ((TokenId->Bool) -> TokenId -> IdKind -> IE
               ,[(PackedString
                 ,(PackedString,PackedString,Tree (TokenId,IdKind)) 
                    -> [[TokenId]] 
                    -> Bool
                 ,HideDeclIds
                 )
                ]
               )

-- When the export list is :: Maybe [Export TokenId])
--    Nothing -> export nothing except instances
--    Just [] -> export everything
--    Just xs -> export only entities from the list xs

preImport flags mtid@(Visible mrps) need (Just expdecls) impdecls =
  case transImport impdecls of
    Left err -> Left err
    Right impdecls ->
      Right ( if null expdecls || (isJust . lookupAT exportAT) (mtid,Modid)
                then reExportAll
                else reExportTid mrps exportAT
            , map (mkNeed need exportAT) impdecls)
  where
  exportAT = mkExportAT expdecls
preImport flags mtid@(Visible mrps) need Nothing impdecls =
  case transImport impdecls of
    Left err -> Left err
    Right impdecls ->
           Right (reExportTid mrps initAT, map (mkNeed need initAT) impdecls)


{-
-- transImport orders the import files (with prelude last),
-- inserts qualified import of prelude,
-- and checks that all imports are consistent
-}
transImport :: [ImpDecl TokenId] 
            -> Either String{-errors-} [IntImpDecl]

transImport impdecls = Right (map finalTouch impdecls2)

  where

  impdecls2 =  (sortImport . traverse initAT False)
              -- (ImportQ (noPos,tPrelude) (Hiding []) :
                  (ImportQ (noPos,vis "Ratio") (NoHiding
				[EntityTyConCls noPos (vis "Rational")
				,EntityTyConCls noPos (vis "Ratio")
				,EntityVar noPos (vis "%")]) :
                     impdecls)
  vis = Visible . packString . reverse

  -- Place imports into order, ensure Prelude is last
  sortImport impdecls =
          ( map snd
          . mergeSortCmp (error "Fail in PreImport.transImport\n") cmpFst 
          . map (\(k,v)-> if k==tPrelude then (Right k,(k,v))
                          else (Left k,(k,v)) )
          ) impdecls

  traverse :: AssocTree TokenId ([Pos],[TokenId],ImportedNamesInScope)
           -> Bool	-- have we found an explicit Prelude import yet?
	   -> [ImpDecl TokenId]
	   -> [(TokenId, ([Pos],[TokenId],ImportedNamesInScope))]

  traverse acc True  []      = treeMapList (:) acc
  traverse acc False []      = traverse acc False [Import (noPos,tPrelude)
							  (Hiding [])]
  traverse acc prel (x:xs)  =
    case extractImp x of
      (tid,info) ->
        traverse (addAT acc comb tid info) (prel || tid==tPrelude) xs

  -- combine two imports: arg is (srcpos, renamings, impspec)
  comb (p1,a1,spec1) (p2,a2,spec2) = (p1++p2, a1++a2, joinNames spec1 spec2)

  extractImp (ImportQ  (pos,tid) impspec) = 
    (tid, ([pos], [],     Q (extractSpec impspec)))
  extractImp (ImportQas (pos,tid) (apos,atid) impspec) =
    (tid, ([pos], [atid], Q (extractSpec impspec)))
  extractImp (Import (pos,tid) impspec) = 
    (tid, ([pos], [],     NQ (extractSpec impspec)))
  extractImp (Importas (pos,tid) (apos,atid) impspec) =
    (tid, ([pos], [atid], NQ (extractSpec impspec)))

  extractSpec (NoHiding entities) = Allow (concatMap extractImpEntity entities)
  extractSpec (Hiding entities)   = Deny  (concatMap extractImpEntity entities)

  extractImpEntity e = 
    map (\e-> case e of ((tid,kind),ie) -> (tid,ie)) (extractEntity e)

{- Now obsolete  i.e. never report explicit/hiding conflicts
--checkImport :: (TokenId, ([Pos],[TokenId],ImportedNamesInScope))
--            -> [String]
--checkImport (tid,(nq,q,pos_spec)) =
--  case partition (isLeft . snd) pos_spec of
--    ([],hide)  -> []  -- Only explicit hide
--    (imp,[])   -> []  -- Only explicit imports
--    (imp,hide) ->
--	if (null . filter (not.null) . map (dropRight . snd)) hide
--	then []         -- Ok as all hidings are empty
--	else ["Conflicting imports for " ++ show tid ++
--            ", used both explicit imports (at" ++ 
--            (mixCommaAnd . map (strPos . fst)) imp 
--	      ++ ") and explicit hidings (at " ++  
--            (mixCommaAnd . map (strPos . fst)) hide ++")."]
-}


  finalTouch :: (TokenId, ([Pos],[TokenId],ImportedNamesInScope))
	     -> IntImpDecl
  finalTouch (tid,(pos,alts,spec)) =  (tid,spec)


{- Obsolete in H'98
--checkForMultipleImport imports = 
--    case foldr prepare (initAT,[]) imports of
--      (qm,qas) ->
--	case (filter (elemM qm) qas,filter ((1/=) . length) (group qas)) of
--	  (qas,qas2) ->
--	    map (\tid -> "Can not rename a module to " ++ show tid ++
--              " as another module with that name is imported qualified.") qas
--          ++
--	    map (\tids -> "More than one module is renamed to " ++
--                        show (head tids) ++ ".") qas2
-- where
--  prepare (tid,(nq,Just tids,pos_spec)) (qm,qas) = (addM qm tid,tids++qas)
--  prepare _ (qm,qas) = (qm,qas)
-}


------------------------------------------------------------------------------

mkExportAT :: [Export TokenId] -> AssocTree (TokenId,IdKind) IE
mkExportAT expdecls = exportAT
 where
  exportAT :: AssocTree (TokenId,IdKind) IE
  exportAT = foldr export initAT (concatMap preX expdecls)

  export (key,value) t = addAT t combIE key value

  preX (ExportEntity _ e) = extractEntity e
  preX (ExportModid _ tid) = [((tid,Modid),IEall)]


extractEntity (EntityVar  pos tid)       = [((tid,Var),IEall)]
extractEntity (EntityTyConCls pos tid)
    | (tid==t_Arrow || tid==t_List)      = [((dropM tid,TC),IEall)]
    | otherwise                          = [((tid,TC),IEall)]
extractEntity (EntityTyCon  pos tid [])
    | (tid==t_Arrow || tid==t_List)      = [((dropM tid,TCon),IEabs)]
    | otherwise                          = [((tid,TCon),IEabs)]
extractEntity (EntityTyCon  pos tid ids)
    | (tid==t_Arrow || tid==t_List)      = [((dropM tid,TCon),IEall)]
    | otherwise                          = [((tid,TCon),IEall)]
		  -- Don't care about checking that all constructors are correct
extractEntity (EntityTyCls  pos tid ids) = [((tid,TClass),IEall)]
		  -- Don't care about checking that all methods are correct


------

reExportAll :: (TokenId->Bool) -> TokenId -> IdKind -> IE
reExportAll q tid kind = IEall

reExportTid :: PackedString -> AssocTree (TokenId,IdKind) IE 
            -> (TokenId->Bool) -> TokenId -> IdKind -> IE
reExportTid modname exportAT mustBeQualified tid kind =
  case lookupAT exportAT (dropM tid, kind) of
    Just imp | not (mustBeQualified tid) -> imp
    _  ->
      case lookupAT exportAT (forceM modname tid, kind) of
        Just imp | mustBeQualified tid -> imp
        _                              -> IEnone

--------------------------------------
  

{-
The selectors for (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,
hideDeclInstance,hideDeclVarsType) are defined in PreImp and used in ParseI
-}

mkNeed :: Tree (TokenId,IdKind)  
       -> Tree ((TokenId,IdKind),IE) 
       -> IntImpDecl 
       -> ( PackedString
          , (PackedString, PackedString, Tree (TokenId,IdKind)) 
               -> [[TokenId]] -> Bool
          , HideDeclIds
          )

mkNeed needM exportSpec (vt@(Visible modname), importSpec) =
   ( modname
   , \needI -> any (needFun needI)
   , (hideDeclType,hideDeclData,hideDeclDataPrim
     ,hideDeclClass,hideDeclInstance,hideDeclVarsType)
   )

 where

  imported = nameInScope importSpec . dropM
  q        = mustQualify importSpec . dropM

  reExport
    | reExportModule = reExportAll
    | otherwise      = reExportTid modname exportSpec

  reExportModule = isJust (lookupAT exportSpec (vt,Modid))

--needFun' x y =
--    let result = needFun x y in
--    strace ("needFun: "++show (fst3 x)++"/"++show (snd3 x)++" "
--            ++show y++" "++show result) $ result
  needFun (orps,rps,needI) ns@(n:_) =
        isJust (lookupAT needI (ensureM rps n))
				-- is used by other interface (real name)
				-- (only check first name = type or class)
     || any (\n-> (isJust . lookupAT needM . forceM orps) n
                  && imported n)
            ns			-- used qualified and imported (un)qualified
     || any (\n-> (isJust . lookupAT needM . dropM) n
                  && imported n && not (q n))
            ns			-- used unqualified and imported unqualified
     || any (\n-> reExportModule && imported n && not (q n))
            ns			-- reexported whether used or not


  hideDeclType :: HideDeclType
  hideDeclType st attr (Simple pos tid tvs) typ = 
    if imported tid then
      iextractType (reExport q tid TSyn) attr q pos tid tvs typ () st
    else
      iextractType IEnone attr (\_->True) pos tid tvs typ () st
		-- used by an interface file, not directly in source code

  hideDeclData :: HideDeclData
  hideDeclData st attr ctxs (Simple pos tid tvs) constrs der =
    if imported tid then
--    case lookupAT impT (dropM tid) of
--    Just IEall -> iextractData  (reExport q tid TCon) q attr ctxs pos
--                                tid tvs constrs () st
--    Just IEabs -> iextractData  (reExport q tid TCon) q attr ctxs pos
--                                tid tvs (if q tid then constrs else []) () st
      iextractData (reExport q tid TCon) q attr ctxs pos
                   tid tvs constrs () st
   else
      iextractData IEnone (\_->True) attr ctxs pos
                   tid tvs (if q tid then constrs else []) () st

  hideDeclDataPrim :: HideDeclDataPrim
  hideDeclDataPrim st (pos,tid) size =
    if imported tid then
      iextractDataPrim (reExport q tid TCon) q pos tid size () st
    else
      iextractDataPrim IEnone (\_->True) pos tid size () st

  hideDeclClass :: HideDeclClass
  hideDeclClass st  ctxs (pos,tid) tvar methods =
    if imported tid then
--    case lookupAT impT (dropM tid) of
--    Just IEall ->  iextractClass (reExport q tid TClass) q pos ctxs tid
--                                 (snd tvar) methods () st
--    Just IEabs ->  iextractClass (reExport q tid TClass) q pos ctxs tid
--                                 (snd tvar) (if q then methods else []) () st
      iextractClass (reExport q tid TClass) q pos ctxs tid
                    (snd tvar) methods () st
    else
      iextractClass IEnone (\_->True) pos ctxs tid
                    (snd tvar) (if q tid then methods else []) () st

  hideDeclInstance :: HideDeclInstance
  hideDeclInstance st ctxs (pos,cls) typ =
    iextractInstance ctxs pos cls typ () st
		-- instances are always imported, they cannot be hidden.

  hideDeclVarsType :: HideDeclVarsType
  hideDeclVarsType st postidanots ctxs typ =    
		-- interface files should never depend on functions
{- we don't create interface files with more than one function/type
    case filter (isJust . lookupAT impT . dropM . snd . fst) postidanots of
      [] -> st
      postidanots ->
-}
	 iextractVarsType  reExport q postidanots ctxs typ () st

