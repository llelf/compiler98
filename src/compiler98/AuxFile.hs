module AuxFile
  ( module AuxFile	-- internals used by module AuxLabelAST
  ) where
 -- toAuxFile		-- primary export used by Main
			-- hbc won't let me put both specs in the export list


import Monad(when)
import IO(hPutStr,stderr)
import Maybe(isNothing)
import List(isPrefixOf)

import Syntax
import TokenId (TokenId,tPrelude,visImport,t_Tuple,getUnqualified)
import AssocTree
import OsOnly
import Import
import Flags
import Extra (noPos)
import AuxTypes
import AuxFixity (fixInfixList)

import Extra (strace)
import AssocTree

-- `toAuxFile' writes out the .hx file given this module's complete
-- parse tree.  The .hx file mentions all exported identifiers, both
-- those defined in this module, and those reexported from imports.

toAuxFile :: Flags -> FilePath -> Module TokenId -> IO ()
toAuxFile flags aux
          (Module pos modid exports imports fixdecls (DeclsParse decls)) =
  do
    let (identMap,definedTypesAndClasses) = mkIdentMap decls
    let (_,definedExported) = extendEnv (visibleIn exports unspecYes modid)
				    (initAT,identMap)
				    (map DeclFixity fixdecls ++ decls)
    (fullInfo,fullIdentMap) <- getImports (visibleIn exports unspecNo)
					  definedExported
                                          flags imports
    writeFile aux ((showString "module " . shows modid . showChar '\n' .
                    showLines (listAT fullInfo)
                   ) "")
    let missingDefns = missing exports (fullInfo,fullIdentMap)
			       definedTypesAndClasses
    when (not (null missingDefns))
         (hPutStr stderr
            ((showString "\nExported but not defined in this module "
		. showString "(possibly imported and reexported):\n"
		. showLines missingDefns) "\n"))
  where
    showLines :: Show a => [a] -> ShowS
    showLines = foldr (\x y-> shows x . showChar '\n' . y) id


-- `getImports' sucks in the .hx files for all explicit imports,
-- following the impspec carefully with regard to explicit naming and
-- hiding.  We take a kind of need-analysis as the `reexport' argument,
-- to determine more accurately which entities are really vital to
-- import and which to ignore.  

getImports :: (TokenId->Visibility) -> Environment
		 -> Flags -> [ImpDecl TokenId] -> IO Environment
getImports reexport (alreadyGot,identMap) flags impdecls = do
    let importFiles = filter normalImport (map impData impdecls)
    auxInfos <- mapM getAuxFile importFiles
    let allInfo = zip importFiles auxInfos
    return ( foldr extendImportEnv alreadyGot allInfo
           , foldr extendIdentMap identMap allInfo )
  where
    normalImport (modid,_) = 
      not (sPrelude flags && "TraceOrig" `isPrefixOf` getUnqualified modid)
    getAuxFile :: (TokenId,Visibility) -> IO [(Identifier,AuxiliaryInfo)]
    getAuxFile (modid,importVisible) = do
        (_,f) <- readFirst (fixImportNames (sUnix flags) "hx" (show modid)
                                           (sIncludes flags ++ sPreludes flags))
	(return . map read . tail . lines) f

    extendImportEnv ((modid,importVisible), auxInfos) got =
        foldr (\(k,v) t-> if notGot k t && importVisible k && reexport modid k
                          then addAT t const k v else t) got auxInfos

    extendIdentMap ((modid,importVisible), auxInfos) got =
        foldr (\(k,_) t-> let i  = subTid k in
                          if notGot i t && importVisible k && reexport modid k
                          then addAT t const i k else t) got auxInfos

    notGot k t = case lookupAT t k of { Nothing -> True; Just _ -> False }

    impData (Import (_,modid) is)      = (modid, impSpec is)
    impData (ImportQ (_,modid) is)     = (modid, impSpec is)
    impData (ImportQas (_,modid) _ is) = (modid, impSpec is)
    impData (Importas (_,modid) _ is)  = (modid, impSpec is)

    impSpec :: ImpSpec TokenId -> Visibility
    impSpec (Hiding [])         = (\x-> True)
    impSpec (Hiding entities)   = (\x-> not (x `isAmong` entities))
    impSpec (NoHiding entities) = (\x-> x `isAmong` entities)

    x `isAmong` entities = (x `match`) `any` entities

    (Var v)     `match` (EntityVar _ y)        =  v  == show y
    (Field t1 f)`match` (EntityVar _ y)        =  f  == show y
    (Method c m)`match` (EntityVar _ y)        =  m  == show y
    (Con _ t1 c)`match` (EntityConClsAll _ t2) =  t1 == show t2
    (Field t1 f)`match` (EntityConClsAll _ t2) =  t1 == show t2
    (Method c m)`match` (EntityConClsAll _ c2) =  c  == show c2
    (Con _ t1 c)`match` (EntityConClsSome _ t2 cs) =
				t1 == show t2 && c `elem` (map (show.snd) cs)
    (Field t1 f)`match` (EntityConClsSome _ t2 cs) =
				t1 == show t2 && f `elem` (map (show.snd) cs)
    (Method c m)`match` (EntityConClsSome _ c2 ms) =
				c  == show c2 && m `elem` (map (show.snd) ms)
    _           `match`  _                     =  False



-- Visibility is a function denoting whether an identifier should be
-- added (or not) to the AuxTree structure.
type Visibility = Identifier -> Bool

-- `visibleIn' is a particular kind of visibility, determined
-- by the explicit exports of the module.

-- When checking Constructors, we need to check both whether it
-- is mentioned explicitly, and whether the type it belongs to it
-- mentioned in Typ(..) syntax, which implicitly exports all its
-- constructors.  Likewise for fields, and for methods with the
-- Class(..) syntax.

visibleIn :: Maybe [Export TokenId] -> Visibility -> TokenId -> Visibility
visibleIn Nothing noneSpecified modid = (\_->False)
visibleIn (Just exports) noneSpecified modid
  | null exports = noneSpecified
  | any (implicitAll modid) exports = (\_->True)
  | otherwise = idFilter
  where
    implicitAll modid (ExportModid _ m) | m==modid = True
    implicitAll _ _ = False

    explicitVars = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityVar _ e) ->[show e]
		_ -> []) exports

    explicitSubordinates = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityConClsSome _ _ sub) -> map (show.snd) sub
		_ -> []) exports

    implicitSubordinates = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityConClsAll _ torc) -> [show torc]
		_ -> []) exports

    idFilter c@(Con _ typ con)    =  con `elem` explicitSubordinates
				|| typ `elem` implicitSubordinates
    idFilter v@(Var var)        =  var `elem` explicitVars
    idFilter v@(Method cls met) =  met `elem` explicitSubordinates
				|| cls `elem` implicitSubordinates
				|| met `elem` explicitVars
    idFilter c@(Field typ f)    =  f   `elem` explicitSubordinates
				|| typ `elem` implicitSubordinates
				|| f   `elem` explicitVars


-- If it is left unspecified whether an entity is exported (due to
-- a declaration like `module M where'), the entity's visibility
-- in the true exports depends on whether the entity was defined here or
-- merely imported.  Defined entities are exported, imported ones are not.
unspecYes, unspecNo :: Visibility
unspecYes = \_->True
unspecNo  = \_->False


-- `extendEnv' extends an AuxTree environment from a list of declarations,
-- filtered for `visibility' by some criterion.  For instance, when writing
-- the .hx file, we take care only to include identifiers that are visible
-- via the export declarations.  But when traversing the syntax tree to
-- resolve fixity conflicts and label it with arity information, the local
-- environment of identifiers is built with all identifiers in scope visible.

extendEnv :: Visibility -> Environment -> [Decl TokenId] 
          -> ([TokenId],Environment)
extendEnv visible (init,identMap) decls = (irrefutableIds,(env,identMap))
  where
  (irrefutableIds,env) = foldrPat (auxInfo visible identMap) init decls


-- auxInfo is the main gatherer of information about declarations.
-- It decides what AuxiliaryInfo to attach to each Identifier in
-- the environment.
auxInfo :: Visibility -> IdentMap -> Decl TokenId -> AuxTree 
        -> ([TokenId],AuxTree)
-- Add varid/varop identifier, with arity.
auxInfo visible identMap (DeclFun _ f clauses) t
    | visible key  = ([],addAT t replaceArity key (emptyAux {args = a}))
    where a   = let (Fun pats rhs local) = head clauses in length pats
          key = Var (show f)
-- Add varop identifier declared in infix equation, with arity.
auxInfo visible identMap (DeclPat (Alt pat@(ExpInfixList _ es) rhs local)) t
    | len >= 3  =
	let (_:defn:_) = es in
	case defn of
	  ExpVarOp _ f -- actually a function definition
	    | visible key -> 
              ([],addAT t replaceArity key (emptyAux {args=len-1}))
						where key = Var (show f)
	  _ -> (\(i,(e,m)) -> ([],e)) $ 
                addPat Irrefutable visible pat (t,identMap)
    where len = length es
-- Add varid identifiers declared in a pattern binding.
auxInfo visible identMap (DeclPat (Alt pat rhs local)) t =
    (\(i,(e,m)) -> ([],e)) $ addPat Irrefutable visible pat (t,identMap)
-- Add varid identifier declared as a primitive, with arity.
auxInfo visible identMap (DeclPrimitive _ f a _) t
    | visible key  = ([],addAT t replaceArity key (emptyAux {args = a}))
						where key = Var (show f)
-- Add varid identifier declared as a foreign import, with arity.
auxInfo visible identMap (DeclForeignImp _ _ _ f a _ _ _) t
    | visible key  = ([],addAT t replaceArity key (emptyAux {args = a}))
						where key = Var (show f)
-- Add conid/conop identifier, with arity, and any associated field names.
auxInfo visible identMap (DeclData sort _ (Simple _ typ _) tycons _) t =
  ([],foldr doCon t tycons)
  where
    doCon (Constr _ c fs) t	   = accept c fs (foldr doFields t fs)
    doCon (ConstrCtx _ _ _ c fs) t = accept c fs (foldr doFields t fs)
    accept con fs t
	| visible key = addAT t replaceArity key (emptyAux {args=a})
	| otherwise = t
	where a = sum (map (\(mb,_)->maybe 1 length mb) fs)
	      key = Con (if isNothing sort then Newtype else Data) 
                      (show typ) (show con)
    doFields (Nothing,_) t = t
    doFields (Just fs,_) t = foldr doField t fs
    doField (_,f) t
        | visible key = addAT t replaceArity key (emptyAux {args=1})
	| otherwise = t
	where key = Field (show typ) (show f)
-- Add class method identifier, arity is always -1.
auxInfo visible identMap (DeclClass _ _ cls _ (DeclsParse decls)) t =
  ([],foldr doMethod t decls)
  where
    doMethod (DeclVarsType pis ctxs typ) env = foldr pId env pis
    doMethod (DeclFixity f) env = fixInfo visible identMap f env
    doMethod _ env = env
    pId (pos,meth) t | visible key = addAT t replaceArity key emptyAux
		     | otherwise = t
		     where key = Method (show cls) (show meth)
-- Add normal infix decl for identifier.
auxInfo visible identMap (DeclFixity f) t = ([],fixInfo visible identMap f t)
-- No other form of decl matters.
auxInfo visible identMap _ t = ([],t)
--auxInfo visible identMap (DeclType _ _) t =
--auxInfo visible identMap (DeclTypeRenamed _ _) t =
--auxInfo visible identMap (DeclDataPrim _ _ _) t =
--auxInfo visible identMap (DeclConstrs _ _ _) t =
--auxInfo visible identMap (DeclInstance _ _ _ _ _) t =
--auxInfo visible identMap (DeclDefault _) t =
--auxInfo visible identMap (DeclForeignExp _ _ _ _) t =
--auxInfo visible identMap (DeclVarsType _ _ _) t =
--auxInfo visible identMap (DeclIgnore _) t =
--auxInfo visible identMap (DeclError _) t =
--auxInfo visible identMap (DeclAnnot _ _) t =


-- Add fixity info for identifier.  Here, we can encounter a constructor
-- without its parent type, or a method without its parent class, and
-- thus need to reconstruct its parent by looking in the IdentMap.

fixInfo :: Visibility -> IdentMap -> FixDecl TokenId -> AuxTree -> AuxTree
fixInfo visible identMap (fixclass,prio,ids) t =
    foldr (\name t -> let key = useIdentMap identMap name in
                      if visible key then addAT t replaceInfix key
					   (emptyAux {priority=prio,fixity=f})
                      else t)
          t
          (map stripFixId ids)
  where
    f = case fixclass of
          InfixDef   -> Def
          InfixL     -> L
          InfixR     -> R
          Infix      -> None
          InfixPre f -> Pre (show f)


-- different ways of combining new item into AuxTree where item already exists
replaceArity, replaceInfix :: AuxiliaryInfo -> AuxiliaryInfo -> AuxiliaryInfo
replaceArity new old = new { priority = priority old, fixity = fixity old} 
  -- old { args = args new }
replaceInfix new old = old { priority = priority new, fixity = fixity new }


-- `missing' determines a list of exported identifiers that were
-- apparently neither defined in this module nor imported/reexported.
-- In fact, it falsely accuses reexported types and classes at the
-- moment.
missing :: Maybe [Export TokenId] -> Environment -> [TokenId] -> [Identifier]
missing Nothing (defined,identMap) definedTypes = []
missing (Just exports) (defined,identMap) definedTypes =
    concatMap notDefined exports
  where
    notDefined (ExportEntity _ (EntityVar _ v))       = valNotDefined v
    notDefined (ExportEntity _ (EntityConClsAll _ t)) = typNotDefined t
    notDefined (ExportEntity _ (EntityConClsSome _ t cs)) =
				concatMap (valNotDefined . snd) cs
    notDefined _ = []

    valNotDefined m = let v = useIdentMap identMap m in
                      case lookupAT defined v of
                          Just _ -> []
                          Nothing -> [v]

    typNotDefined typ = if typ `elem` definedTypes then []
			else [Con Data (show typ) ".."]


-- Extending environment from handling of patterns

data PatSort = Refutable | Irrefutable

foldrPat :: (a -> env -> ([b],env)) -> env -> [a] -> ([b],env)
foldrPat f env pats = (foldr combinePat ((,) []) . map f $ pats) env

combinePat :: (env -> ([b],env)) -> (env -> ([b],env)) -> (env -> ([b],env)) 
combinePat f g env = (fbs++gbs,fenv)
  where
  (gbs,genv) = g env
  (fbs,fenv) = f genv
  

-- `addPat' extends the environment with a lambda-bound variable
-- (e.g. pattern).  Visibility is only important in the exported aux file.
-- A variable in a refutable pattern becomes lambda-bound in the environment,
-- a variable in an irrefutable pattern becomes let-bound;
-- the latter variables are returned as first part of the result.
addPat :: PatSort -> Visibility -> Pat TokenId -> Environment 
       -> ([TokenId],Environment)
addPat patSort visible pat (env,identMap) = (refutableVars,(newEnv,identMap))
  where
  (refutableVars,newEnv) = ap patSort pat env
  ap :: PatSort -> Pat TokenId -> AuxTree -> ([TokenId],AuxTree)
  ap s (ExpRecord (ExpCon p id) fields) env = foldrPat (addField s) env fields
  ap Refutable pat@(ExpApplication p ((ExpCon _ id):exps)) env 
    | isIrrefutable pat = foldrPat (ap Irrefutable) env exps
  ap s (ExpApplication _ exps) env = foldrPat (ap s) env exps
  ap s (ExpVar p id) env = extendEnvPat s id env
  ap s (ExpCon p id) env = ([],env)
  ap s (ExpList p exps) env = foldrPat (ap s) env exps
  ap s (PatAs p id pat) env = (ap s pat `combinePat` extendEnvPat s id) env
  ap s (PatIrrefutable p pat) env  = ap Irrefutable pat env
  ap s (PatNplusK p id1 id2 exp1 exp2 exp3) env = 
    ([],addAT env letBound key patternAux{letBound=True,args=0})
    where
    key = Var (show id1)
    -- variable shall be let bound
  ap s (ExpInfixList p exps) env = ap s (fixInfixList (env,identMap) exps) env
  -- ap s (ExpVarOp p id) env = extendEnvPat s id env
  -- ap s (ExpConOp p id) env = env
  ap _   _ env = ([],env)

  addField s (FieldExp p id exp) env = ap s exp env
  addField s (FieldPun p id) env     = extendEnvPat s id env

  extendEnvPat :: PatSort -> TokenId -> AuxTree -> ([TokenId],AuxTree)
  extendEnvPat Refutable id env 
    | visible key = ([],addAT env lambdaBound key patternAux)
    where
    key = Var (show id)
  extendEnvPat Irrefutable id env 
    | visible key = 
      ([id],addAT env letBound key patternAux{letBound=True,args=0})
    where
    key = Var (show id)
  extendEnvPat _ _ env = ([],env)

  lambdaBound new old = old { letBound=False, args= -1 }
  letBound new old = old { letBound=True, args=0 }

  isIrrefutable :: Pat TokenId -> Bool
  isIrrefutable (ExpApplication pos pats) = and . map isIrrefutable $ pats
  isIrrefutable (ExpCon _ id) = isNewTypeDataCon id
  isIrrefutable (ExpVar _ _) = True
  isIrrefutable (PatAs _ _ pat) = isIrrefutable pat
  isIrrefutable (PatIrrefutable _ _) = True
  isIrrefutable (ExpRecord (ExpCon _ id) fieldPats) =
    and . (isNewTypeDataCon id :) . map isIrrefutableField $ fieldPats
    where
    isIrrefutableField (FieldExp _ _ pat) = isIrrefutable pat
  isIrrefutablePat _ = False

  isNewTypeDataCon id = case lookupAT identMap id of
    Just (Con Newtype _ _) -> True
    Just (Con Data _ _) -> False
