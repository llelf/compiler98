module AuxFile
  ( toAuxFile		-- primary export used by Main
  , module AuxFile	-- internals used by module AuxLabelAST
  ) where

import Maybe
import List
import Char
import Monad
import IO
#if defined(__GLASGOW_HASKELL__)
import IOExts (trace)
#else
import NonStdTrace (trace)
#endif

import Syntax
import TokenId (TokenId,tPrelude)
import AssocTree
import OsOnly
import Import
import Flags
import Extra (noPos)


-- `toAuxFile' writes out the .hx file given this module's complete
-- parse tree.  The .hx file mentions all exported identifiers, both
-- those defined in this module, and those reexported from imports.

toAuxFile :: Flags -> FilePath -> Module TokenId -> IO ()
toAuxFile flags aux
          (Module pos modid exports imports fixdecls (DeclsParse decls)) =
  do
    let (toIdent,definedTypesAndClasses) = mkIdentMap decls
    let definedExported = extendEnv
				(visibleIn exports unspecYes modid)
				toIdent initAT
				(map DeclFixity fixdecls ++ decls)
    fullInfo <- getImports (visibleIn exports unspecNo)
				definedExported flags imports
    writeFile aux ((showString "module " . shows modid . showChar '\n' .
                    showLines (listAT fullInfo)
                   ) "")
    let missingDefns = missing exports fullInfo definedTypesAndClasses toIdent
    if not (null missingDefns) then
        hPutStr stderr
            ((showString "\nExported but not defined in this module "
		. showString "(possibly imported and reexported):\n"
		. showLines missingDefns) "\n")
      else return ()
  where
    showLines :: Show a => [a] -> ShowS
    showLines = foldr (\x y-> shows x . showChar '\n' . y) id



-- AuxiliaryInfo is the extra information we need to know about identifiers.
data AuxiliaryInfo = Has
	{ args     :: Int
	, fixity   :: Fixity
	, priority :: Int
	, letBound :: Bool
	}
	deriving (Show,Read)
data Fixity = L | R | Pre String | Def | None deriving (Show,Read)
emptyAux = Has { args=(-1), fixity=Def, priority=9, letBound=True }

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con String{-type-} String{-con-}
		| Method String{-class-} String{-method-}
	deriving (Show,Read,Eq,Ord)

-- AuxTree is an environment, associating each identifier with a
-- unique AuxiliaryInfo.
type AuxTree = AssocTree Identifier AuxiliaryInfo

-- IdentMap is an environment associating each constructor with
-- its type, each method with its class.  We can encounter a
-- constructor (or method) without its type (or class) in a fixity decl,
-- but we then need to know its type (or class) to know whether it
-- is exported or not.  If an entity is neither a known constructor
-- nor a known method, we assume it is just an ordinary variable.
type IdentMap = TokenId{-con, var, or method-} -> Identifier


-- `mkIdentMap' makes a little lookup table from data constructors to their
-- type name, and methods to their class .  Additionally, it builds a list
-- of all defined types, plus synonyms and class names, used to check that
-- all exports have a referent.

mkIdentMap :: [Decl TokenId] -> (IdentMap,[TokenId])
mkIdentMap decls =
    let dataDecls  = concatMap dataDecl decls
        classDecls = concatMap classDecl decls
    in ( lookup (foldr addMethod (foldr addCon initAT dataDecls) classDecls)
       , map fst dataDecls ++ map fst classDecls ++ concatMap typeSyn decls)
  where
    dataDecl (DeclData _ _ (Simple _ typ _) tycons _) = [(typ,tycons)]
    dataDecl _ = []

    classDecl (DeclClass _ _ id _ (DeclsParse decls)) = [(id,decls)]
    classDecl _ = []

    typeSyn (DeclType (Simple pos id vars) _) = [id]
    typeSyn _ = []

    addCon (typ, tycons) t = foldr doCon t tycons
	where
            doCon (Constr _ c _) t        = addAT t const c typ
            doCon (ConstrCtx _ _ _ c _) t = addAT t const c typ

    addMethod (cls, decls) t = foldr doMethod t decls
	where
	    doMethod (DeclVarsType pis ctxs typ) t = foldr pId t pis
	    doMethod _ t = t
	    pId (pos,meth) t = addAT t const meth cls

    lookup t v =
      let id = show v in
      case lookupAT t v of
	Just tc -> if isCon id then Con (show tc) id else Method (show tc) id
	Nothing -> Var id

isCon :: String -> Bool
isCon (x:_) = isUpper x || x==':'


-- `extendEnv' extends an AuxTree environment from a list of declarations,
-- filtered for `visibility' by some criterion.  For instance, when writing
-- the .hx file, we take care only to include identifiers that are visible
-- via the export declarations.  But when traversing the syntax tree to
-- resolve fixity conflicts and label it with arity information, the local
-- environment of identifiers is built with all identifiers in scope visible.

extendEnv :: Visibility -> IdentMap -> AuxTree -> [Decl TokenId] -> AuxTree
extendEnv visible toIdent init decls =
    foldr (auxInfo visible toIdent) init decls


-- `getImports' sucks in the .hx files for all explicit imports,
-- following the impspec carefully with regard to explicit naming and
-- hiding.  We take a kind of need-analysis as the `reexport' argument,
-- to determine more accurately which entities are really vital to
-- import and which to ignore.  For the moment, we always grab an
-- implicit Prelude, even if there is an explicit one hiding some entities,
-- but this is probably safe because we always ensure that we don't import
-- an identifier if it is already defined locally.

getImports :: (TokenId->Visibility) -> AuxTree
		 -> Flags -> [ImpDecl TokenId] -> IO AuxTree
getImports reexport alreadyGot flags =
    foldM getAuxFile alreadyGot . map impData
	. (Import (noPos,tPrelude) (Hiding []) :)   -- omit until bootstrapped!
  where
    getAuxFile got (modid,importVisible) = do
        (_,f) <- readFirst (fixImportNames (sUnix flags) "hx" (show modid)
                                           (sIncludes flags ++ sPreludes flags))
	(return
          . foldr (\(k,v) t-> if notGot k t
				 && importVisible k
				 && reexport modid k
                              then addAT t const k v else t) got
          . map read . tail . lines) f

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

    (Var v)     `match` (EntityVar _ y)       =  v  == show y
    (Con t1 c)  `match` (EntityTyCon _ t2 cs) =  t1 == show t2 &&
						 c `elem` (map (show.snd) cs)
    (Con t1 c)  `match` (EntityTyConCls _ t2) =  t1 == show t2
    (Method c m)`match` (EntityVar _ y)       =  m  == show y
    (Method c m)`match` (EntityTyConCls _ cs) =  c  == show cs
    (Method c m)`match` (EntityTyCls _ cs ms) =  c  == show cs &&
					 	 m `elem` (map (show.snd) ms)
    _           `match`  _                    =  False


-- Visibility is a function denoting whether an identifier should be
-- added (or not) to the AuxTree structure.
type Visibility = Identifier -> Bool


-- `visibleIn' is a particular kind of visibility, determined
-- by the explicit exports of the module.

-- When checking Constructors, we need to check both whether it
-- is mentioned explicitly, and whether the type it belongs to it
-- mentioned in Typ(..) syntax, which implicitly exports all its
-- constructors.  Likewise for methods and the Class(..) syntax.

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

    explicitTypes = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityTyCon _ _ cons) -> map (show.snd) cons
		_ -> []) exports

    explicitMethods = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityTyCls _ _ mets) -> map (show.snd) mets
		_ -> []) exports

    implicitTypesOrMethods = concatMap
	 (\e-> case e of
		ExportEntity _ (EntityTyConCls _ torc) -> [show torc]
		_ -> []) exports

    idFilter c@(Con typ con)    = con `elem` explicitTypes
				|| typ `elem` implicitTypesOrMethods
    idFilter v@(Var var)        = var `elem` explicitVars
    idFilter v@(Method cls met) = met `elem` explicitMethods
				|| met `elem` explicitVars
				|| cls `elem` implicitTypesOrMethods


-- The following comment is no longer true - I have added a Maybe
-- wrapper around the parsed export list to distinguish the two cases.
-- However I believe there may a different, more subtle, use of this
-- distinction, so the code remains for now until I can check more
-- closely.

-- There is one horrible hack: nhc98 does not distinguish `module
-- M where' from `module M () where', and treats them identically
-- in the abstract syntax!  We are forced to assume that an empty
-- export list really means to export everything, since this is the
-- likeliest of the two cases.  But of course, we should only
-- export everything defined in this module, not everything it imports.

unspecYes, unspecNo :: Visibility
unspecYes = \_->True
unspecNo  = \_->False


-- Add varid/varop identifier, with arity.
auxInfo :: Visibility -> IdentMap -> Decl TokenId -> AuxTree -> AuxTree
auxInfo visible toIdent (DeclFun _ f clauses) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
    where a   = let (Fun pats rhs local) = head clauses in length pats
          key = Var (show f)
-- Add varop identifier declared in infix equation, with arity.
auxInfo visible toIdent (DeclPat (Alt (ExpInfixList _ es) rhs local)) t
    | len >= 3  =
	let (_:defn:_) = es in
	case defn of
	  ExpVarOp _ f
	    | visible key -> addAT t replaceArity key (emptyAux {args=len-1})
						where key = Var (show f)
	  _ -> t
    where len = length es
-- Add varid identifier declared as a primitive, with arity.
auxInfo visible toIdent (DeclPrimitive _ f a _) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
						where key = Var (show f)
-- Add varid identifier declared as a foreign import, with arity.
auxInfo visible toIdent (DeclForeignImp _ _ f a _ _ _) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
						where key = Var (show f)
-- Add conid/conop identifier, with arity.
auxInfo visible toIdent (DeclData _ _ typ tycons _) t =
    foldr doCon t tycons
  where
    doCon (Constr _ c ps) t	 = accept c ps t
    doCon (ConstrCtx _ _ _ c ps) t = accept c ps t
    accept con ps t
	| visible key = addAT t replaceArity key (emptyAux {args=a})
	| otherwise = t
	where a = sum (map (\(mb,_)->maybe 1 length mb) ps)
	      (Simple _ typname _) = typ
	      key = (Con (show typname) (show con))
auxInfo visible toIdent (DeclClass _ _ cls _ (DeclsParse decls)) t =
    foldr doMethod t decls
  where
    doMethod (DeclVarsType pis ctxs typ) env = foldr pId env pis
    doMethod (DeclFixity f) env = fixInfo visible toIdent f env
    doMethod _ env = env
    pId (pos,meth) t | visible key = addAT t replaceArity key emptyAux
		     | otherwise = t
		     where key = Method (show cls) (show meth)
-- Add normal infix decl for identifier.
auxInfo visible toIdent (DeclFixity f) t = fixInfo visible toIdent f t
-- No other form of decl matters.
auxInfo visible toIdent _ t = t
--auxInfo visible toIdent (DeclType _ _) t =
--auxInfo visible toIdent (DeclTypeRenamed _ _) t =
--auxInfo visible toIdent (DeclDataPrim _ _ _) t =
--auxInfo visible toIdent (DeclConstrs _ _ _) t =
--auxInfo visible toIdent (DeclInstance _ _ _ _ _) t =
--auxInfo visible toIdent (DeclDefault _) t =
--auxInfo visible toIdent (DeclForeignExp _ _ _ _) t =
--auxInfo visible toIdent (DeclVarsType _ _ _) t =
--auxInfo visible toIdent (DeclPat _) t =
--auxInfo visible toIdent (DeclIgnore _) t =
--auxInfo visible toIdent (DeclError _) t =
--auxInfo visible toIdent (DeclAnnot _ _) t =


-- Add fixity info for identifier.  Here, we can encounter a constructor
-- without its parent type, or a method without its parent class, and
-- thus need to reconstruct its parent by looking in the IdentMap.

fixInfo :: Visibility -> IdentMap -> FixDecl TokenId -> AuxTree -> AuxTree
fixInfo visible toIdent (fixclass,prio,ids) t =
    foldr (\name t -> let key = toIdent name in
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
replaceArity aux1 aux2 = aux2 { args = args aux1 }
replaceInfix aux1 aux2 = aux2 { priority = priority aux1, fixity = fixity aux1 }


-- `missing' determines a list of exported identifiers that were
-- apparently neither defined in this module nor imported/reexported.
-- In fact, it falsely accuses reexported types and classes at the
-- moment.
missing :: Maybe [Export TokenId] -> AuxTree -> [TokenId] -> IdentMap
	 -> [Identifier]
missing Nothing defined definedTypes toIdent = []
missing (Just exports) defined definedTypes toIdent =
    concatMap notDefined exports
  where
    notDefined (ExportEntity _ (EntityVar _ v))      = methodOrVar v
    notDefined (ExportEntity _ (EntityTyCon _ t cs)) =
			 concatMap (isntDef . Con (show t) . show . snd) cs
    notDefined (ExportEntity _ (EntityTyCls _ c ms)) =
			 concatMap (isntDef . Method (show c) . show . snd) ms
    notDefined (ExportEntity _ (EntityTyConCls _ t)) = typNotDefined t
    notDefined _ = []

    isntDef v = case lookupAT defined v of
		  Just _ -> []
		  Nothing -> [v]

    methodOrVar m =
		let v = toIdent m in
		case lookupAT defined (toIdent m) of
		  Just _ -> []
		  Nothing -> [v]

    typNotDefined typ = if typ `elem` definedTypes then []
			else [Con (show typ) ".."]

