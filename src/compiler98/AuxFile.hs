module AuxFile
  ( module AuxFile	-- internals used by module AuxLabelAST
  ) where
 -- toAuxFile		-- primary export used by Main
			-- hbc won't let me put both specs in the export list

import Maybe
import List
import Char
import Monad
import IO

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
    when (not (null missingDefns))
         (hPutStr stderr
            ((showString "\nExported but not defined in this module "
		. showString "(possibly imported and reexported):\n"
		. showLines missingDefns) "\n"))
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
patternAux = Has { args=(-1), fixity=Def, priority=9, letBound=False }

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con String{-type-} String{-con-}
		| Field String{-type-} String{-field-}
		| Method String{-class-} String{-method-}
	deriving (Show,Read,Eq,Ord)

-- AuxTree is an environment, associating each identifier with a
-- unique AuxiliaryInfo.
type AuxTree = AssocTree Identifier AuxiliaryInfo

-- IdentMap is an environment associating each constructor/field with
-- its type, and each method with its class.  We can encounter a
-- constructor (or method) without its type (or class) in a fixity decl,
-- but we then need to know its type (or class) to know whether it
-- is exported or not.  If an entity is neither a known constructor/field
-- nor a known method, we assume it is just an ordinary variable.
type IdentMap = TokenId{-con, var, or method-} -> Identifier


-- `mkIdentMap' makes a little lookup table from data constructors and field
-- names to their type name, and methods to their class.  Additionally, it
-- builds a list of all defined types, plus synonyms and class names, used
-- to check that all exports have a referent.

mkIdentMap :: [Decl TokenId] -> (IdentMap,[TokenId])
mkIdentMap decls =
    let dataDecls  = concatMap dataDecl decls
        classDecls = concatMap classDecl decls
    in ( lookup (foldr addMethod (foldr addCon initAT dataDecls) classDecls)
       , map fst dataDecls ++ map fst classDecls ++ concatMap typeSyn decls)
  where
    dataDecl (DeclData _ _ (Simple _ typ _) tycons _)  = [(typ,tycons)]
    dataDecl _ = []

    classDecl (DeclClass _ _ cls _ (DeclsParse decls)) = [(cls,decls)]
    classDecl _ = []

    typeSyn (DeclType (Simple pos id vars) _) = [id]
    typeSyn _ = []

    addCon (typ, tycons) t = foldr doCon t tycons
	where
            doCon (Constr _ c fs) t        = conAndFields c fs t
            doCon (ConstrCtx _ _ _ c fs) t = conAndFields c fs t
            conAndFields c fs t = addFields (addAT t const c (Con styp)) styp fs
            styp = show typ

    addFields t typ [] = t
    addFields t typ ((Nothing,_):_) = t
    addFields t typ ((Just posids,_):cs) = foldr doField (rest t) posids
        where
            doField (_,f) t = addAT t const f (Field typ)
            rest t = addFields t typ cs

    addMethod (cls, decls) t = foldr doMethod t decls
	where
	    doMethod (DeclVarsType pis ctxs typ) t = foldr pId t pis
	    doMethod _ t = t
	    pId (pos,meth) t = addAT t const meth (Method (show cls))

    lookup t v =
      let id = show v in
      case lookupAT t v of
	Just tc -> tc id
	Nothing -> Var id


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
-- import and which to ignore.  

getImports :: (TokenId->Visibility) -> AuxTree
		 -> Flags -> [ImpDecl TokenId] -> IO AuxTree
getImports reexport alreadyGot flags =
    foldM getAuxFile alreadyGot . map impData
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

    (Var v)     `match` (EntityVar _ y)        =  v  == show y
    (Field t1 f)`match` (EntityVar _ y)        =  f  == show y
    (Method c m)`match` (EntityVar _ y)        =  m  == show y
    (Con t1 c)  `match` (EntityConClsAll _ t2) =  t1 == show t2
    (Field t1 f)`match` (EntityConClsAll _ t2) =  t1 == show t2
    (Method c m)`match` (EntityConClsAll _ c2) =  c  == show c2
    (Con t1 c)  `match` (EntityConClsSome _ t2 cs) =
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

    idFilter c@(Con typ con)    =  con `elem` explicitSubordinates
				|| typ `elem` implicitSubordinates
    idFilter v@(Var var)        =  var `elem` explicitVars
    idFilter v@(Method cls met) =  met `elem` explicitSubordinates
				|| cls `elem` implicitSubordinates
				|| met `elem` explicitVars
    idFilter c@(Field typ f)    =  f   `elem` explicitSubordinates
				|| typ `elem` implicitSubordinates


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


auxInfo :: Visibility -> IdentMap -> Decl TokenId -> AuxTree -> AuxTree
-- Add varid/varop identifier, with arity.
auxInfo visible toIdent (DeclFun _ f clauses) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
    where a   = let (Fun pats rhs local) = head clauses in length pats
          key = Var (show f)
-- Add varop identifier declared in infix equation, with arity.
auxInfo visible toIdent (DeclPat (Alt pat@(ExpInfixList _ es) rhs local)) t
    | len >= 3  =
	let (_:defn:_) = es in
	case defn of
	  ExpVarOp _ f
	    | visible key -> addAT t replaceArity key (emptyAux {args=len-1})
						where key = Var (show f)
	  _ -> addPat visible pat t
    where len = length es
-- Add varid identifiers declared in a pattern binding.
auxInfo visible toIdent (DeclPat (Alt pat rhs local)) t =
    addPat visible pat t
-- Add varid identifier declared as a primitive, with arity.
auxInfo visible toIdent (DeclPrimitive _ f a _) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
						where key = Var (show f)
-- Add varid identifier declared as a foreign import, with arity.
auxInfo visible toIdent (DeclForeignImp _ _ _ f a _ _ _) t
    | visible key  = addAT t replaceArity key (emptyAux {args = a})
						where key = Var (show f)
-- Add conid/conop identifier, with arity, and any associated field names.
auxInfo visible toIdent (DeclData _ _ (Simple _ typ _) tycons _) t =
    foldr doCon t tycons
  where
    doCon (Constr _ c fs) t	   = accept c fs (foldr doFields t fs)
    doCon (ConstrCtx _ _ _ c fs) t = accept c fs (foldr doFields t fs)
    accept con fs t
	| visible key = addAT t replaceArity key (emptyAux {args=a})
	| otherwise = t
	where a = sum (map (\(mb,_)->maybe 1 length mb) fs)
	      key = Con (show typ) (show con)
    doFields (Nothing,_) t = t
    doFields (Just fs,_) t = foldr doField t fs
    doField (_,f) t
        | visible key = addAT t replaceArity key (emptyAux {args=1})
	| otherwise = t
	where key = Field (show typ) (show f)
-- Add class method identifier, arity is always -1.
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
    notDefined (ExportEntity _ (EntityVar _ v))       = valNotDefined v
    notDefined (ExportEntity _ (EntityConClsAll _ t)) = typNotDefined t
    notDefined (ExportEntity _ (EntityConClsSome _ t cs)) =
				concatMap (valNotDefined . snd) cs
    notDefined _ = []

    valNotDefined m = let v = toIdent m in
                      case lookupAT defined v of
                          Just _ -> []
                          Nothing -> [v]

    typNotDefined typ = if typ `elem` definedTypes then []
			else [Con (show typ) ".."]


-- `addPat' extends the environment with a lambda-bound variable
-- (e.g. pattern).  Visibility is only important in the exported aux file.
--
addPat :: Visibility -> Pat TokenId -> AuxTree -> AuxTree
addPat v (ExpRecord (ExpCon p id) fields) env = foldr (addField v) env fields
addPat v (ExpRecord (ExpVar p id) fields) env = foldr (addField v)
                                                  (extendEnvPat v id env) fields
addPat v (ExpApplication p exps) env = foldr (addPat v) env exps
addPat v (ExpVar p id) env           = extendEnvPat v id env
addPat v (ExpCon p id) env           = env
addPat v (ExpInfixList p exps) env   = foldr (addPat v) env exps
addPat v (ExpVarOp p id) env         = extendEnvPat v id env
addPat v (ExpConOp p id) env         = env
addPat v (ExpList p exps) env        = foldr (addPat v) env exps
addPat v (PatAs p id pat) env        = addPat v pat (extendEnvPat v id env)
addPat v (PatIrrefutable p pat) env  = addPat v pat env
addPat v (PatNplusK p id1 id2 exp1 exp2 exp3) env = extendEnvPat v id1 env
addPat _   _ env = env

addField v (FieldExp p id exp) env = addPat v exp env
addField v (FieldPun p id) env     = extendEnvPat v id env

extendEnvPat visible id env
  | visible key = addAT env lambdaBound key patternAux
  | otherwise   = env
  where
    key = Var (show id)
    lambdaBound aux1 aux2 = aux2 { letBound=False }

