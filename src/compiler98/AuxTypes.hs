module AuxTypes where

import Char (isDigit)
import List (isPrefixOf)
import Syntax
import AssocTree
import TokenId (TokenId,tPrelude,visImport,t_Tuple)

-- AuxiliaryInfo is the extra information we need to know about identifiers.
data AuxiliaryInfo = Has
	{ args     :: Int
	, fixity   :: Fixity
	, priority :: Int
	, letBound :: Bool
	}
	deriving (Show,Read)
data Fixity = L | R | Pre String | Def | None deriving (Eq,Show,Read)
emptyAux = Has { args=(-1), fixity=Def, priority=9, letBound=True }
patternAux = Has { args=(-1), fixity=Def, priority=9, letBound=False }

-- Identifier is used to distinguish varids from conids, and relate
-- conids back to the type they belong to.  It also relates methods
-- to their class.
data Identifier = Var String | Con TypeSort String{-type-} String{-con-}
		| Field String{-type-} String{-field-}
		| Method String{-class-} String{-method-}
	deriving (Show,Read,Eq,Ord)

data TypeSort = Data | Newtype deriving (Show,Read,Eq,Ord)

subTid :: Identifier -> TokenId
subTid (Var v)      = visImport v
subTid (Con t _ c)    = possTuple c
subTid (Field t f)  = visImport f
subTid (Method c m) = visImport m

possTuple "()" = t_Tuple 0
possTuple s | "Prelude." `isPrefixOf` s =
    let nm = drop 8 s in
    if isDigit (head nm) then t_Tuple (read nm)
    else visImport s
possTuple s = visImport s

-- The main Environment is composed of two pieces...
type Environment = (AuxTree,IdentMap)

-- AuxTree is an environment, associating each identifier with a unique
-- AuxiliaryInfo.
type AuxTree = AssocTree Identifier AuxiliaryInfo

-- IdentMap is an environment associating each constructor/field with
-- its type, and each method with its class.  We can encounter a
-- constructor (or method) without its type (or class) in a fixity decl,
-- but we then need to know its type (or class) to know whether it
-- is exported or not.  If an entity is neither a known constructor/field
-- nor a known method, we assume it is just an ordinary variable.
type IdentMap = AssocTree TokenId{-con, var, or method-} Identifier

-- `mkIdentMap' makes a little lookup table from data constructors and field
-- names to their type name, and methods to their class.  Additionally, it
-- builds a list of all defined types, plus synonyms and class names, used
-- to check that all exports have a referent.
mkIdentMap :: [Decl TokenId] -> (IdentMap,[TokenId])
mkIdentMap decls =
    let dataDecls  = concatMap dataDecl decls
        classDecls = concatMap classDecl decls
    in ( foldr addMethod (foldr addCon initAT dataDecls) classDecls
       , map (\(x,_,_)->x) dataDecls ++ map fst classDecls 
         ++ concatMap typeSyn decls)
  where
    dataDecl (DeclData Nothing _ (Simple _ typ _) tycons _)  = 
      [(typ,Newtype,tycons)]
    dataDecl (DeclData (Just _) _ (Simple _ typ _) tycons _)  = 
      [(typ,Data,tycons)]
    dataDecl _ = []

    classDecl (DeclClass _ _ cls _ (DeclsParse decls)) = [(cls,decls)]
    classDecl _ = []

    typeSyn (DeclType (Simple pos id vars) _) = [id]
    typeSyn _ = []

    addCon (typ,typeSort,tycons) t = foldr doCon t tycons
	where
        doCon (Constr _ c fs) t        = conAndFields c fs t
        doCon (ConstrCtx _ _ _ c fs) t = conAndFields c fs t
        conAndFields c fs t = addFields (addAT t const c
                                          (Con typeSort styp (show c)))
                                          styp fs
        styp = show typ

    addFields t typ [] = t
    addFields t typ ((Nothing,_):_) = t
    addFields t typ ((Just posids,_):cs) = foldr doField (rest t) posids
        where
            doField (_,f) t = addAT t const f (Field typ (show f))
            rest t = addFields t typ cs

    addMethod (cls, decls) t = foldr doMethod t decls
	where
	    doMethod (DeclVarsType pis ctxs typ) t = foldr pId t pis
	    doMethod _ t = t
	    pId (pos,meth) t = addAT t const meth
                                     (Method (show cls) (show meth))

useIdentMap :: IdentMap -> TokenId -> Identifier
useIdentMap m v =
      case lookupAT m v of
	Just tc -> tc
	Nothing -> Var (show v)

