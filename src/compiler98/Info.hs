{- ---------------------------------------------------------------------------
Central data structures of the symbol table
-}
module Info(module Info, IdKind,TokenId,NewType,InfixClass(..),Pos(..)
           ,AssocTree(..),Tree{-,PackedString-}) where

import IdKind(IdKind)
import TokenId(TokenId)
import NT
import Extra(Pos(..),sndOf,strace)
import PackedString(PackedString)
import Tree234
import AssocTree
import Syntax(InfixClass(..))
import Id(Id)

-- Guess: this is "Interface Exports"?
data IE = IEnone | IEsel | IEabs | IEall deriving (Eq,Show) 
--    defined in a lattice   IEall
--                          /     \
--                       IEsel   IEabs
--                          \     /
--                           IEnone
isExported IEnone = False
isExported IEsel  = False
isExported _      = True


combIE IEall  _ = IEall
combIE IEnone i = i
combIE _ IEall  = IEall
combIE i IEnone = i
combIE _      i = i

data DataKind = 
    DataTypeSynonym Bool  -- True <-> unboxed after expansion
                    Int   -- depth (used to determine 
                          -- which type synonym to expand)
  | DataNewType	    Bool  -- always False
                    [Id]  -- constructor(one or zero) 
  | Data 	    Bool  -- True <-> unboxed
                    [Id]  -- constructors
  | DataPrimitive   Int	  -- size
  deriving (Show)

data Info =
    InfoClear   -- used to remove imported when redefining in mutally 
                -- recursive modules and when compiling the prelude
  | InfoUsed      Id       -- unique
                  [(IdKind,TokenId,PackedString,Pos)] -- occurrence where used
  | InfoUsedClass Id       -- unique
                  [(IdKind,TokenId,PackedString,Pos)] -- occurrence where used
                  (AssocTree Int ([Int],[(Int,Int)]))  
                  -- instances of the class
                  -- the tree associates a type constructor with
                  -- the free variables and the superclass context 
                  -- of an instance
  | InfoData      -- data type (algebraic, type synonym, ...)
                  Id       -- unique
                  TokenId  -- token of data type name
                  IE 
                  NewType  -- if type synonym: type it is defined to be
                           -- if data or newtype: defined type
                           -- e.g.: data Num a => Test a b = A a | B b
                           -- NewType [1,2] [] [(NumId, 1)] 
                           --   [NTvar 1, NTvar 2, NTcons TestId 
                           --                        [NTvar 1, NTvar 2]]
                  DataKind -- kind of data type 
  | InfoClass     Int      -- unique
                  TokenId  -- token of class name
                  IE 
                  NewType  -- pseudo type built from class and type variable
                           -- (type of dictionary?)
                  [Id]     -- method ids refering to type declaration
                  [Id]     -- method ids refering to default definition
                           -- ids in same position refer to same method
                           -- => lists have same lengths
                  (AssocTree Int ([Int],[(Int,Int)]))    
                  -- instances of the class
                  -- the tree associates a type constructor with
                  -- the free variables and the superclass context 
                  -- of an instance
  | InfoVar       -- term variable 
                  Int          -- unique 
                  TokenId      -- token for name
                  (InfixClass TokenId,Int)  -- fixity 
                  IE 
                  NewType      -- type
                  (Maybe Int)  -- arity (if available)
  | InfoConstr    -- data constructor
                  Int          -- unique 
                  TokenId      -- token for name
                  (InfixClass TokenId,Int)  -- fixity 
                  NewType      -- type of the constructor
                  [Maybe Int]  -- field names (if they exist) 
                  Int          -- data type to which constructor belongs
  | InfoField     -- field name
                  Id           -- unique
                  TokenId      -- token for name
                  [(Id,Int)]   -- icon_offs
                  Id           -- iData
                  Id           -- iSel	
                  -- unique tid [(constructor,offset)] type selector
  | InfoMethod    -- for type declaration of method in a class definition
                  Id           -- unique 
                  TokenId      -- token for method name
                  (InfixClass TokenId,Int) -- fixity
                  NewType 
                  (Maybe Int)  -- arity (if available; here bogus)
                  Id           -- unique of class to which method belongs
  | InfoIMethod   -- for definition in instance definition
                  Id           -- unique 
                  TokenId      -- token for name
                  NewType 
                  (Maybe Int)  -- arity (if available) 
                  Id           -- iMethod (0 after renaming)
                  -- The type is NewType free instancs_ctx instance_type, 
                  -- for real type follow iMethod
  | InfoDMethod   -- for default definition in class definition
                  Id           -- unique
                  TokenId      -- token for method name
                  NewType 
                  (Maybe Int)  -- arity (if available) 
                  Id           -- class to which method belongs
  | InfoInstance  -- Only used in Export
                  Id           -- unique
                  NewType 
                  Id 	       -- unique of class (of which this is instance)
  | InfoName      Id           -- unique
                  TokenId      -- token for name
                  Int          -- arity
                  TokenId      
    -- inserted late to hold name and arity for some functions 
    -- (second TokenId is profname )
  deriving (Show)

{- Template
z (InfoUsed   unique uses) =
z (InfoUsedClass unique uses insts) =
z (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) ->
	(DataNewType unboxed constructors) ->
	(Data unboxed  constrs) ->
	(DataPrimitive size) ->
z (InfoClass  unique tid exp nt ms ds insts) = 
z (InfoVar    unique tid fix exp nt annot) = 
z (InfoConstr unique tid fix nt fields iType) =
z (InfoField  unique tid icon_offs iData iSel) =
z (InfoMethod unique tid fix nt annot iClass) =
z (InfoIMethod unique tid nt annot iMethod) =
z (InfoDMethod unique tid nt annot iClass) =
z (InfoInstance unique  nt iClass) =
z (InfoName pos unique tid Int ptid) =
-}

clearI :: a -> Info
clearI _ = InfoClear


--isClear InfoClear = True
--isClear _ = False

isMethod :: Info -> Bool

isMethod (InfoMethod unique tid fix nt annot iClass) = True
isMethod _ = False


isData :: Info -> Bool 

isData (InfoData   unique tid exp nt dk) = True
isData _ = False


isRealData :: Info -> Bool

isRealData (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> False
	(DataNewType unboxed constructors) -> False
	(DataPrimitive size) -> True
	(Data unboxed  constrs) -> True
isRealData info = error ("isRealData " ++ show info)


isRenamingFor :: AssocTree Int Info -> Info -> NewType

isRenamingFor st (InfoData  unique tid exp nt (DataTypeSynonym _ depth))   = nt
isRenamingFor st info@(InfoData  unique tid exp nt (DataNewType _ constrs)) =
    case constrs of
      []  -> error ("Cannot find constructor for newtype: "++show info)
      [c] -> case lookupAT st c of
               Just i  -> ntI i
               Nothing -> error ("Cannot find info for newtype constructor: "++show info)
isRenamingFor st info = error ("isRenamingFor " ++ show info)


isDataUnBoxed :: Info -> Bool  
 
isDataUnBoxed (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> unboxed
	(DataNewType unboxed constructors) -> unboxed
	(Data unboxed  constrs) -> unboxed
	(DataPrimitive size) -> True
isDataUnBoxed info = error ("isDataUnBoxed: " ++ show info)


isField :: Info -> Bool

isField (InfoField _ _ _ _ _) = True
isField _ = False

isClass :: Info -> Bool

isClass (InfoClass _ _ _ _ _ _ _) = True
isClass _ = False


depthI :: Info -> Maybe Int

depthI (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> Just depth
	_ -> Nothing
depthI _ = Nothing


updTypeSynonym :: Bool -> Int -> Info -> Info

updTypeSynonym unboxed depth (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym _ _) ->
	  (InfoData   unique tid exp nt (DataTypeSynonym unboxed depth)) 


{-
Sets the unboxedness information in newtype info as given.
-}
updNewType :: Bool -> Info -> Info

updNewType unboxed (InfoData unique tid exp nt dk) =
      case dk of
	(DataNewType _ constructors) -> 
          InfoData unique tid exp nt (DataNewType unboxed constructors)

{-
Sets the type information in variable info as given.
Is only applied to identifiers without types,i.e. never methods of any kind!
-}
newNT :: NewType -> Info -> Info

newNT nt (InfoVar unique tid fix exp _ annot) =  InfoVar unique tid fix exp nt annot


ntI :: Info -> NewType

ntI (InfoData   unique tid exp nt dk) = nt
-- ntI (InfoClass  unique tid exp nt ms ds) =  nt   --- Not needed?
ntI (InfoVar     unique tid fix exp nt annot) = nt
ntI (InfoConstr  unique tid fix nt fields iType) = nt
ntI (InfoMethod  unique tid fix nt annot iClass) = nt
ntI (InfoIMethod  unique tid nt annot iMethod) = nt  -- Work here?
ntI (InfoDMethod  unique tid nt annot iClass) = nt


strictI :: Info -> [Bool]

strictI (InfoConstr  unique tid fix (NewType free [] ctx nts) fields iType) = 
  map strictNT (init nts)
strictI _ = []  
  -- Not strict in any argument so it doesn't matter if we return empty list

qDefI (InfoUsed _ _) = False
qDefI (InfoUsedClass _ _ _) = False
qDefI _ = True

uniqueI (InfoUsed   unique _)     = unique
uniqueI (InfoUsedClass unique _ _)     = unique
uniqueI (InfoData   unique tid exp nt dk) = unique
uniqueI (InfoClass  unique _ _ _ _ _ _) = unique
uniqueI (InfoVar     unique _ _ _ _ _) = unique
uniqueI (InfoConstr  unique _ _ _ _ _) = unique
uniqueI (InfoField   unique tid icon_offs iData iSel) = unique
uniqueI (InfoMethod  unique _ _ _ _ _) = unique
uniqueI (InfoIMethod  unique _ _ _ _) = unique
uniqueI (InfoDMethod  unique _ _ _ _) = unique
uniqueI (InfoInstance unique _ _) = unique
uniqueI (InfoName  unique _ _ _) = unique


tidI :: Info -> TokenId

tidI (InfoData   unique tid exp nt dk) = tid
tidI (InfoClass  u tid _ _ _ _ _) = tid
tidI (InfoVar     u tid _ _ _ _) = tid
tidI (InfoConstr  u tid _ _ _ _) = tid
tidI (InfoField   u tid icon_offs iData iSel) = tid
tidI (InfoMethod  u tid _ _ _ _) = tid
tidI (InfoIMethod  u tid _ _ _) = tid
tidI (InfoDMethod  u tid _ _ _) = tid
tidI (InfoName  u tid _ _) = tid
tidI info = error ("tidI (Info.hs) called with bad info:\n" ++ show info)


cmpTid :: TokenId -> Info -> Bool

cmpTid t (InfoUsed _ _) = False
cmpTid t (InfoUsedClass _ _ _) = False
cmpTid t i =  tidI i == t


methodsI :: Info -> [(Int,Int)]

methodsI (InfoClass u tid e nt ms ds inst) = zip ms ds


instancesI :: Info -> Tree (Int,([Int],[(Int,Int)]))

instancesI (InfoClass u tid e nt ms ds inst) = inst
instancesI info@(InfoUsedClass u uses inst) = 
  strace ("***instanceI(1) " ++ show info ++ "\n") inst
instancesI info = 
  strace ("***instanceI(2) " ++ show info ++ "\n") initAT 
  -- This is a lie!!! For some reason has this class no real entry


{- Return identifiers of all superclasses of the class which is described
by given info -}
superclassesI :: Info -> [Int]

superclassesI (InfoClass u tid e (NewType free [] ctxs nts) ms ds inst) = 
  map fst ctxs
superclassesI info = error ("superclassesI " ++ show info)

{- Add information about an instance to info of a class.
If information about this instance exists already in info, then info left
unchanged.
type constructor -> free type variables -> context -> class info -> class info
-}
addInstanceI :: Int -> [Int] -> [(Int,Int)] -> Info -> Info

addInstanceI con free ctxs info@(InfoClass u tid e nt ms ds inst) =
  case lookupAT inst con of
    Just _ -> info
    Nothing -> InfoClass u tid e nt ms ds (addAT inst sndOf con (free,ctxs))
addInstanceI con free ctxs info@(InfoUsedClass u uses inst) =
  case lookupAT inst con of
    Just _ -> info
    Nothing -> InfoUsedClass u uses (addAT inst sndOf con (free,ctxs))
addInstanceI con free ctxs (InfoUsed u uses) = 
	addInstanceI con free ctxs (InfoUsedClass u uses initAT)

{-
In joining two trees for describing instances the second one gets
precedence in case of conflict.
-}
joinInsts :: AssocTree Int a -> AssocTree Int a -> AssocTree Int a

joinInsts inst inst' =
  foldr ( \ (k,v) inst -> addAT inst sndOf k v) inst (treeMapList (:) inst')


{- Determine constructors of a type from the info of the type -}
constrsI :: Info -> [Int]

constrsI (InfoName  unique tid i ptid) = [unique]   
  -- ^this is a lie! but it is consistent with belongstoI :-)
constrsI (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) ->  
           strace ("Constr of type synonym " ++ show tid) []
	(DataNewType unboxed constructors) -> constructors
	(DataPrimitive size) ->  
           strace ("Constr of data primitive " ++ show tid) []
	(Data unboxed  constrs) -> constrs
constrsI info = error ("constrsI " ++ show info)


updConstrsI :: Info -> [Int] -> Info

updConstrsI (InfoData   unique tid exp nt dk) constrs' =
  case dk of
    (Data unboxed  constrs) -> 
      InfoData   unique tid exp nt (Data unboxed  constrs')

fieldsI (InfoConstr unique tid fix nt fields iType) = fields


combInfo :: Info -> Info -> Info

combInfo  InfoClear      info'                     = info'
combInfo (InfoUsed _ w) (InfoUsed u' w')           = InfoUsed u' (w++w')
combInfo (InfoUsed _ _)  info'                     = info'
combInfo  info           InfoClear                 = info
combInfo  info          (InfoUsed _ _)             = info
combInfo i1@(InfoUsedClass _ uses insts) 
         i2@(InfoClass u tid exp nt ms ds insts') =
  InfoClass u tid exp nt ms ds (joinInsts insts' insts)
combInfo i1@(InfoClass _ tid exp nt ms ds insts) 
         i2@(InfoUsedClass u uses insts') =
  InfoClass u tid exp nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid exp nt ms ds insts) 
         (InfoClass u' tid' exp' nt' [] [] insts') =	
  InfoClass u tid (combIE exp exp') nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid exp nt ms ds insts) 
         (InfoClass u' tid' exp' nt' ms' ds' insts') =	
  InfoClass u tid (combIE exp exp') nt' ms' ds' (joinInsts insts' insts)
combInfo info@(InfoData u tid exp nt dk) 
         info'@(InfoData u' tid' exp' nt' dk')  =
  case dk' of
    Data unboxed [] -> info
    _ -> if isExported exp' then info' else info
combInfo info                        info'                            =  
  -- Use new (if possible) so that code can override old imported
	if isExported (expI info)
	then info
        else info'

expI (InfoData   unique tid exp nt dk) = exp
expI (InfoClass  unique tid exp nt ms ds insts) = exp
expI (InfoVar     unique tid fix exp nt annot) = exp
expI (InfoConstr  unique tid fix nt fields iType) = IEnone  -- Data contains export info
expI (InfoField   unique tid icon_offs iData iSel) = IEnone -- Data contains export info
expI (InfoMethod  unique tid fix nt annot iClass) = IEnone
expI (InfoIMethod  unique tid nt annot iMethod) = IEnone
expI (InfoDMethod  unique tid nt annot iClass) = IEnone
expI info = IEnone -- I get InfoUsed here !!!

-- arity without context (Visible)
arityVI (InfoVar     unique tid fix exp nt (Just arity)) =  arity
arityVI (InfoConstr  unique tid fix (NewType _ _ _ nts) fields iType) = length nts - 1
arityVI (InfoMethod  unique tid fix nt (Just arity) iClass) = 1
arityVI (InfoIMethod  unique tid nt (Just arity) iMethod) = arity
arityVI (InfoDMethod  unique tid nt (Just arity) iClass) = arity 
arityVI (InfoName  unique tid arity ptid) = arity

-- arity with context
arityI (InfoVar     unique tid fix exp (NewType _ _ ctxs _) (Just arity)) =  
  length ctxs + arity
arityI (InfoVar     unique tid fix exp NoType (Just arity)) =  
  arity   -- NR Generated after type deriving
arityI (InfoConstr  unique tid fix (NewType _ _ _ nts) fields iType) = 
  length nts - 1
arityI (InfoMethod  unique tid fix nt (Just arity) iClass) = 1
-- Wrong !!! 
-- arityI (InfoIMethod unique tid (NewType _ _ ctxs _) (Just arity) iMethod) =
-- length ctxs + arity
arityI (InfoDMethod  unique tid  (NewType _ _ ctxs _) (Just arity) iClass) = 
  length ctxs + arity + 1  {- 1 is for the dictionary -}
arityI (InfoName  unique tid arity ptid) = arity
arityI info =  error ("arityI " ++ show info)

arityIM (InfoMethod  unique tid fix (NewType _ _ ctx _) (Just arity) iClass) =
  length ctx + arity

fixityI (InfoVar     unique tid fix exp nt annot) = fix
fixityI (InfoConstr  unique tid fix nt fields iType) = fix
fixityI (InfoMethod  unique tid fix nt annot iClass) = fix
fixityI _ = (InfixDef,9::Int)


belongstoI :: Info -> Int

belongstoI (InfoConstr  unique tid fix nt fields iType) = iType
belongstoI (InfoField   unique tid icon_offs iData iSel) = iData
belongstoI (InfoMethod  unique tid fix nt annot iClass) = iClass
belongstoI (InfoIMethod  unique tid nt annot iMethod) = iMethod  
  -- ^Maybe ought to be it's own function
belongstoI (InfoDMethod  unique tid nt annot iClass) = iClass
belongstoI (InfoInstance unique  nt iClass) = iClass
belongstoI (InfoName  unique tid i ptid) = unique   
  -- ^this is a lie! but it is consistent with constrsI :-)
belongstoI info =  error ("belongstoI " ++ show info)


profI :: Info -> TokenId

profI (InfoData   unique tid exp nt dk) = tid
profI (InfoClass  u tid _ _ _ _ _) = tid
profI (InfoVar     u tid _ _ _ _) = tid
profI (InfoConstr  u tid _ _ _ _) = tid
profI (InfoField   u tid icon_offs iData iSel) = tid
profI (InfoMethod  u tid _ _ _ _) = tid
profI (InfoIMethod  u tid _ _ _) = tid
profI (InfoDMethod  u tid _ _ _) = tid
profI (InfoName  u tid _ ptid) = ptid
profI info = error ("profII (Info.hs) " ++ show info)
