module Info(module Info, Kind,TokenId,NewType,InfixClass(..),Pos(..),AssocTree(..),Tree{-,PackedString-}) where

import Kind(Kind)
import TokenId(TokenId)
import NT
import Extra(Pos(..),sndOf,strace)
import PackedString(PackedString)
import Tree234
import AssocTree
import Syntax(InfixClass(..))

data IE = IEnone | IEsel | IEabs | IEall deriving (Eq,Show) 

isExp IEnone = False
isExp IEsel = False
isExp _      = True

combIE IEall  _ = IEall
combIE IEnone i = i
combIE _ IEall  = IEall
combIE i IEnone = i
combIE _      i = i

data DataKind =  -- Bool tells if type will be unboxed after expension
    DataTypeSynonym Bool Int	  -- depth (used to determine which type synonym to expand)
  | DataNewType	    Bool [Int]    -- constructor(one or zero) 
  | Data 	    Bool [Int]    -- constructors
  | DataPrimitive   Int	          -- size
  deriving (Show)

data Info =
    InfoClear   -- used to remove imported when redefining in mutaly recursive modules and when compiling the prelude
  | InfoUsed      Int [(Kind,TokenId,PackedString,Pos)]
  | InfoUsedClass Int [(Kind,TokenId,PackedString,Pos)]  (AssocTree Int ([Int],[(Int,Int)]))  -- the tree does con -> (free,ctxs)

  | InfoData         Int TokenId IE NewType DataKind 
  | InfoClass        Int TokenId IE NewType [Int] [Int] (AssocTree Int ([Int],[(Int,Int)]))    -- the tree does con -> (free,ctxs)
  | InfoVar          Int TokenId (InfixClass TokenId,Int) IE NewType (Maybe Int)
  | InfoConstr       Int TokenId (InfixClass TokenId,Int) NewType [Maybe Int] Int
  | InfoField        Int TokenId [(Int,Int)] Int Int	-- unique tid [(constructor,offset)] type selector
  | InfoMethod       Int TokenId (InfixClass TokenId,Int) NewType (Maybe Int) Int
  | InfoIMethod      Int TokenId NewType (Maybe Int) Int    -- The type is NewType free instancs_ctx instance_type, for real type follow int
  | InfoDMethod      Int TokenId NewType (Maybe Int) Int
  | InfoInstance     Int NewType Int			-- Only used in Export
  | InfoName         Int TokenId Int TokenId		-- inserted late to hold name and arity for some functions (second TokenId is profname )
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

clearI _ = InfoClear


--isClear InfoClear = True
--isClear _ = False

isMethod (InfoMethod unique tid fix nt annot iClass) = True
isMethod _ = False

isData (InfoData   unique tid exp nt dk) = True
isData _ = False

isRealData (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> False
	(DataNewType unboxed constructors) -> False
	(DataPrimitive size) -> True
	(Data unboxed  constrs) -> True
isRealData info = error ("isRealData " ++ show info)

isDataUnBoxed (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> unboxed
	(DataNewType unboxed constructors) -> unboxed
	(Data unboxed  constrs) -> unboxed
	(DataPrimitive size) -> True
isDataUnBoxed info = error ("isDataUnBoxed: " ++ show info)

isField (InfoField _ _ _ _ _) = True
isField _ = False

isClass (InfoClass _ _ _ _ _ _ _) = True
isClass _ = False

depthI (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) -> Just depth
	_ -> Nothing
depthI _ = Nothing

updTypeSynonym unboxed depth (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym _ _) ->
	  (InfoData   unique tid exp nt (DataTypeSynonym unboxed depth)) 

updNewType unboxed (InfoData   unique tid exp nt dk) =
      case dk of
	(DataNewType _ constructors) -> InfoData   unique tid exp nt (DataNewType unboxed constructors)


-- newNT is only applied to identifiers without types, i.e. never methods of any kind!
newNT nt (InfoVar unique tid fix exp _ annot) =  InfoVar unique tid fix exp nt annot



ntI (InfoData   unique tid exp nt dk) = nt
-- ntI (InfoClass  unique tid exp nt ms ds) =  nt   --- Not needed?
ntI (InfoVar     unique tid fix exp nt annot) = nt
ntI (InfoConstr  unique tid fix nt fields iType) = nt
ntI (InfoMethod  unique tid fix nt annot iClass) = nt
ntI (InfoIMethod  unique tid nt annot iMethod) = nt  -- Work here?
ntI (InfoDMethod  unique tid nt annot iClass) = nt

strictI (InfoConstr  unique tid fix (NewType free [] ctx nts) fields iType) = map strictNT (init nts)
strictI _ = []  -- Not strict in any argument so it doesn't matter if we return empty list

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

tidI (InfoData   unique tid exp nt dk) = tid
tidI (InfoClass  u tid _ _ _ _ _) = tid
tidI (InfoVar     u tid _ _ _ _) = tid
tidI (InfoConstr  u tid _ _ _ _) = tid
tidI (InfoField   u tid icon_offs iData iSel) = tid
tidI (InfoMethod  u tid _ _ _ _) = tid
tidI (InfoIMethod  u tid _ _ _) = tid
tidI (InfoDMethod  u tid _ _ _) = tid
tidI (InfoName  u tid _ _) = tid
tidI info = error ("tidI (Info.hs) " ++ show info)

methodsI (InfoClass u tid e nt ms ds inst) = zip ms ds
instancesI (InfoClass u tid e nt ms ds inst) = inst
instancesI info@(InfoUsedClass u uses inst) = strace ("***instanceI(1) " ++ show info ++ "\n") inst
instancesI info = strace ("***instanceI(2) " ++ show info ++ "\n") initAT -- This is a lie!!! For some reason has this class no real entry
superclassesI (InfoClass u tid e (NewType free [] ctxs nts) ms ds inst) = map fst ctxs
superclassesI info = error ("superclassesI " ++ show info)

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

-- joinInsts :: AssocTree Int ([Int],[(Int,Int)]) -> AssocTree Int ([Int],[(Int,Int)]) -> AssocTree Int ([Int],[(Int,Int)])
joinInsts inst inst' =
  foldr ( \ (k,v) inst -> addAT inst sndOf k v) inst (treeMapList (:) inst')

constrsI (InfoName  unique tid i ptid) = [unique]   -- this is a lie! but it is consistent with belongstoI :-)
constrsI (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) ->  strace ("Constr of type synonym " ++ show tid) []
	(DataNewType unboxed constructors) -> constructors
	(DataPrimitive size) ->  strace ("Constr of data primitive " ++ show tid) []
	(Data unboxed  constrs) -> constrs
constrsI info = error ("constrsI " ++ show info)

updConstrsI (InfoData   unique tid exp nt dk) constrs' =
      case dk of
	(Data unboxed  constrs) -> InfoData   unique tid exp nt (Data unboxed  constrs')

filedsI (InfoConstr unique tid fix nt fields iType) = fields

combInfo  InfoClear                       info'                     = info'
combInfo (InfoUsed _ w)                  (InfoUsed u' w')           = InfoUsed u' (w++w')
combInfo (InfoUsed _ _)                   info'                     = info'
combInfo  info                            InfoClear                 = info
combInfo  info                           (InfoUsed _ _)             = info
combInfo i1@(InfoUsedClass _ uses insts) i2@(InfoClass u tid exp nt ms ds insts') =
	 InfoClass u tid exp nt ms ds (joinInsts insts' insts)
combInfo i1@(InfoClass _ tid exp nt ms ds insts) i2@(InfoUsedClass u uses insts') =
	 InfoClass u tid exp nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid exp nt ms ds insts) (InfoClass u' tid' exp' nt' [] [] insts') =	
	 InfoClass u tid (combIE exp exp') nt ms ds (joinInsts insts' insts)
combInfo (InfoClass u tid exp nt ms ds insts) (InfoClass u' tid' exp' nt' ms' ds' insts') =	
	 InfoClass u tid (combIE exp exp') nt' ms' ds' (joinInsts insts' insts)
combInfo info@(InfoData u tid exp nt dk) info'@(InfoData u' tid' exp' nt' dk')  =
  case dk' of
    Data unboxed [] -> info
    _ -> if isExp exp' then info' else info
combInfo info                        info'                            =  -- Use new (if possible) so that code can override old imported
	if isExp (expI info)
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
arityI (InfoVar     unique tid fix exp (NewType _ _ ctxs _) (Just arity)) =  length ctxs + arity
arityI (InfoVar     unique tid fix exp NoType (Just arity)) =  arity   -- NR Generated after type deriving
arityI (InfoConstr  unique tid fix (NewType _ _ _ nts) fields iType) = length nts - 1
arityI (InfoMethod  unique tid fix nt (Just arity) iClass) = 1
-- Wrong !!! -- arityI (InfoIMethod unique tid (NewType _ _ ctxs _) (Just arity) iMethod) = length ctxs + arity
arityI (InfoDMethod  unique tid  (NewType _ _ ctxs _) (Just arity) iClass) = length ctxs + arity + 1  {- 1 is for the dictionary -}
arityI (InfoName  unique tid arity ptid) = arity
arityI info =  error ("arityI " ++ show info)

arityIM (InfoMethod  unique tid fix (NewType _ _ ctx _) (Just arity) iClass) = length ctx + arity

fixityI (InfoVar     unique tid fix exp nt annot) = fix
fixityI (InfoConstr  unique tid fix nt fields iType) = fix
fixityI (InfoMethod  unique tid fix nt annot iClass) = fix
fixityI _ = (InfixDef,9::Int)


belongstoI (InfoConstr  unique tid fix nt fields iType) = iType
belongstoI (InfoField   unique tid icon_offs iData iSel) = iData
belongstoI (InfoMethod  unique tid fix nt annot iClass) = iClass
belongstoI (InfoIMethod  unique tid nt annot iMethod) = iMethod  -- Maybe ought to be it's own function
belongstoI (InfoDMethod  unique tid nt annot iClass) = iClass
belongstoI (InfoInstance unique  nt iClass) = iClass
belongstoI (InfoName  unique tid i ptid) = unique   -- this is a lie! but it is consistent with constrsI :-)
belongstoI info =  error ("belongstoI " ++ show info)

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
