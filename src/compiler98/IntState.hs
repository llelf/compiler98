module IntState(module IntState, module Info, AssocTree(..), Tree) where

import AssocTree
import NT
import IdKind
import TokenId(TokenId,mkQual3,mkQualD,dropM)
import Extra
import PackedString(PackedString,packString,unpackPS)
import Info
import MergeSort(group)

data IntState = 
      IntState
	Int				-- unique
	(Int,PackedString)		-- modid
	(AssocTree Int Info)		-- symboltable (unique int -> info)
	[String]			-- errors

dummyIntState = IntState 0 (0,packString "<Dummy>") initAT []

-- -===== State

getInfo i down state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just info -> (info,state)

addDefaultMethod tidcls (iMethod,iDefault) state@(IntState unique irps@(_,rps) st errors) =
  case lookupIS state iMethod of
    Just (InfoMethod u tid fix nt' annot iClass) ->
      IntState unique irps (addAT st fstOf iDefault (InfoDMethod  iDefault (mkQualD rps tid) nt' annot iClass)) errors

getUnique down state@(IntState unique rps st errors) =
  (unique,IntState (unique+1) rps st errors)

addInstMethod  tidcls tidtyp tidMethod nt iMethod down state@(IntState unique rps st errors) =
  case lookupIS state iMethod of
    Just (InfoMethod u tid fix nt' (Just arity) iClass) -> 
      (unique,IntState (unique+1) rps (addAT st (error "adding twice!") unique (InfoIMethod unique (mkQual3 tidcls tidtyp tidMethod) nt (Just arity) iMethod)) errors)

-- -====== State0

updInstMethodNT tidcls tidtyp i nt iMethod  down state@(IntState unique rps st errors) =
  case lookupAT st iMethod of
    Just (InfoMethod _  _  _ _ annots _) ->
      case lookupAT st i of
	Just (InfoIMethod u tid' _ _ _) ->
	    let tid = mkQual3 tidcls tidtyp (dropM tid')
	    in IntState unique rps (addAT st fstOf i (InfoIMethod u tid nt annots iMethod)) errors

addInstance cls con free ctxs down state@(IntState unique rps st errors) =
  let st' = updateAT st cls (addInstanceI con free ctxs)
  in  IntState unique rps st' errors

addNewLetBound i tid down state =
  addIS i (InfoVar i tid (InfixDef,9) IEnone NoType Nothing) state

-- -==== Reduce

updVarNT pos i nt state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just (InfoVar u tid fix exp NoType annots) ->
        case checkNT pos (strIS state) nt of
          Nothing -> IntState unique rps (addAT st fstOf i (InfoVar u tid fix exp nt annots)) errors
	  Just err -> IntState unique rps st (err :errors)
    Just (InfoVar u tid fix exp nt' annots) ->
	IntState unique rps st (("New type signature for " ++ show tid ++ " at " ++ strPos pos):errors)

updVarArity pos i arity state@(IntState unique rps st errors) =
  case lookupAT st i of
    Just (InfoVar  u tid fix exp nt _) ->  -- Always update, might change arity for redefined import in Prelude
        IntState unique rps (addAT st fstOf i (InfoVar u tid fix exp nt (Just arity))) errors
    _ -> state   -- Ignore arity for methods, methods instances and methods default

-- -==== Stand alone

addIS u info state@(IntState unique rps st errors) =
  IntState unique rps (addAT st combInfo u info) errors

lookupIS (IntState unique rps st errors) i = lookupAT st i

updateIS (IntState unique rps st errors) i upd =
  IntState unique rps (updateAT st i upd) errors

uniqueIS (IntState unique rps st errors) = (unique,IntState (unique+1) rps st errors)

uniqueISs (IntState unique rps st errors) l =
   (zip l [unique..],IntState (unique+(length l)) rps st errors)

strIS state i =
   case lookupIS state i of
     Just info -> show (tidI info)
     Nothing -> 'v':show i

tidIS state i =
   case lookupIS state i of
     Just info -> tidI info

getErrors (IntState unique rps st errors) = (IntState unique rps st [], errors)
addError (IntState unique rps st errors) err = IntState unique rps st (err:errors)

getSymbolTable (IntState unique rps st errors) = st

mrpsIS (IntState unique (i,rps) st errors) = rps
miIS (IntState unique (i,rps) st errors) = i

getIndDataIS state indDataI =
  case constrsI indDataI of
    (c:_) ->   -- Can only be one constructor 
      case lookupIS state c of
	(Just infoCon) ->
          case ntI infoCon of
	    (NewType ctx free [] (NTcons con _:_)) -> con

globalIS state i =
  case lookupIS state i of
    Nothing -> False
    Just info -> globalI info
 where
  globalI (InfoData   unique tid exp nt dk) = isExp exp
  globalI (InfoClass  unique tid exp nt ms ds insts) = isExp exp
  globalI (InfoVar     unique tid fix IEsel nt annot) = True
  globalI (InfoVar     unique tid fix exp nt annot) = isExp exp
  globalI (InfoConstr  unique tid fix nt fields iType) = globalI' (lookupIS state iType) 
  globalI (InfoField   unique tid icon_offs iData iSel) = globalI' (lookupIS state iData) 
  globalI (InfoMethod  unique tid fix nt annot iClass) = True
  globalI (InfoIMethod  unique tid nt annot iMethod) = True
  globalI (InfoDMethod  unique tid nt annot iClass) = True
  globalI (InfoName  unique tid arity ptid) = False

  globalI' (Just (InfoData   unique tid IEall nt dk)) = True
  globalI' _ = False


-- arity with context
arityIS state i =
  case lookupIS state i of 
    Just (InfoIMethod  unique tid (NewType _ [] ctxs [NTcons tcon _]) (Just arity) iMethod) ->
	case lookupIS state iMethod of
	  Just (InfoMethod  unique tid fix (NewType _ [] ictxs _) (Just iarity) iClass) ->
	       length ictxs + iarity + (length . snd . dropJust . lookupAT ((instancesI . dropJust . lookupIS state) iClass)) tcon

--	       length ctxs + arity + (length . snd . dropJust . lookupAT ((instancesI . dropJust . lookupIS state) iClass)) tcon
--	       length ictxs + length ctxs + arity 
    Just info@(InfoIMethod  unique tid _ _ iMethod) -> error ("arityIS " ++ show info)
    Just info -> arityI info
    _ -> error ("arityIS in IntState.hs couldn't find " ++ show i)


-- Not a good place

checkNT pos strFun (NewType free [] ctxs nts) =
  case (filter ((1/=) . length) . group) ctxs of
    [] -> Nothing
    [x] -> Just ("Multiple occurences of " ++ (strFun . fst . head) x ++
                  " with identical type variable in context close to " ++ strPos pos) 
    xs -> Just ("Multiple occurences of " ++ mixCommaAnd (map (strFun . fst . head) xs) ++
                  " with identical type variables in context close to " ++ strPos pos) 
