module Export(export,strExport) where

import List
import Info
import NT
import IntState
import Scc
import AssocTree
import Extra
import Tree234(treeMapList)
import TokenId
import PackedString(PackedString,packString,unpackPS)
import MergeSort(unique)
import Syntax(InfixClass(..))
import Nice
import IExtract(defFixity)
import Flags
import Memo
--import NonStdProfile
profile a b = b

export flags state =
  let infoExport =  (filter (isExported . expI) . treeMapList ((:).snd) . getSymbolTable) state

      insts = ( foldr ( \ (InfoClass  unique tid exp nt ms ds insts) r -> 
			foldr (fixInst state (sPrelude flags || notPrelude tid) unique) r (treeMapList (:) insts)) []
	      . filter isClass
	      . treeMapList ((:).snd)
	      . getSymbolTable
	      ) state

      mrps = mrpsIS state
  in case uniqueISs state insts of
    (insts,state) ->
      let 
	(infoInst,depInst) = unzip (map ( \ ((cls,nt,dep),i) -> (InfoInstance i nt cls,(i,dep))) insts)
        depExport = map infoDepend infoExport

	depExtra = profile "getAll start" (getAll state (foldr (flip addM) initM (map fst depExport)) (concatMap snd depExport ++ concatMap snd depInst))

	depend = reverse (profile "start sccDepend" (sccDepend (depExport ++ depExtra ++ depInst)))
        expTree = foldr ( \ info tree -> addAT tree sndOf (uniqueI info) info) initAT (infoInst++infoExport)

	declExport = (map (\ xs -> if all isLeft xs
		                  then (False,map dropLeft xs)
		                  else (True,map dropEither xs)    -- !!! Not good, more is exported than should have been 
			  )
		     . filter (not . null) 
		     . map (fixInfo (sPrelude flags) state expTree)
		     ) depend

        infExport = filter ( (/=defFixity) . snd) (concatMap (concatMap getFixity . snd) (filter fst declExport))

        getFixity (InfoData   unique tid IEall nt dk) =
	  case dk of
	    Data unboxed constrs -> 
	      map (( \ info -> (tidI info,fixityI info)) . dropJust . lookupIS state) constrs
	    _ -> []
        getFixity (InfoData   unique tid _ nt dk) = []
        getFixity (InfoClass  unique tid exp nt  ms ds insts) = 
	  map (( \ info -> (tidI info,fixityI info)) . dropJust . lookupIS state) ms
        getFixity (InfoVar    unique tid fix exp nt annot) = [(tid,fix)]
        getFixity (InfoInstance unique nt iClass) = []
        getFixity x = error ("getFixity = " ++ show x)

      in 
	(infExport,declExport)

 where

  getAll state found [] = profile "getAll end" []
  getAll state found (u:us) =
    if elemM found u
    then getAll state found us
    else 
      case (infoDepend . dropJust . lookupIS state) u of
        depend@(u,dep) -> depend : getAll state (addM found u) (dep ++ us)


  infoDepend (InfoData   unique tid exp nt dk) =
      case dk of
	(DataTypeSynonym unboxed depth) ->  (unique,useNewType nt)
	(DataNewType unboxed constructors) ->  dataDepend exp nt constructors
	(DataPrimitive size) -> (unique,useNewType nt)
	(Data unboxed constrs) -> dataDepend exp nt constrs
    where
      dataDepend exp nt constrs =
	if exp == IEabs then      (unique,useNewType nt) 
	else (unique,snub (useNewType nt ++ concatMap (useConstr . lookupIS state) constrs))

  infoDepend (InfoClass  unique tid IEall nt ms ds insts) = (unique,snub (useNewType nt ++ concatMap (useMethod . lookupIS state) ms))
  infoDepend (InfoClass  unique tid _     nt ms ds insts) = (unique,snub (useNewType nt))
  infoDepend (InfoVar    unique tid fix exp nt annot) = (unique,useNewType nt)
  infoDepend info = error ("infoDepend " ++ show info)

fixInst state keep unique (con,(free,ctxs)) r =
  if keep || (notPrelude . tidI . dropJust . lookupIS state) con then
    (unique,NewType free [] ctxs [NTcons con (map NTvar free)],snub (con:map fst ctxs)):r
  else
    r

dropDepend [] = []
dropDepend (NoRec d:r) = d : dropDepend r 
dropDepend (Rec ds:r) = ds ++ dropDepend r

getModule (Qualified m n) = Just m
getModule _ = Nothing

fixInfo keep state ds (NoRec n) = fixInfo' keep state ds n
fixInfo keep state ds (Rec ns) = concatMap ( fixInfo' keep state ds) ns

fixInfo' keep state ds n =
  case lookupAT ds n of
    Just info -> [Right info]
    Nothing ->
      case lookupIS state n of
	Just info ->
	  if (keep || notPrelude (tidI info))
	  then [Left info]
	  else []


useNewType (NewType free exist ctxs nts) = snub (map fst ctxs ++ (concatMap useNT nts))

useConstr (Just (InfoConstr  unique tid fix nt fields iType)) = useNewType nt

useMethod (Just (InfoMethod  unique tid fix nt annot iClass)) = useNewType nt
useMethod x = error ("No match in useMethod:" ++ show x)

strExport keep modidl state (fixs,exps) =
  ( showString ("interface " ++ reverse (unpackPS modidl) ++ " where {\n")
  . foldr ((.).showsFix modrps) id fixs
  . foldr ((.).showsHide modrps) id (optExport False Nothing (map preExport exps))
  ) "}\n"

 where
  modrps = mrpsIS state

  preExport (visible,infos@(InfoInstance u nt iClass:_)) =
    ((visible,Nothing),infos)
  preExport (visible,infos@(info:_)) = 
    case (extractM . tidI) info of
      rps -> ((visible,Just rps),infos)

  optExport preV preRps [] = []
  optExport preV preRps (((v,rps),infos):xs) =
    (preV == v && preRps == rps,v,rps,infos) : optExport v rps xs

  showsFix mrps (tid,(InfixPre i,l)) =
    showString "prefix ". niceTid state i . showChar ' ' . shows l . showChar ' ' . showsOp (fixTid mrps tid) . showString ";\n"
  showsFix mrps (tid,(InfixDef ,l)) = id
  showsFix mrps (tid,(inf,l)) =  shows inf . showChar ' ' . shows l . showChar ' ' . showsOp (fixTid mrps tid) . showString ";\n"

  showsExp rps infos =
    showString "{-# NEED" . foldr ((.).showsNeed rps) id infos . showString " #-}\n" .
    foldr ((.).showsInfo rps) id infos

  showsHide mrps (prev,visible,rps,infos) =  -- Would have prefered not to use qualified names, but have to
	(if prev || isNothing rps then id
          else (if visible then showString "interface ! " else showString "interface ") . (showString . reverse . unpackPS . dropJust) rps)
	. showString "\n{-# NEED" . foldr ((.).showsNeed (dropJust rps)) id infos . showString " #-}\n" -- need does not need to be qualified
	. foldr ((.).showsInfo (dropJust rps)) id infos						  -- but the definitions must


 	-- Hack for tuples
  showsNeed mrps (InfoData   unique (TupleId n) exp nt dk) = id  -- Always look in tuple definitions
  showsNeed mrps (InfoData   unique tid exp nt dk) =
      case dk of
	(DataNewType unboxed constructors) -> groupNeed mrps exp tid constructors
	(Data unboxed  constrs) -> groupNeed mrps exp tid constrs
	_ ->  showChar ' ' . showsVar (fixTid mrps tid) 
  showsNeed mrps (InfoClass  unique tid exp nt ms ds insts) = groupNeed mrps exp tid ms
  showsNeed mrps (InfoVar     unique tid fix exp nt annot) = showChar ' '.showsVar (fixTid mrps tid)
  showsNeed mrps (InfoConstr  unique tid fix nt fields iType) = showChar ' '. showsVar (fixTid mrps tid) . foldr ((.) . showsField mrps) id fields
  showsNeed mrps (InfoMethod  unique tid fix nt annot iClass) = showChar ' ' . showsVar (fixTid mrps tid)
  showsNeed mrps (InfoInstance unique  nt iClass) = id

  groupNeed mrps exp group parts =
	if exp == IEall && not (null parts) then 
	  showString " {" . showsVar (fixTid mrps group) . foldr ((.) . showsNeed mrps . dropJust . lookupIS state) id parts . showChar '}'
	else
	  showChar ' ' . showsVar (fixTid mrps group) 

  showsField mrps Nothing = id
  showsField mrps (Just i) = showChar ' ' . shows (fixTid mrps (tidIS state i))

 	-- Hack for tuples
  showsInfo mrps (InfoData   unique (TupleId nargs) exp nt@(NewType free exist ctxs nts) dk) =
      let arg = mkAL free
	  al = arg ++ zip (map snd ctxs) (map (('_':).(:[])) ['a'..'z']) -- a-z is to short!
	  strNewType = niceCtxs Nothing state al ctxs ++ mixSpace (map (niceNT Nothing state al) nts)
	  strArgs = concatMap ((' ':).snd) arg
	  strTuple = if nargs > 0 then take nargs ('(':repeat ',') ++ ")" else "()"
      in
	case dk of
	  (DataTypeSynonym unboxed depth) ->
	    showString "type {-# " . shows depth . (if unboxed then showString " !" else id) . showString " #-} " .
		showString strTuple . showString strArgs . showString  " = " .
	    	showString strNewType . showString ";\n"
	  (DataNewType unboxed constrs) ->
	    showString "newtype {-# " . (if unboxed then showChar '!' else id) . showString " #-} "
		. showString (niceCtxs Nothing state al ctxs)
		. showString strTuple . showString strArgs
		. (if exp == IEall  && not (null constrs) then
	 	      showString "\n = " .  showString (mix "\n  | " (map (expConstr mrps al . lookupIS state) constrs)) else id)
		. showString ";\n"
	  (DataPrimitive size) ->
	    if nargs == 0 then
	      showString "data primitive () = " . shows size . showString ";\n"
            else
	      error ("showsInfo in Export can not handle primitive TupleID with " ++ show nargs ++ " arguments.")
	  (Data unboxed constrs) ->
	    showString ("data " ++ (if unboxed then "unboxed " else "") ++ niceCtxs (Just mrps) state al ctxs) . 
		 showString strTuple . showString strArgs
		 . (if exp == IEall && not (null constrs) then
			 showString "\n = " .  showString (mix "\n  | " (map (expConstr mrps al . lookupIS state) constrs)) else id)
		 . showString ";\n"
  showsInfo mrps (InfoData   unique tid exp nt@(NewType free exist ctxs nts) dk) =
      let arg = mkAL free
	  al = arg ++ zip (map snd ctxs) (map (('_':).(:[])) ['a'..'z']) -- a-z is to short!
	  strNewType = niceCtxs (Just mrps) state al ctxs ++ mixSpace (map (niceNT (Just mrps) state al) nts)
	  strArgs = concatMap ((' ':).snd) arg
      in
	case dk of
	  (DataTypeSynonym unboxed depth) ->
	    showString "type {-# " . shows depth . (if unboxed then showString " !" else id) . showString " #-} " .
		showsVar (fixTid mrps tid) . showString (strArgs ++ " = ") . showString strNewType . showString ";\n"
	  (DataNewType unboxed constrs) ->
	    showString "newtype {-# " . (if unboxed then showChar '!' else id) . showString " #-} "
		. showString (niceCtxs Nothing state al ctxs) . showsVar (fixTid mrps tid) . showString strArgs
		. (if exp == IEall && not (null constrs) then
			 showString "\n  = " . showString  (mix "\n  | " (map (expConstr mrps al . lookupIS state) constrs)) else id)
		. showString ";\n"
	  (DataPrimitive size) ->
	    showString "data primitive " . showsVar (fixTid mrps tid) . showString " = " . shows size . showString ";\n"
	  (Data unboxed constrs) ->
	    showString ("data " ++ (if unboxed then "unboxed " else "") ++ niceCtxs (Just mrps) state al ctxs) . showsVar (fixTid mrps tid) 
		. showString strArgs 
		. (if exp == IEall && not (null constrs) then
			 showString "\n  = " . showString  (mix "\n  | " (map (expConstr mrps al . lookupIS state) constrs)) else id)
		. showString ";\n"

  showsInfo mrps (InfoClass  unique tid exp (NewType free exist ctxs nts)  ms ds insts) = 
    let al = mkAL free
    in showString "class " . showString (niceCtxs Nothing state al ctxs) . showsVar (fixTid mrps tid)
		 . showString (concatMap ((' ':).snd) al)
		 . (if exp == IEall && not (null ms) then
			 showString " where {\n" . showString (concatMap (expMethod mrps . lookupIS state) ms) . showString "};\n"
		    else
		      showString ";\n")
  showsInfo mrps (InfoVar    unique tid fix exp nt annot) =
	showsVar (fixTid mrps tid) . showsAnnot annot . showString "::" . showString (niceNewType state nt) . showString ";\n"
  showsInfo mrps (InfoInstance unique (NewType free exist ctxs [nt]) iClass) =
	let al = mkAL free
	in showString "instance " . showString (niceCtxs Nothing state al ctxs) . niceInt Nothing state iClass .  showChar ' ' . showString (niceNT Nothing state al nt) . showString ";\n"

  showsAnnot Nothing = id
  showsAnnot (Just a) = showString "{-# " . shows a . showString " #-}"

  expConstr mrps al (Just (InfoConstr  unique (TupleId nargs) fix (NewType free exist ctxs nts) field iType)) =
       (showString ( if nargs > 0 then take nargs ('(':repeat ',') ++ ") " else "()"))  (mixSpace (map (niceField state al) (zip field (init nts))))
  expConstr mrps al (Just (InfoConstr  unique tid fix (NewType free exist ctxs ectxs_nts) field iType)) =
	-- al contains type variables to string mapping
	-- exist contains existential type variables not in al
     let exist' = zip exist (map (:"_fa") ['a'..'z']) -- a-z is to short!
	 al' = al ++ exist'
         ectxs = map ntContext2Pair (filter contextNT ectxs_nts)
         nts = filter (not . contextNT) ectxs_nts
     in
	  (if null exist then "" else "forall " ++ mixSpace (map snd exist') ++ " . ") ++
          niceCtxs Nothing state al' ectxs ++
          (showsVar (fixTid mrps tid) . showChar ' ')  (mixSpace (map (niceField state al') (zip field (init nts))))

  expMethod mrps (Just (InfoMethod  unique tid fix nt annot iClass)) =
	(showString "  " . showsVar (fixTid mrps tid) . showsAnnot annot . showString "::" . showString (niceNewType state nt))  ";\n"


