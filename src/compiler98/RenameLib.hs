{- ---------------------------------------------------------------------------
-}
module RenameLib(module RenameLib
	,AssocTree
	,Tree ,ImportState,NT,NewType,IE,Either,Info
	,Maybe,Flags) where

import List
import TokenId(TokenId(..),t_Tuple,ensureM,mkQual2,visible,mkQual3,mkQualD
              ,rpsPrelude,forceM)
import Syntax hiding (TokenId)
import Scc
import NT
import Extra

import AssocTree
import ImportState hiding (TokenId)
import IntState(checkNT)
import IExtract(fixOne,fixFun)
import State
import IdKind
import PreImp(sExp,sQual,sLG,sFix)
import SysDeps(PackedString,unpackPS)
import Info hiding (TokenId)
import SyntaxPos
import TokenInt
import OsOnly(isPrelude)
import Flags
import Overlap		-- added in H98
import Id(Id)

data RenameState =
      RenameState
        Flags				-- flags
	Id				-- unique
	(Id,PackedString)		-- modid
	[AssocTree (TokenId,IdKind) Id] -- stack of rename (name -> unique)
	(AssocTree (TokenId,IdKind) Id) -- active rename (name	-> unique)
	(AssocTree Id Info)		-- symboltable (unique -> info)
	[(Id,[(Pos,Id)])]		-- derived   [(con,[(pos,cls)])]
	(Maybe [Id])			-- defaults
	[String]			-- errors
	[Id]				-- type Synonyms

--- The selectors for (qualFun,expFun,fixity) are defined
--- in PreImp and used here and in Fixity

{- Used several times: -}
type RenameToken = (PackedString -> Int -> TokenId -> TokenId
                   ,TokenId -> TokenId
                   ,TokenId -> IdKind -> IE
                   ,TokenId -> (InfixClass TokenId,Int)
                   ) 
type RenameToken2 = (PackedString -> Int -> TokenId -> TokenId
                   ,TokenId -> TokenId
                   ,TokenId -> IdKind -> IE
                   ) 
type RenameMonad  a      = State  RenameToken RenameState a RenameState
type RenameRMonad a b    = State  a           RenameState b RenameState
type RenameMonadEmpty    = State0 RenameToken RenameState RenameState
type RenameRMonadEmpty a = State0 a           RenameState RenameState

{-
Destruct rename state to obtain all its elements we are interested in.
Additionally checks some properties of types.
-}
keepRS :: RenameState 
       -> (Int
          ,((TokenId,IdKind) -> Int,(TokenId,IdKind) -> Maybe Int)
          ,(Int,PackedString)
          ,AssocTree Int Info   -- the symbol table
          ,[(Int,[(Pos,Int)])]  -- derived
          ,Maybe [Int]          -- user defined defaults for Num classes
          ,[String])            -- errors

keepRS (RenameState flags unique rps rts rt st derived
                    defaults errors needCheck) =
  case checkTypes st needCheck of
    Right st ->
      (unique,getInts (lookupAll (rt:rts)),rps,st,derived,defaults,errors)
    Left x ->
      (unique,getInts (lookupAll (rt:rts)),rps,st,derived,defaults,errors ++ x)
  where
  checkTypes :: AssocTree Id Info -> [Id] -> Either [String] (AssocTree Id Info)
  checkTypes st needCheck =
    case foldls (checkPrep st) ([],[]) needCheck of   
      -- !!! Do these checks at defining site only !!!
      (synType,newType) -> -- first look for error in type synonym defs
	let keep = map fst synType
	    sccSyn = sccDepend 
                       (map ( \ (u,d) -> (u,filter (`elem` keep) d)) synType)
        in
	  case (filter isRec sccSyn) of
	    x@(_:_) -> Left (map (err2 st) x)
	    [] -> -- now look for error in newtype defs
	      case foldls ( \ (st,err) (u,c) -> 
				case isUnBoxedNT st newType [u] c of
				  Just unboxed -> 
                                    (updateAT st u (updNewType unboxed),err)  
                                    -- WORKING ON
				  Nothing -> 
                                    (updateAT st u (updNewType False)
                                    ,("Newtype " ++ 
                                      (show . tidI . dropJust . lookupAT st) u 
                                      ++ " could be circular.") : err)
			  ) (st,[]) newType of
	        (st,err@(_:_)) -> Left err
	 	(st,[]) -> Right (snd (foldls fixDepth (0::Int,st) sccSyn))

  fixDepth (d,st) (NoRec u) =
    let unboxed = isUnBoxedTS st u
        d' = 1 + max d (minDepth st u) 
    in unboxed `seq` d' `seq` (d',updateAT st u (updTypeSynonym unboxed d'))

  minDepth st c = -- called for type synonyms only
    case (ntI . dropJust . lookupAT st) c of
      (NewType _ [] _ [nt]) ->
	(maximum . (0:) -- Ensures maximum works
                 . map (safeDepthI . dropJust . lookupAT st) . consNT) nt

  safeDepthI info =
    case depthI info of
      Nothing -> 0
      Just d  -> d


  {- 
  Determines for given newtype type constructor, if the renamed type
  is unboxed. Returns Nothing if definition is circular.
  -}
  isUnBoxedNT :: AssocTree Id Info -- symboltable
              -> [(Id,Id)]  -- for every newtype type constructor 
                            -- the top type constructor of the renamed type
              -> [Id]       -- accumulates newtype type constructors
                            -- that have already been visited to recognise
                            -- circularity.
              -> Id         -- newtype type constructor that is tested
              -> Maybe Bool

  isUnBoxedNT st nt ac u =
    if u `elem` ac then	-- already been here, so circular defn.
      Nothing
    else
    case lookupAT st u of
      Just info ->
	if isRealData info then	-- got the answer!
	  Just (isDataUnBoxed info)
	else
          case depthI info of
	    Just _ ->  -- type synonym, so follow the chain
	      case ntI info of
		(NewType free [] ctx [NTcons u' _ _]) ->
		  isUnBoxedNT st nt (u:ac) u'	      
	    Nothing -> -- newtype, so follow the chain
	      case lookup u nt of
		Just u' -> isUnBoxedNT st nt (u:ac) u'	-- defn in this module
		Nothing ->	-- defn in an imported module
                  -- error ("nhc98 needs a fix here, but I don't know how")
                  case constrsI info of
                    (coni:_) ->
                      case (ntI . dropJust . lookupAT st) coni of
                        (NewType _ _ _ [NTcons u' _ _,_]) ->
                          isUnBoxedNT st nt (u:ac) u'
                        _ -> -- strace 
                      -- ("Warning: renaming newtype of imported newtype:\n"++
                      --  "  Real type of "++show(tidI info)++
                      --                                  " is not visible.\n"++
                      --  "  I might get boxed/unboxed info wrong.")
                          (Just False)
                    [] -> -- strace 
                 --  ("Warning: when renaming newtype of imported newtype:\n"++
                 --   "  Real type of "++show(tidI info)++" is not visible.\n"++
                 --   "  I might get boxed/unboxed info wrong.")
                     (Just False)
      Nothing -> Nothing	-- possibly not circular at all

  isUnBoxedTS st u = -- No circular dependency when this function is called
    case lookupAT st u of
      Nothing -> -- FAKE This is a BUG but unboxed is not used anyway
        False
      Just info ->
        case depthI info of
	  Just _ ->  -- type synonym
	    case ntI info of
	      (NewType free [] ctx [NTcons u' _ _]) ->
	        isUnBoxedTS st u'	      
              _ ->  -- FAKE This is a BUG but unboxed is not used anyway
                False
	  Nothing ->
	    isDataUnBoxed info

  {- 
  Add some information about given type constructor to list,
  either to list about type synonyms or list about newtypes.
  Type constructor must be for type synonym or newtype.
  -}
  checkPrep :: AssocTree Id Info -- symboltable
            -> ([(Id,[Id])],[(Id,Id)]) 
               -- 1 synonym list: type constructor, type cons occuring in rhs
               -- 2 newtype list: type constructor, top type constructor
               --     of renamed type (eg. [] in newtype T = T [Int])
            -> Id -- type constructor 
            -> ([(Id,[Id])],[(Id,Id)])
               -- same format as argument

  checkPrep st (synType,newType) u =
    case lookupAT st u of
      Just info ->
        case depthI info of
	  Just _ ->  -- Only typeSyn has depth
	    case ntI info of
	      (NewType _ [] _ [nt]) ->
	        ((u,consNT nt):synType,newType)
	  Nothing -> -- If it isn't a typeSyn then it must be a newType 
            case constrsI info of
	      (coni:_) ->
		 case (ntI . dropJust . lookupAT st ) coni of
  	           (NewType _ [] _ [NTcons c _ _,res]) -> (synType,(u,c):newType)
  	           (NewType _ [] _ [NTvar v _,res]) -> (synType,(u,v):newType)
  	           (NewType _ [] _ [NTapp v1 v2,res]) -> (synType,newType)
			-- ^ MW hack: omits potential circularity check!
  	           (NewType _ [] _ (_:_:_)) ->
                        error ("Invalid rhs of newtype: " ++
                               show (tidI info)++
                               "\nA newtype can rename only one type.")
                   _ -> error ("Couldn't find rhs of newtype: " ++
                               show (tidI info)++
                               "\nTwo conflicting newtype definitions?")
	      [] -> (synType,newType) -- !!! Not a good solution !!!
      Nothing -> error ("Couldn't find definition for newtype "++show u)

  err2 ts (Rec [x]) = 
    "Circular type synonym " ++ (show . tidI . dropJust . lookupAT ts) x ++ "."
  err2 ts (Rec (x:xs)) = "Circular dependency between the type synonyms "
    ++ concatMap ((++", "). show . tidI . dropJust . lookupAT ts) xs ++ "and "
    ++ (show . tidI . dropJust . lookupAT ts) x ++ "."

-- Only important that it works for data, class, type and newtype
thisModule rps (TupleId _) = rps == rpsPrelude
thisModule rps (Visible _) = False
thisModule rps (Qualified rps' _) = rps == rps'
thisModule rps (Qualified2 _ _) = False		
thisModule rps (Qualified3 _ _ _) = False


{-
Basically transform the importState into a renameState
-}
is2rs :: Flags 
      -> PackedString 
      -> (TokenId -> [TokenId]) 
      -> ((TokenId->Bool) -> a {- TokenId -> IdKind -> IE -}) 
      -> Overlap
      -> ImportState 
      -> Either [String] 
                (TokenId -> TokenId
                ,a
                ,RenameState
                ,AssocTree (TokenId, IdKind) (Either [Pos] [Int])
                )

is2rs flags mrps qualFun expFun overlap 
  (ImportState visible unique orps rps needI irt st insts fixity errors) =
--case treeMapList undef irt of
--  [] ->
--    case foldls reorderFun (treeMap ( \ (k,Right (v:_)) -> (k,v)) irt
--                           ,addAT initAT ignore unique minfo)
--                           (listAT st) of
--      (rt,ts) ->
--          Right (qualFun
--                ,expFun (\_->False)
--                ,RenameState flags (unique+1) (unique,pmrps) [] rt ts []
--                   Nothing errors []
--                ,irt)
--  xs -> Left (map err1 xs)
  case deAlias qualFun overlap irt of
    ([],qf) ->
      case foldls reorderFun 
             (mapAT deRight irt,addAT initAT ignore unique minfo) 
             (listAT st) of
        (rt,ts) ->
            Right (qf
                  ,expFun (\_->False)
                  ,RenameState flags (unique+1) (unique,pmrps) [] rt ts [] 
                     Nothing errors []
                  ,irt)
    (xs,_) -> Left xs
 where
  deRight (Right (v:_)) = v
  deRight (Left _)      = error ("Tripped over aliased identifier")
  pmrps = if (isPrelude . reverse . unpackPS) mrps then rpsPrelude else mrps
  mtid = Visible pmrps
  minfo = InfoName unique mtid 0 mtid False --PHtprof

  reorderFun (rt,at) (key,info) =
    let u = uniqueI info
        rt' = if thisModule rps (fst key) then
		addAT rt ignore key u
	      else
		rt
    in seq rt' (rt',addAT at ignore u info)

  ignore a b = b  -- Happens due to mutally recursive modules

--  undef (key,Left poss) err = Left (key,poss) : err
--  undef (key,Right [x]) err = err
--  undef (key,Right (x:xs)) err =
--	if all (x==) xs then  --- Tuples are entered twice
--	    err
--	else
--	  Right (key,x:xs) : err
--
--  err1 (Left ((tid,Method),poss)) =
--      "The identifier " ++ show tid ++ " instantiated at " ++
--       mix "," (map strPos poss) ++ " does not belong to this class."
--  err1 (Left ((tid,kind),poss)) =
--      show kind ++ ' ':show tid ++ " used at " ++
--      mix "," (map strPos poss) ++ " is not defined."
--  err1 (Right ((tid,kind),xs)) =
--      show kind ++ ' ':show tid ++
--      " defined " ++ show (length xs) ++ " times."


--fixFixityRS ::
--    RenameState ->
--    [(InfixClass TokenId, Int, [FixId TokenId])] ->
--    (TokenId -> (InfixClass TokenId, Int), RenameState)
--
--fixFixityRS (RenameState flags unique irps@(_,rps) rts rt st
--        derived defaults errors needCheck) fixdecls =
-- case foldr (fixOne rps) (initAT,[]) fixdecls of
--  (fixAT,err) -> (fixFun fixAT
--                 ,RenameState flags unique irps rts rt st
--                      derived defaults (err++errors) needCheck)

-- Changed in H98 to:
fixFixityRS ::
    (TokenId -> (InfixClass TokenId, Int)) ->
    RenameState ->
    [(InfixClass TokenId, Int, [FixId TokenId])] ->
    (TokenId -> (InfixClass TokenId, Int), RenameState)

fixFixityRS oldfix rs [] = (oldfix,rs)
fixFixityRS oldfix (RenameState flags unique irps@(_,rps) rts rt st
        derived defaults errors needCheck) fixdecls =
 case foldr (fixOne rps) (initAT,[]) fixdecls of
  (fixAT,err) -> (fixFun fixAT oldfix
                 ,RenameState flags unique irps rts rt st
                      derived defaults (err++errors) needCheck)

--------------------  End duplication


getSymbolTableRS :: RenameState -> AssocTree Id Info
getSymbolTableRS (RenameState flags unique rps rts rt st
                              derived defaults errors needCheck) =
  st


getErrorsRS :: RenameState -> (RenameState,[String])
getErrorsRS (RenameState flags unique rps rts rt st
                         derived defaults errors needCheck) =
  (RenameState flags unique rps rts rt st derived defaults [] needCheck
  ,errors)


pushScope :: a -> RenameState -> RenameState
pushScope _ (RenameState flags unique rps rts rt st
                         derived defaults errors needCheck) =
  RenameState flags unique rps (rt:rts) initAT st derived
              defaults errors needCheck


popScope :: a -> (b,RenameState) -> (b,RenameState)
popScope _ (res, RenameState flags unique rps (rt:rts) _ st
                             derived defaults errors needCheck) =
  (res
  ,RenameState flags unique rps rts rt st derived defaults errors needCheck)


renameError :: String -> a -> RenameMonad a
renameError err r fix (RenameState flags unique rps rst rt st
                                   derived defaults errors needCheck) =
  (r
  ,RenameState flags unique rps rst rt st derived
               defaults (err:errors) needCheck)


{-
Looks up identifier (given as token,kind) in list of trees.
Returns first entry found.
-}
lookupAll :: [AssocTree (TokenId,IdKind) Id] -> (TokenId,IdKind) -> Maybe Id
lookupAll [] key = Nothing
lookupAll (t:ts) key =
  case lookupAT t key of
    Nothing -> lookupAll ts key
    just -> just


{-
Looks up id in rename table for identifier given through its kind and token.
If no entry exists, new id is created but appropriate error message added to
rename state.
-}
uniqueTid :: Pos -> IdKind -> TokenId -> RenameMonad Id
uniqueTid pos kind tid down 
          renameState@(RenameState flags unique rps rts rt st derived defaults 
                                   errors needCheck) =
  let key =  (sQual down tid,kind)
  in case lookupAll (rt:rts) key of 
       Just u -> (u,renameState)
       Nothing -> (unique, RenameState flags (unique+1) rps rts
			     (addAT rt sndOf key unique)
			     st derived defaults
			     (("Unbound " ++ show kind ++ " " ++ show tid ++ 
                               " at " ++ strPos pos ++ "\n\n" ++ 
                               show (rt:rts)) : errors)
			     needCheck)


fixTid :: IdKind -> TokenId -> RenameMonad (InfixClass TokenId,Id)
fixTid kind tid down 
       renameState@(RenameState flags unique rps rts rt st derived defaults 
                                errors needCheck) =
  let key =  (sQual down tid,kind)  --- !!! check if real name !!!
  in case lookupAll (rt:rts) key of
       Just u ->
	 case lookupAT st u of
	   Nothing ->
               (sFix down (ensureM (snd rps) (fst key)),renameState) -- hack
               -- old code    ((InfixL,9::Int),renameState) 
               -- It's an argument, and I have lost the fixity information :-(
	   Just info ->  (fixityI info,renameState)
       Nothing   ->
               (sFix down (ensureM (snd rps) (fst key)),renameState) -- hack
               -- old code    ((InfixL,9::Int),renameState)
	

{-
Adds the the given identifier (position, kind of id, token with name)
to the active names in renameState.
Adds error to renameState, if identifier is already an active name,
that is, redefinition occurred.
-}
bindTid :: Pos -> IdKind -> TokenId -> a -> RenameState -> RenameState
bindTid pos kind tid _ 
        renameState@(RenameState flags unique irps@(_,rps) rts rt st derived 
                                 defaults errors needCheck) =
  let key =  (tid,kind)
      redefinedGlobal [rt] = lookupAT rt (forceM rps tid,kind)
      redefinedGlobal _ = Nothing
  in case lookupAT rt key of 
       Just u -> 
         if sRedefine flags 
           then renameState
	   else
	     (RenameState flags unique irps rts rt st derived defaults
		(("Redefinition of " ++ show kind ++ " " ++ show tid 
                  ++ " at " ++ strPos pos) : errors) 
                needCheck)
       Nothing   ->
	 case redefinedGlobal rts of
	   Nothing ->
	     (RenameState flags (unique+1) irps rts
			  (addAT rt sndOf key unique)
			  st derived defaults errors needCheck)
	   Just u -> 
             (if sRedefine flags 
                then id 
                else
		  strace ("Warning: " ++ show tid 
                          ++ " is both imported and defined")) $
 		  (RenameState flags unique irps rts
			       (addAT rt sndOf key u) -- catch same scope redef
			       (updateAT st u clearI) derived defaults
			       errors needCheck)


bindNK :: Pos -> RenameMonad TokenId
bindNK pos _ renameState@(RenameState flags unique irps@(_,rps) rts rt st
                                      derived defaults errors needCheck) =
  let tid = visible (show unique)
      key = (tid,Var)
  in if sNplusK flags then
       case lookupAll (rt:rts) key of
         Nothing-> (tid
                   ,RenameState flags (unique+1) irps rts
				(addAT rt sndOf key unique)
                                st
				-- (addAT st (\a b->b) unique
				--     (InfoUsed unique [(Var,tid,rps,pos)]))
				-- (addAT st (\a b->b) unique
				--     (InfoName unique tid 0 tid))
				-- (addAT st (\a b->b) unique
				--     (InfoVar unique tid IEall (InfixDef,9)
				--              (NewType [] [] [] [NTany 0])
				--              (Just 0)))
                                derived defaults errors needCheck)
         Just u -> (tid
                   ,RenameState flags (unique+1) irps rts
				(addAT rt sndOf key unique)
				st derived defaults
                                (("Binding (n+k) pattern to new unique "++
                                  "identifier at "++strPos pos): errors)
				needCheck)
     else (tid
          ,RenameState flags (unique+1) irps rts
		       (addAT rt sndOf key unique)
		       st derived defaults
                       (("(n+k) patterns are disabled - pattern at "++
                         strPos pos): errors)
		       needCheck)

checkPuns pos down renameState@(RenameState flags unique irps rts rt st
                                   derived defaults errors needCheck) =
  if sPuns flags then
    renameState
  else
    RenameState flags unique irps rts rt st derived defaults
                (("Named field puns are not Haskell'98 - used at "++
                  strPos pos):errors)
                needCheck


{-
Checks if given identifier (kind, token) is already known as active name.
It is used to check if a field have already been included in the bindings
-}
checkTid :: Pos -> IdKind -> TokenId -> RenameRMonad a Bool
checkTid pos kind tid _ renameState@(RenameState flags unique rps rts rt st
                                        derived defaults errors needCheck) =
  let key =  (tid,kind)
  in  case lookupAT rt key of 
        Just u -> (True,renameState)
        Nothing -> (False,renameState)

---- =================

{-
This function makes use of the ability of NewType to contain several types.
-}
transTypes :: [(TokenId,Int)] 
           -> [Id] 
           -> [Context TokenId] 
           -> [Type TokenId]
           -> RenameMonad NewType

transTypes al free ctxs ts =
  unitS (NewType free []) =>>> 
  mapS (transContext al) ctxs =>>> 
  mapS (transType al) ts

transTVar pos al v =
  unitS mkNTvar =>>> uniqueTVar pos al v	-- no KIND inference

uniqueTVar pos al v =
  case lookup v al of
    Just v -> unitS v
    Nothing -> renameError ("Unbound type variable " ++ show v ++ " at " 
                            ++ strPos pos) 0

transContext al (Context pos cid [(vpos,vid)]) = 
  unitS pair =>>> uniqueTid pos TClass cid =>>> uniqueTVar vpos al vid


transType :: [(TokenId,Int)] -> Type TokenId -> RenameMonad NT

transType al (TypeApp  t1 t2) = 
  unitS NTapp =>>> transType al t1 =>>> transType al t2
transType al (TypeCons  pos hs types) = 
  unitS mkNTcons =>>> uniqueTid pos TCon hs =>>> mapS (transType al) types
transType al (TypeVar   pos v) = transTVar pos al v
transType al (TypeStrict pos t) = unitS NTstrict =>>> transType al t

----- ==================================

{-
Adds list of default types to RenameState.
Checks for illegal types and redefinition, 
extending error messages appropriately.
-}
defineDefault :: [Type Id] -> a -> RenameState -> RenameState
defineDefault types down (RenameState flags unique rps rts rt st
                                      derived Nothing errors needCheck) =
    case partition (\nt-> case nt of TypeCons _ _ [] -> True
                                     TypeApp _ _ -> True
                                     _ -> False) 
                   types of
      (cs,[]) -> 
        RenameState flags unique rps rts rt st derived 
          (Just (map getCon types)) errors needCheck
      (_,es) ->  
        RenameState flags unique rps rts rt st derived Nothing
	  (("Illegal type in default at " ++ strPos (getPos es)):errors) 
          needCheck
    where
    getCon (TypeCons _ con _) = con
    getCon (TypeApp tf ta) = getCon tf	-- not really sure about this.
defineDefault types down (RenameState flags unique rps rts rt st
                                      derived defaults errors needCheck) =
    RenameState flags unique rps rts rt st derived defaults 
      (("Redefinition of defaults at " ++ strPos (getPos types)) :errors) 
      needCheck

{-
Add a type synonym to symboltable. (It must be already in renaming table.)
-}
defineType :: TokenId      {- type synonym -}
           -> NewType      {- the type it is defined to denote -}
           -> RenameMonad Id  -- id of the type synonym

defineType tid nt down (RenameState flags unique irps@(_,rps) rts rt st
                                    derived defaults errors needCheck) =
  let realtid = ensureM rps tid
      key = (tid,TSyn)
  in case lookupAT rt key of
       Just u -> (u, RenameState flags unique irps rts rt
		         (addAT st combInfo u {-(realtid,TSyn)-} 
                            (InfoData u realtid (sExp down tid TSyn) nt 
                               (DataTypeSynonym False 0)))
		         derived defaults errors (u:needCheck))


{- 
Add a class to symboltable. 
(It must be already in renaming table.)
Also checks for duplicate predicates in context (=> extend error messages)
-}
defineClass :: Pos
            -> TokenId 
            -> NewType  {- pseudo type built from class and type variable
                           (type of dictionary?) -}
            -> [(Id,Id)] {- (type info for method, default info for method) -}
            -> RenameToken
            -> RenameState 
            -> RenameState

defineClass pos tid nt mds down (RenameState flags unique irps@(_,rps)
                                             rts rt st derived defaults errors 
                                             needCheck) =
  let realtid = ensureM rps tid
      key = (tid,TClass)
      (ms,ds) = unzip mds
  in case lookupAT rt key of
       Just u ->
        let newst = addAT st combInfo u {-(realtid,TClass)-} 
                     (InfoClass u realtid (sExp down tid TSyn) nt ms ds initAT)
        in case checkNT pos (strAT st) nt of
          Nothing -> 
            RenameState flags unique irps rts rt newst derived defaults 
              errors needCheck
          Just err -> 
             RenameState flags unique irps rts rt newst derived defaults 
               (err:errors) needCheck


defineDataPrim :: TokenId -> NewType -> Int -> RenameMonad Id 
defineDataPrim tid nt size down (RenameState flags unique irps@(_,rps)
                                             rts rt st derived defaults errors 
                                             needCheck) =
  let realtid = ensureM rps tid
      key = (tid,TCon)
  in case lookupAT rt key of
       Just u -> (u,RenameState flags unique irps rts rt 
		      (addAT st combInfo u {-(realtid,TCon)-} 
                         (InfoData u realtid (sExp down tid TCon) nt 
                            (DataPrimitive size)))
		      derived defaults errors needCheck
                 )

{-
-- Add entry for data or newtype declaration to symboltable.
-}
defineData :: Maybe Bool {- Nothing: newtype, Just False: data unboxed,
                            Just True: data (boxed) -}
           -> TokenId    {- type constructor -}
           -> NewType    {- defined type (coded with type variables) -}
           -> [Id]       {- data constructors -} 
           -> RenameMonad Id

defineData d tid nt cs down (RenameState flags unique irps@(_,rps) rts rt st
                                     derived defaults errors needCheck)  =
  let realtid = ensureM rps tid
      key = (tid,TCon)
  in case lookupAT rt key of
       Just u ->
         let (needCheck',dk,patch) =
		case d of
		  Just unboxed -> (needCheck,   Data unboxed cs,      id)
		  Nothing -> (u:needCheck, DataNewType False cs, patchIE)
                              -- unboxed fixed by keepRS
	 in (u,RenameState flags unique irps rts rt 
		 (addAT st combInfo u {-(realtid,TCon)-} 
                    (InfoData u realtid (patch (sExp down tid TCon)) nt dk))
		 derived defaults errors needCheck')


{-
-- Add entry for type declaration of given method to symboltable.
-- Return identifier for this entry.
-}
defineMethod :: Pos	{- position of type declaration -}
             -> TokenId	{- method id -} 
             -> NewType	{- method type -}
             -> Int	{- method arity -} 
             -> Id	{- class to which method belongs -} 
             -> TokenId	{- class name -} 
             -> RenameMonad Id

defineMethod pos tid nt arity classId ctid down
             (RenameState flags unique irps@(_,rps) rts rt st derived defaults 
                          errors needCheck) =
  let realtid = ensureM rps tid
      key = (tid,Method)
      rex = case sExp down ctid TClass of
                IEall -> IEsel
                IEabs -> IEnone
                _     -> sExp down tid Method
  in case lookupAT rt key of
       Just u ->
         let newst = addAT st combInfo u {-(realtid,Method)-} 
                       (InfoMethod u realtid rex (sFix down realtid) nt 
                         (Just arity) classId)
         in case checkMNT nt of
              Nothing ->
                (u,RenameState flags unique irps rts rt newst derived defaults
                     errors needCheck)
              Just err ->
                (u,RenameState flags unique irps rts rt newst derived defaults
                     (err:errors) needCheck)
  where
  checkMNT nt@(NewType free@(cv:_) [] ctxs nts) =
    case filter ((cv==) . snd) ctxs of
      [] -> checkNT pos (strAT st) nt
      [x] -> Just ("Illegal restriction " ++ strAT st (fst x) ++ 
                   " for type variable in type signature at " ++ strPos pos)
      xs -> Just ("Illegal restriction " ++ 
                  mixCommaAnd (map (strAT st . fst) xs) ++ 
                  " for type variable in type signature at " ++ strPos pos)



defineConstr typtid tid nt fields bt down
             (RenameState flags unique irps@(_,rps) rts rt st derived
                          defaults errors needCheck) = 
  let realtid = ensureM rps tid
      key = (tid,Con)
      rex = case sExp down typtid TCon of
                IEall -> IEsel
                IEabs -> IEnone
                _     -> sExp down tid Con
  in case lookupAT rt key of
       Just u -> (u,RenameState flags unique irps rts rt
			 (addAT st combInfo u {-(realtid,Con)-}
                             (InfoConstr u realtid rex (sFix down realtid)
                                         nt fields bt))
			 derived defaults errors needCheck)

defineField typtid bt c ((Nothing,_),_) down up = (Nothing,up)
defineField typtid bt c ((Just (p,tid,_),_),i) down
                        up@(RenameState flags unique irps@(_,rps) rts rt st
                                    derived defaults errors needCheck) = 
  let realtid = ensureM rps tid
      key = (tid,Field)
  in
    case lookupAT rt key of
      Just u ->
        case lookupAT st u of
	  Just (InfoField u' realtid' ie cis' bt' iSel') ->
	    if bt == bt'
	    then (Nothing,RenameState flags unique irps rts rt
			     (addAT st fstOf u' {-(realtid,Field)-}
                                 (InfoField u' realtid' ie
                                     ((c,i):cis') bt' iSel'))
			     derived defaults errors needCheck)
	    else (Nothing,RenameState flags unique irps rts rt st
			      derived defaults
                              (("Field " ++ show tid ++ " at " ++ strPos p ++
                                " is already defined"):errors)
                              needCheck)
	  Just u -> (Nothing,up)
	  Nothing ->
	    case lookupAT rt (tid,Var) of
	      Just selu ->
		( Just (p,u,selu)
		, RenameState flags unique irps rts rt
                              (addAT (addAT st combInfo u {-(realtid,Field)-}
                                            (InfoField u realtid IEnone
						       -- Var gives true IEinfo
                                                       [(c,i)] bt selu))
				     combInfo selu
                                     (InfoVar selu realtid
                                          (case sExp down typtid TCon of
					      IEall -> IEsel
					   -- IEsome -> sExp down tid Field
					   -- IEabs -> sExp down tid Var
                                              _     -> sExp down tid Var)
                                          (sFix down realtid)
                                          NoType (Just 1)))
                              derived defaults errors needCheck)


{- creates token for instance methods for tuple type? -} 
localTid :: PackedString -> Int -> TokenId -> TokenId
localTid rps u tid = mkQual3 (Visible rps) (t_Tuple u) tid


{- if token is not qualified make it qualified with given module name -}
globalTid :: PackedString -> Id -> TokenId -> TokenId
globalTid rps u tid = ensureM rps tid


defineVar :: TokenId -> RenameMonad Id
defineVar tid  down (RenameState flags unique irps@(_,rps) rts rt st
                                 derived defaults errors needCheck) = 
  let key = (tid,Var)
  in case lookupAT rt key of
       Just u ->
	 let realtid = sLG down rps u tid
	 in  (u,RenameState flags unique irps rts rt
			(addAT st combInfo u {-(realtid,Var)-}
			    (InfoVar u realtid (sExp down tid Var)
                                (sFix down (ensureM rps tid)) NoType Nothing))
			derived defaults errors needCheck)


defineDefaultMethod :: TokenId -> RenameMonad Id
defineDefaultMethod tid  down (RenameState flags unique irps@(_,rps)
                                           rts rt st derived defaults
                                           errors needCheck) = 
  let realtid = mkQualD rps tid
      skey = (tid,Method)
  in case lookupAT rt skey of
	Nothing -> error ("***defineDefaultMethod(1) " ++
                           show skey ++ "\n" ++ show rt)
        Just u ->
         case lookupAT st u of 
	   Nothing -> error ("***defineDefaultMethod(1) " ++
                              show skey ++ " " ++ show u ++ "\n" ++ show rt)
           Just (InfoMethod _ _ _ fix nt annot iClass) ->
	     (unique,RenameState flags (unique+1) irps rts rt
			 (addAT st combInfo  unique
                                {-(realtid,MethodDefault)-}
                                (InfoDMethod unique realtid nt annot iClass))
			 derived defaults errors needCheck)
           Just _ -> (unique
                     ,RenameState flags (unique+1) irps rts rt st derived
                       defaults 
                       (("Default method declared outside class: " ++ show tid)
                       :errors) needCheck)


defineInstMethod :: TokenId -> RenameMonad Id
defineInstMethod tid  down (RenameState flags unique irps@(_,rps)
                                        rts rt st derived defaults
                                        errors needCheck) = 
  let realtid = mkQual2 (t_Tuple unique) (ensureM rps tid)
                -- this is obscure! why a tuple with the size of unique?
  in (unique,RenameState flags (unique+1) irps rts rt
		 (addAT st combInfo  unique {-(realtid,MethodInstance)-}
                        (InfoIMethod unique realtid NoType Nothing (0::Int)))
		 derived defaults errors needCheck)


defineDerived :: Int -> [(Pos,Int)] -> a -> RenameState -> RenameState
defineDerived con posis down (RenameState flags unique rps rts rt st
                                   derived defaults errors needCheck) = 
  RenameState flags unique rps rts rt st ((con,posis):derived)
              defaults errors needCheck

strAT st i = (show . tidI . dropJust . lookupAT st) i

