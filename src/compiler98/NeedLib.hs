module NeedLib(initNeed,needit,NeedLib,pushNeed,popNeed,bindTid,needTid) where
--		,TokenId,IdKind,Memo(..),Tree) where

import Memo
import TokenId(TokenId,t_error,tTrue)
import TokenInt(tokenAllways,tokenMain)
import IdKind
import AssocTree
import Tree234
import Extra
import Overlap (Overlap(..),addOverlap)

-- Added in H98: the overlap table, which allows for later resolution of
-- shared module aliases.

data NeedLib = NeedLib (TokenId -> [TokenId])		-- qualified renaming
                       (Memo (TokenId,IdKind))		-- tids already seen
                       [Memo (TokenId,IdKind)]		-- stack of memos
                    -- (AssocTree (TokenId,IdKind) (Bool,TokenId,[TokenId])) -- overlap table
                       Overlap			-- overlaps for later resolution
                       (AssocTree (TokenId,IdKind) [Pos])	-- final need-table

initNeed b = treeMap (`pair`[]) (initNeed' b)
  where initNeed' True  = foldr fun (initNeed' False) tokenMain
	initNeed' False = foldr fun initM tokenAllways
        fun (k,t) m = addM m (t,k)

needit n r iNeed =
  case n (NeedLib r initM [] initAT iNeed) of
    (NeedLib r m [] o n) -> (n,o)

pushNeed (NeedLib r m ms o n) = NeedLib r m (m:ms) o n
popNeed  (NeedLib r _ (m:ms) o n) = NeedLib r m ms o n

--bindTid idKind tid (NeedLib r m ms o n) = NeedLib r (addM m (r tid,idKind)) ms o n

bindTid :: IdKind -> TokenId -> NeedLib -> NeedLib
bindTid idKind tid (NeedLib r m ms o n) =
    NeedLib r (foldr memoise m (r tid)) ms o n
  where
    memoise :: TokenId -> Memo (TokenId,IdKind) -> Memo (TokenId,IdKind)
    memoise tid m = addM m (tid,idKind)

--needTid pos idKind tid needlib@(NeedLib r m ms o n) =
--  case r tid of
--    [tid] ->
--      case lookupM m (tid,idKind) of
--	Just _ -> needlib
--	Nothing ->
--	  case lookupAT n (tid,idKind) of -- mostly to evaluate n now and then :-)
--	    Just _ ->  NeedLib r (addM m (tid,idKind)) ms o (updateAT n (tid,idKind) (pos:))
--	    Nothing -> NeedLib r (addM m (tid,idKind)) ms o (addAT n undefined (tid,idKind) [pos])
----  tids -> 
----    case lookupM m (tids,idKind) of
----	Just _ -> needlib
----	Nothing ->
----	  case lookupAT n (tids,idKind) of -- mostly to evaluate n now and then :-)
----	    Just _ ->  NeedLib r (addM m (tids,idKind)) ms (updateAT n (tids,idKind) (pos:))
----	    Nothing -> NeedLib r (addM m (tids,idKind)) ms (addAT n undefined (tids,idKind) [pos])

needTid pos idKind tid needlib@(NeedLib r m ms o n) =
  case r tid of
    []    -> error ("qualified renaming of "++show tid++" produced no results!")
    [tid] -> record tid needlib
    tids  -> foldr record (NeedLib r m ms (addOverlap tid idKind o tids) n) tids
 where
  record tid needlib@(NeedLib r m ms o n) =
    case lookupM m (tid,idKind) of 
      (Just _) -> needlib
      Nothing ->
        case lookupAT n (tid,idKind) of -- mostly to evaluate n now and then :-)
          Just _ ->  NeedLib r (addM m (tid,idKind)) ms o (updateAT n (tid,idKind) (pos:))
          Nothing -> NeedLib r (addM m (tid,idKind)) ms o (addAT n undefined (tid,idKind) [pos])

