module NeedLib(initNeed,needit,NeedLib,pushNeed,popNeed,bindTid,needTid) where
--		,TokenId,Kind,Memo(..),Tree) where

import Memo
import TokenId(TokenId,t_error,tTrue)
import TokenInt(tokenAllways,tokenMain)
import Kind
import AssocTree
import Tree234
import Extra
import List (delete)

-- Added in H98: the overlap table, which allows for later resolution of
-- shared module aliases.

data NeedLib = NeedLib (TokenId -> [TokenId])		-- qualified renaming
                       (Memo (TokenId,Kind))		-- tids already seen
                       [Memo (TokenId,Kind)]		-- stack of memos
                       (AssocTree (TokenId,Kind) (Bool,TokenId,[TokenId])) -- overlap table
                       (AssocTree (TokenId,Kind) [Pos])	-- final need-table

initNeed b = treeMap (`pair`[]) (initNeed' b)
  where initNeed' True  = foldr fun (initNeed' False) tokenMain
	initNeed' False = foldr fun initM tokenAllways
        fun (k,t) m = addM m (t,k)

needit n r iNeed =
  case n (NeedLib r initM [] initAT iNeed) of
    (NeedLib r m [] o n) -> (n,o)

pushNeed (NeedLib r m ms o n) = NeedLib r m (m:ms) o n
popNeed  (NeedLib r _ (m:ms) o n) = NeedLib r m ms o n

--bindTid kind tid (NeedLib r m ms o n) = NeedLib r (addM m (r tid,kind)) ms o n

bindTid :: Kind -> TokenId -> NeedLib -> NeedLib
bindTid kind tid (NeedLib r m ms o n) =
    NeedLib r (foldr memoise m (r tid)) ms o n
  where
    memoise :: TokenId -> Memo (TokenId,Kind) -> Memo (TokenId,Kind)
    memoise tid m = addM m (tid,kind)

--needTid pos kind tid needlib@(NeedLib r m ms o n) =
--  case r tid of
--    [tid] ->
--      case lookupM m (tid,kind) of
--	Just _ -> needlib
--	Nothing ->
--	  case lookupAT n (tid,kind) of -- mostly to evaluate n now and then :-)
--	    Just _ ->  NeedLib r (addM m (tid,kind)) ms o (updateAT n (tid,kind) (pos:))
--	    Nothing -> NeedLib r (addM m (tid,kind)) ms o (addAT n undefined (tid,kind) [pos])
----  tids -> 
----    case lookupM m (tids,kind) of
----	Just _ -> needlib
----	Nothing ->
----	  case lookupAT n (tids,kind) of -- mostly to evaluate n now and then :-)
----	    Just _ ->  NeedLib r (addM m (tids,kind)) ms (updateAT n (tids,kind) (pos:))
----	    Nothing -> NeedLib r (addM m (tids,kind)) ms (addAT n undefined (tids,kind) [pos])

needTid pos kind tid needlib@(NeedLib r m ms o n) =
  case r tid of
    []    -> error ("qualified renaming of "++show tid++" produced no results!")
    [tid] -> record tid needlib
    tids  -> foldr record (NeedLib r m ms (addOverlap tid kind o tids) n) tids
 where
  record tid needlib@(NeedLib r m ms o n) =
    case lookupM m (tid,kind) of 
      (Just _) -> needlib
      Nothing ->
        case lookupAT n (tid,kind) of -- mostly to evaluate n now and then :-)
          Just _ ->  NeedLib r (addM m (tid,kind)) ms o (updateAT n (tid,kind) (pos:))
          Nothing -> NeedLib r (addM m (tid,kind)) ms o (addAT n undefined (tid,kind) [pos])

addOverlap atid kind o tids =
    foldr add o tids
  where add t o = addAT o sndOf (t,kind) (True, atid, delete t tids)

    

