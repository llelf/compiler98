module Overlap
  ( deAlias
  ) where

-- Added in H98: the overlap table, which allows for later resolution of
-- shared module aliases.		Malcolm.Wallace@cs.york.ac.uk

import TokenId(TokenId,t_error,tTrue)
import Kind
import AssocTree
import Tree234
import Extra

--                                       unresolved yet?
--                                            source alias
--                                                    target aliases
type Overlap = AssocTree (TokenId,Kind) (Bool,TokenId,[TokenId])


deAlias :: (TokenId->[TokenId])
             -> Overlap
             -> AssocTree (TokenId,Kind) (Either [Pos] [Int])
             -> ([String], (TokenId->TokenId))
deAlias qf o rt =
    (foldr findUndef err flatrt, newqf)
  where
    (err,o') = resolveOverlaps o flatrt
    flatrt   = treeMapList (:) rt

    findUndef (key, Left poss) err = checkNonUnique key poss err
    findUndef (key, Right _)   err = err

    checkNonUnique key@(_,kind) poss err =
        case lookupAT o' key of
                                 -- No overlap, remains undefined.
          Nothing -> mkErrorND key poss: err
                                 -- Overlap, but resolves to a different alias.
          (Just (False,alias,others)) -> err
                                 -- Overlap, still unresolved.
          (Just (True,alias,others)) -> mkErrorOVND key poss alias others: err

    newqf t =
        case lookupAT (foldr buildqf initAT (treeMapList (:) o')) t of
            Nothing   -> head (qf t)
            (Just t') -> t'

    buildqf ((tid,_), (True,alias,_))  t = addAT t undefined alias tid
    buildqf ((tid,_), (False,_,_))     t = t 


resolveOverlaps :: Overlap -> [((TokenId,Kind), Either [Pos] [Int])]
              -> ([String],Overlap)
resolveOverlaps o rt =
    foldl resolve ([],o) rt
  where
    resolve   o     (key, Left poss) = o
    resolve   o     (key, Right [x]) = mkUnique o key
    resolve (err,o) (key, Right (x:xs)) =
        if all (x==) xs then    --- Tuples are entered twice
            mkUnique (err,o) key
        else (mkErrorMD key (x:xs): err, o)

    mkUnique (err,o) key@(_,kind) =
        case lookupAT o key of
              -- No overlap, nothing to do
          Nothing -> (err, o)
              -- Overlaps, already resolved to a different alias.
          (Just (False,alias,others)) -> (mkErrorOV key alias others: err, o)
              -- Overlaps, either still unresolved, or resolves to this alias.
          (Just (True,alias,others)) -> (err, foldr (undef kind) o others)

    undef kind tid o = updateAT o (tid,kind) (\(_,a,as)->(False,a,as))

------
mkErrorMD (tid,kind) xs =
  show kind ++ ' ':show tid ++ " defined " ++ show (length xs) ++ " times."

mkErrorOV (tid,kind) alias others =
  show kind ++ ' ':show alias ++ " resolves to two or more of "
  ++ showList (tid:others) "."

mkErrorND (tid,Method) poss =
  "The identifier " ++ show tid ++ " instantiated at "
  ++ mix "," (map strPos poss) ++ " does not belong to this class."
mkErrorND (tid,kind) poss =
  show kind ++ ' ':show tid ++ " used at " ++ mix "," (map strPos poss)
  ++ " is not defined."

mkErrorOVND (tid,kind) poss alias others =
  show kind ++ ' ':show alias ++ " used at " ++ mix "," (map strPos poss)
  ++ " cannot be resolved:\n  none of " ++ showList (tid:others) " is defined."

------
