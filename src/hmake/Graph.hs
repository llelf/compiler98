module Graph(seteq, hascycleeq, mkseteq, anysameeq, allsameeq, assocdefeq,
             unioneq, intereq, diffeq, scceq, tsorteq) where
import Compat(mix)
import ListUtil(elemEq,lconcatMap, mapAccuml)
import List

#if defined(__HASKELL98__)
default (Int,Double)
#endif

-- This is actually .../lml/src/misc/util.m
tsorteq eq [] = []
tsorteq eq gG =
    case partition (\(_, x) -> null x) gG of
      ([], _) -> error ("tsorteq: cycle in data\n" ++
                        lconcatMap (\(f, fs) -> f ++ ": " ++ mix fs " " ++ "\n")
                                  gG)
      (a, b) -> let a' = map fst a
                in  a ++ tsorteq eq (map (\(x, xs) -> (x, diffeq eq xs a')) b)

hascycleeq eq [] = False
hascycleeq eq gG =
    case partition (\(_, x) -> null x) gG of
      ([], _) -> True
      (a, b) -> let a' = map fst a
                in  hascycleeq eq (map (\(x, xs) -> (x, diffeq eq xs a')) b)

diffeq eq l1 l2 = filter (\x -> not (elemEq eq x l2)) l1

intereq eq l1 l2 = filter (\x -> elemEq eq x l2) l1

unioneq eq l1 l2 = l1 ++ diffeq eq l2 l1

assocdefeq eq i [] d = d
assocdefeq eq i ((k, v) : r) d =
    if eq i k then v else assocdefeq eq i r d

scceq :: (a -> a -> Bool) -> [(a,[a])] -> [[(a,[a])]]
scceq eq gG =
    let
#if defined(__NHC__)
        searchc :: Int -> [(a,Int)] -> (a,[a]) -> ((Int,[(a,Int)],[(a,[a])],Int), [[(a,[a])]])
#endif
        searchc n low vv@(v, es) =
            let n' = n + 1
                low' = (v, n') : low
                ((n'', low'', nstack, min'), cs) =
                    let f (n'', low'', stack, min') w =
                            let ((n''', low''', stack', m), cs) =
                                    let vm = assocdefeq eq w low'' 0
                                    in  if vm == 0 then
                                            searchc n''
                                                    low''
                                                    (w,
                                                     assocdefeq eq
                                                                w
                                                                gG
                                                                (error "scc-assoc"))
                                        else
                                            ((n'', low'', [], vm), [])
                            in  ((n''',
                                  low''',
                                  stack' ++ stack,
                                  if m < min' then m else min'),
                                 cs)
                    in  mapAccuml f (n', low', [vv], n') es
                cs' = concat cs
            in  if assocdefeq eq v low'' (error "scc-assoc") == min' then
                    ((n'', map (\(x, _) -> (x, maxBound)) nstack ++ low'', [], min'),
                     cs' ++ [nstack])
                else
                    ((n'', low'', nstack, min'), cs')
        (low, cs) =
            let g low vv@(v, _) =
                    if assocdefeq eq v low 0 == 0 then
                        let ((n, low', stack, min'), cs) = searchc 1 low vv
                        in  (low', cs)
                    else
                        (low, [])
            in  mapAccuml g [] gG
    in  concat cs

allsameeq _ [] = True
allsameeq _ [a] = True
allsameeq eq (a : b) = all (eq a) b

anysameeq _ [] = False
anysameeq eq (a : b) = elemEq eq a b || anysameeq eq b

mkset' eq l [] = []
mkset' eq l (a : b) =
    if elemEq eq a l then mkset' eq l b else a : mkset' eq (a : l) b

mkseteq eq l = mkset' eq [] l

seteq eq x y = null (diffeq eq x y) && null (diffeq eq y x)
