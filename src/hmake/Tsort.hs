module Tsort(ptsort) where
import Compat(difference)
import List

--
--	ptsort:		partial topological sort
--
--		tsort G	sorts the graph G.  A graph is a list of nodes, each
--		node is a pair, a name and a list of names of connected nodes.
--
--		The output is a list of lists, where the lists contain nodes
--		that come before nodes occuring in later lists.
--		The order of nodes within each list is arbitrary and not
--		determined by the graph.
ptsort [] = []
ptsort gG =
    case partition (\(_, x) -> null x) gG of
      ([], _) -> error "ptsort: cycle in data\n"
      (a, b) -> let a' = map fst a
                in  a' : ptsort (map (\(x, xs) -> (x, difference xs a')) b)

