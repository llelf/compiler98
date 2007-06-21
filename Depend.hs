module Depend(depend) where

import AssocTree(listAT)
import Flags (Flags,sDepend,sPrelude,sSourceFile,sUnix)
import IntState
import Util.Extra
import Util.OsOnly
import TokenId

-- Only the beginning, can probably do mych better

depend flags state rt =
  if sDepend flags then
    let
      isUnix = sUnix flags
      sourcefile = sSourceFile flags
      (rootdir,filename) = fixRootDir isUnix sourcefile
    in writeFile (fixDependFile isUnix rootdir filename)
                 ((unlines
                  . filter (not . null)
                  . map (strId (sPrelude flags) state . head . dropRight . snd)
                  . listAT) rt)
  else
    return ()


strId prelude state i =
  case lookupIS state i of
    Nothing -> ""
    Just info ->
      let tid = tidI info
      in
        if (prelude || notPrelude tid) then
          case tidI info of
            TupleId 0 -> "()"
            TupleId n -> '(':take (n-1) (repeat ',') ++ ")"
            tid -> show tid
        else
	  ""

