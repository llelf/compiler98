module ReportImports where

import List(nub,groupBy,intersperse,sortBy)
import PackedString(unpackPS)
import TokenId (extractM,extractV)
import Info
import IntState
import Tree234

reportImports :: String -> IntState -> [String]
reportImports m =
  map ("import "++) .
  nub .
  map moduleName .
  filter ((m/=).moduleName) .
  concatMap (possibleTid.snd) .
  treeMapList (:) .
  getSymbolTable

reportFnImports :: String -> IntState -> [String]
reportFnImports m =
  map (\xs-> "import "++ fst (head xs) ++ "\t(" ++
             concat (intersperse ", " (map snd xs)) ++ ")") .
  groupBy (\(x,_) (y,_)-> x==y) .
  sortBy  (\(x,_) (y,_)-> compare x y) .
  map (\t-> (moduleName t, varName t)) .
  filter (("Prelude"/=).moduleName) .
  filter ((m/=).moduleName) .
  concatMap (possibleTid.snd) .
  treeMapList (:) .
  getSymbolTable

moduleName :: TokenId -> String
moduleName = reverse . unpackPS . extractM

varName :: TokenId -> String
varName = reverse . unpackPS . extractV

possibleTid (InfoClear)                                  = []
possibleTid (InfoUsed   unique uses)                     = []
possibleTid (InfoUsedClass unique uses insts)            = []
possibleTid (InfoData   unique tid exp nt dk)            = [tid]
possibleTid (InfoClass  unique tid exp nt ms ds insts)   = [tid]
possibleTid (InfoVar    unique tid fix exp nt annot)     = [tid]
possibleTid (InfoConstr unique tid fix nt fields iType)  = []
possibleTid (InfoField  unique tid icon_offs iData iSel) = []
possibleTid (InfoMethod unique tid fix nt annot iClass)  = []
possibleTid (InfoIMethod unique tid nt annot iMethod)    = []
possibleTid (InfoDMethod unique tid nt annot iClass)     = []
possibleTid (InfoInstance unique  nt iClass)             = []
possibleTid (InfoName _ tid _ _)			 = [tid]

