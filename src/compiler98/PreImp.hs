{- ---------------------------------------------------------------------------
Define selectors for tuples with elements of complicated type
-}
module PreImp where

import ImportState(ImportState)
import TokenId(TokenId)
import Syntax(Simple,Type,Context,Constr)

type HideDeclType =    ImportState -> (Int,Bool) -> Simple TokenId 
                    -> Type TokenId -> ImportState
type HideDeclData =    ImportState -> Either Bool Bool -> [Context TokenId] 
                    -> Simple TokenId -> [Constr TokenId] -> [(Int,TokenId)] 
                    -> ImportState
type HideDeclDataPrim = ImportState -> (Int,TokenId) -> Int -> ImportState
type HideDeclClass =   ImportState -> [Context TokenId] -> (Int,TokenId) 
                     -> (Int,TokenId) 
                     -> [([((Int,TokenId),Maybe Int)]
                         ,[Context TokenId]
                         ,Type TokenId
                         )] 
                     -> ImportState
type HideDeclInstance =    ImportState -> [Context TokenId] -> (Int,TokenId) 
                        -> Type TokenId -> ImportState
type HideDeclVarsType = ImportState -> [((Int,TokenId),Maybe Int)] 
                        -> [Context TokenId] -> Type TokenId -> ImportState
type HideDeclIds = (HideDeclType,HideDeclData,HideDeclDataPrim
                   ,HideDeclClass,HideDeclInstance,HideDeclVarsType)


-- Keep this selectors in sync with the tuple built by mkNeed in PreImport  

hType :: HideDeclIds -> HideDeclType
hType     (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclType

hData :: HideDeclIds -> HideDeclData
hData     (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclData

hDataPrim :: HideDeclIds -> HideDeclDataPrim
hDataPrim (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclDataPrim

hClass :: HideDeclIds -> HideDeclClass
hClass    (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclClass

hInstance :: HideDeclIds -> HideDeclInstance
hInstance (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclInstance

hVarsType :: HideDeclIds -> HideDeclVarsType
hVarsType (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclVarsType


-- Keep this selectors in sync with the tuple built by is2rs in RenameLib

sLG (localGlobal,qualFun,expFun,fixFun) = localGlobal
sQual (localGlobal,qualFun,expFun,fixFun) = qualFun
sExp (localGlobal,qualFun,expFun,fixFun) = expFun
sFix (localGlobal,qualFun,expFun,fixity) = fixity

