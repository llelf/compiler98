module PreImp where

-- Keep this selectors in sync with the tuple built by mkNeed in PreImport  

hType     (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclType
hData     (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclData
hDataPrim (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclDataPrim
hClass    (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclClass
hInstance (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclInstance
hVarsType (hideDeclType,hideDeclData,hideDeclDataPrim,hideDeclClass,hideDeclInstance,hideDeclVarsType) = hideDeclVarsType


-- Keep this selectors in sync with the tuple built by is2rs in RenameLib

sLG (localGlobal,qualFun,expFun,fixFun) = localGlobal
sQual (localGlobal,qualFun,expFun,fixFun) = qualFun
sExp (localGlobal,qualFun,expFun,fixFun) = expFun
sFix (localGlobal,qualFun,expFun,fixity) = fixity

