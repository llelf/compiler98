{- ---------------------------------------------------------------------------
Internal state of the compiler used from need analysis until renaming
-}
module ImportState (module ImportState, module Info, Decl) where

import SysDeps(PackedString,packString)
import Syntax(Decl)
import AssocTree
import Memo
import Info

data  ImportState =
      ImportState
        Bool			 -- visible
	Int			 -- unique
	PackedString		 -- modid of interface file
	PackedString		 -- modid of this section of the interface file
	(Memo TokenId)           -- needI
	(AssocTree (TokenId,IdKind) (Either [Pos] [Int]))	
        -- ^ rename (name -> unique)
	(AssocTree (TokenId,IdKind) Info)			
        -- ^ symboltable (real name -> info)
	[(TokenId,TokenId,[Int],[(Pos,TokenId,Int)])]	
        -- ^ [ (realClass, realData, free , Ctxs) ]
	(TokenId -> (InfixClass TokenId,Int))		
        -- fixity information (name -> fixity)
	[String]		 -- errors
#if !defined(__HASKELL98__)
    deriving (Eval)
#endif


{- initial import state -}
initIS :: AssocTree (TokenId,IdKind) [Pos] -> ImportState
initIS rt = 
  ImportState False 1 (packString "???") (packString "???") initM
    (mapAT Left rt) initAT [] (error "no fixity") []


{- put modid of interface file into import state -}
putModidIS :: ImportState -> PackedString -> ImportState
putModidIS (ImportState visible unique _ _ needI rt st insts fixity errors)
           rps =
  ImportState True unique rps rps needI {-initM-} rt st insts fixity errors


{- put modid of current section of interface file into import state -}
putModid2IS :: ImportState -> Bool -> PackedString -> ImportState
putModid2IS (ImportState _ unique orps _ needI rt st insts fixity errors) 
            visible rps =
  ImportState visible unique orps rps needI rt st insts fixity errors


getNeedIS
  (ImportState visible unique orps rps needI rt st insts fixity errors) = 
  (orps,rps,needI)

getSymbolTableIS 
  (ImportState visible unique orps rps needI rt st insts fixity errors) = st

getRenameTableIS 
  (ImportState visible unique orps rps needI rt st insts fixity errors) = rt

getErrIS is@(ImportState vis unique orps rps needI rt st insts fixity errors)
  | null errors = Right is
  | otherwise   = Left errors

importError :: String -> Int -> () -> ImportState -> (Int,ImportState)
importError err r _  
    (ImportState visible unique orps rps needI rt st insts fixity errors) =
  (r
  ,ImportState visible unique orps rps needI rt st insts fixity (err:errors))
