module ImportState
	(module ImportState
	,module Info 
--	,PackedString
	,Memo(..),TokenId,AssocTree(..),Tree,Decl,InfixClass,Kind,NewType,Pos(..)) where

import PackedString(PackedString,packString,unpackPS)
import NT
import Kind
import Syntax(InfixClass,Decl)
import AssocTree
import Memo
import TokenId
import Extra(Pos(..))
import Info
import Tree234

data  ImportState =
      ImportState
        Bool						-- visible
	Int						-- unique
	PackedString					-- modid of interface file
	PackedString					-- modid of this section of the interface file
	(Memo (TokenId,Kind))				-- needI
	(AssocTree (TokenId,Kind) (Either [Pos] [Int]))	-- rename (name	-> unique)
	(AssocTree (TokenId,Kind) Info)			-- symboltable (real name -> info)
	[(TokenId,TokenId,[Int],[(Pos,TokenId,Int)])]	-- [ (realClass, realData, free , Ctxs) ]
	(TokenId -> (InfixClass TokenId,Int))		-- fixity information (name -> fixity)
	[String]					-- errors


initIS rt = ImportState False 1 (packString "???") (packString "???") initM (treeMap ( \ (k,v) -> (k,Left v)) rt) initAT [] (error "no fixity") []

putModidIS (ImportState visible unique _ _ needI rt st insts fixity errors) rps =
  ImportState True unique rps rps needI {-initM-} rt st insts fixity errors
putModid2IS (ImportState _ unique orps _ needI rt st insts fixity errors) visible rps =
  ImportState visible unique orps rps needI rt st insts fixity errors

getNeedIS (ImportState visible unique orps rps needI rt st insts fixity errors) = (orps,rps,needI)
getSymbolTableIS (ImportState visible unique orps rps needI rt st insts fixity errors) = st
getRenameTableIS (ImportState visible unique orps rps needI rt st insts fixity errors) = rt
getErrorsIS (ImportState visible unique orps rps needI rt st insts fixity errors) =
  (ImportState visible unique orps rps needI rt st insts fixity [],errors)
importError err r _  (ImportState visible unique orps rps needI rt st insts fixity errors) =
  (r, ImportState visible unique orps rps needI rt st insts fixity (err:errors))
