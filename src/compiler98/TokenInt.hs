module TokenInt where

import IdKind
import TokenId
import DbgId
import AssocTree
import Info
import Extra(sndOf)
import Tree234(treeMapList)

getInts tidk2i =
  case length (treeMapList (:) assocTree) of  -- force evaluation of tree
    0 -> error "What??? (in TokenInt)\n"
    _ -> (getIntsUnsafe assocTree,lookupAT assocTree)
 where
   assocTree = foldr fix initAT 
		(tokenList ++ tokenInteger ++ tokenRational ++ tokenAllways ++ tokenMain
		++ tokenMonad ++ tokenBounded ++ tokenEnum
		++ tokenEq ++ tokenEval ++ tokenIx
		++ tokenOrd ++ tokenRead ++ tokenShow ++ tokenDbg
                ++ tokenBinary ++ tokenNplusK)		--MALCOLM modified

   fix (k,tid) t =
      case tidk2i (tid,k) of
         Just u -> addAT t sndOf (tid,k) u
	 Nothing -> t

   getIntsUnsafe t  k = 
     case lookupAT t k of
       Nothing -> error ("Can't find int for " ++ show k ++"\n")
       Just i -> i


tokenMain =     [(TCon,tIO),(TCon,t_Tuple 0)]
tokenList =	[(TClass,tNum),(Var,tnegate)]
tokenInteger  = [(Var,tfromInteger)]
tokenRational =	[(Var,tfromRational),(TSyn,tRational),(TCon,tRatio),(TClass,tFractional)]
tokenAllways = 	[(Var,t_undef)
                ,(TCon,tBool),(Con,tTrue),(Con,tFalse)
	        ,(TCon,tInt),(TCon,tInteger),(TCon,tFloat),(TCon,tDouble)
	        ,(TCon,tChar),(TCon,tString)
	        ,(TCon,t_List),(Con,t_Colon),(Con,t_List)
	        ,(TCon,t_Arrow),(Con,t_Tuple 2)
	        ,(Var,t_eqInteger),(Var,t_eqFloat),(Var,t_eqDouble)
		,(Var,t_otherwise)
	        ,(Var,terror),(Var,tident)
                ,(Var,t_apply1),(Var,t_apply2),(Var,t_apply3),(Var,t_apply4)]
tokenMonad =	[(Var,t_gtgt),(Var,t_gtgteq),(Var,t_zero)]
tokenBounded =	[(TClass,tBounded),(Var,tminBound),(Var,tmaxBound)]
tokenEnum =	[(TClass,tEnum)
		,(Var,ttoEnum),(Var,tfromEnum),(Var,tenumFrom)
                ,(Var,tenumFromThen)
		,(Var,t_toEnum),(Var,t_fromEnum),(Var,t_enumFromTo)
                ,(Var,t_enumFromThenTo)]
tokenEq =	[(TClass,tEq),(Var,t_fromEnum),(Var,t_equalequal)
                ,(Var,t_andand)]
tokenEval =	[(Var,tseq)]  -- seq is now standalone, without class Eval
tokenIx =	[(TClass,tIx)
		,(Var,trange),(Var,tindex),(Var,tinRange)
		,(Var,t_tupleRange),(Var,t_tupleIndex),(Var,t_andand)
		,(Var,t_enumRange),(Var,t_enumIndex),(Var,t_enumInRange)]
tokenOrd =	[(TClass,tOrd),(Var,t_fromEnum),(Var,t_equalequal)
                ,(Var,t_lessthan),(Var,t_lessequal)
		,(Var,t_andand),(Var,t_pipepipe),(Var,tcompare)
                ,(Con,tLT),(Con,tEQ),(Con,tGT)]
tokenRead =	[(TClass,tRead)
		,(Var,treadsPrec),(Var,treadParen),(Var,t_append)
                ,(Var,t_greater)
		,(Var,t_readCon0),(Var,t_readConInfix),(Var,t_readCon)
                ,(Var,t_readConArg),(Var,t_readField),(Var,t_readFinal)]
tokenShow =	[(TClass,tShow)
	      	,(Var,tshowsType),(Var,tshowsPrec),(Var,tshowParen)
                ,(Var,tshowString),(Var,tshowChar),(Var,t_lessthan)
		,(Var,t_dot)]
{- MALCOLM additions: -}
tokenBinary    = [(TClass,tBinary)
		,(Var,t_put),(Var,t_get),(Var,t_getF),(Var,t_sizeOf)
		,(Var,t_putBits),(Var,t_getBits),(Var,t_getBitsF)
                ,(Var,t_gtgt),(Var,t_gtgteq),(Var,t_return)
                ,(Var,t_ltlt),(Var,t_plus)]
tokenNplusK = 	[(Var,t_lessequal),(Var,t_subtract)]

