module HatTrace (
 HatTrace,HatExpression,HatExprType,HatApplType,
 openTrace,edtChildren,observe,hatMain,hatParent,hatResult,hatApplFun,
 hatApplArity,hatApplArgs,hatExpressionStr,hatApplType,hatExprType,
 isApplication,
 showReductionList,showReduction,showExpression
)
where

import FFI
import GreenCard
import Maybe

type HatTrace = ForeignObj
type NodeNumber = Int
type HatExpression = (HatTrace,NodeNumber)

data HatExprType = HatApplication | HatSAT_A | HatSAT_B | HatSAT_C | HatHidden | 
		   HatProjection | HatName | HatConstructor | HatIdentifier |
		   HatUnknown deriving Show

data HatApplType = HatApplied | HatBlackholed | HatComplete deriving Show

%C #include "Expressions.h"
%C #include "hatfileops.h"
%C #include "nodelist.h"
%C #include "hashtable.h"
%C #include "FunTable.h"
%C #include "observe.h"
%C #include "detect.h"

%dis addr x = declare "void*" x in (%%Addr x)

%fun lookupAddr :: Addr -> Int
%call (addr a)
%code
%result (int "*(int*)a")

%fun lookupArray :: ForeignObj -> Int -> Int
%call (foreign f r) (int i)
%code
%result (int "((int*) r)[i]")

tomightbe :: HatExpression -> Maybe HatExpression
tomightbe expr@(_,nodenumber) =
    if (nodenumber == 0) then Nothing else Just expr

frommightbe :: Maybe HatExpression -> HatExpression
-- frommightbe Nothing = (0,0)
frommightbe (Just v) = v

%dis maybehatExpression x y = <frommightbe / tomightbe > (int x, int y)


%fun openTrace :: String -> IO HatTrace
%call (string name)
%code hatfileHandle=(void*) openfile(name);
%result (foreign "closefile" hatfileHandle)


%fun _hatMain :: HatTrace -> NodeNumber
%call (foreign f hattrace)
%code switchToHandle((int) hattrace); 
%     nodenumber = mainCAF();
%result (int nodenumber)
-- %result (maybehatExpression "hattrace" nodenumber)

hatMain :: HatTrace -> Maybe HatExpression
hatMain hattrace = tomightbe (hattrace,_hatMain hattrace)

%fun _hatParent :: HatExpression -> NodeNumber
%call (foreign f hattrace, int nodenumber)
%code switchToHandle((int) hattrace);
%     followSATs(nodenumber);
%     newnode = getTrace();
%result (int newnode)
-- %result (maybehatExpression "hattrace" newnode)

hatParent :: HatExpression -> Maybe HatExpression
hatParent expr@(hattrace,nodenumber) = tomightbe (hattrace,_hatParent expr)

%fun _hatResult :: HatExpression -> NodeNumber
%call (foreign f hattrace,int nodenumber)
%code filepointer fp;
%     switchToHandle((int) hattrace); 
%     fp = followSATs(nodenumber);
%     newnode = getNodeType();
%     if ((newnode!=TRAPP)&&(newnode!=TRNAM)) newnode=0; else {
%       newnode = findAppSAT(fp);
%     }
%result (int newnode)
-- %result (maybehatExpression "hattrace" newnode)

hatResult :: HatExpression -> Maybe HatExpression
hatResult expr@(hattrace,nodenumber) = tomightbe (hattrace,_hatResult expr)

%fun _hatApplFun :: HatExpression -> NodeNumber
%call (foreign f hattrace,int nodenumber)
%code filepointer fp;
%     switchToHandle((int) hattrace); 
%     fp = followSATs(nodenumber);
%     newnode = getNodeType();
%     if (newnode==TRAPP) newnode=getFunTrace();
%     else
%      if (newnode==TRNAM) newnode=getNmType();
%      else  newnode=0;
%result (int newnode)
-- %result (maybehatExpression "hattrace" newnode)

hatApplFun :: HatExpression -> Maybe HatExpression
hatApplFun expr@(hattrace,nodenumber) = tomightbe (hattrace,_hatApplFun expr)

%fun hatApplArity :: HatExpression -> Int
%call  (foreign f hattrace,int nodenumber)
%code switchToHandle((int) hattrace); 
%     followSATs(nodenumber);
%     newnode = getNodeType();
%     if (newnode==TRAPP) newnode=getAppArity();
%     else
%        newnode = 0;
%result (int newnode)


%fun gethatArgInternal :: HatExpression -> Int -> Int
%call  (foreign f hattrace,int nodenumber) (int argnum)
%code switchToHandle((int) hattrace); 
%     followSATs(nodenumber);
%     newnode = getNodeType();
%     if (newnode!=TRAPP) newnode=0; else {
%       newnode = getAppArgument(argnum);  
%     }
%result (int newnode)
-- %result (int "hattrace", int newnode)



hatApplArgs :: HatExpression -> [HatExpression]
hatApplArgs expr@(hattrace,h) = gethatArgs 0 (hatApplArity expr)
 where gethatArgs _ 0 = []
       gethatArgs i j = (hattrace,(gethatArgInternal expr i)):(gethatArgs (i+1) (j-1))


%fun hatExpressionStr :: HatExpression -> Int -> Int -> String
%call (foreign g hattrace,int nodenumber) (int verbose) (int precision)
%code ExprNode* exp;
%     switchToHandle((int) hattrace); 
%     exp = buildExpr(nodenumber,verbose,precision);
%     prettystring = prettyPrintExpr(exp,1);
%     freeExpr(exp);
%result (string prettystring)


toHatExprType :: Int -> HatExprType
toHatExprType 0 = HatApplication
toHatExprType 1 = HatApplication
toHatExprType 2 = HatProjection
toHatExprType 3 = HatHidden
toHatExprType 4 = HatSAT_A
toHatExprType 5 = HatSAT_B
toHatExprType 6 = HatSAT_C
toHatExprType 12 = HatSAT_A
toHatExprType 13 = HatSAT_B
toHatExprType 14 = HatSAT_C
toHatExprType 71 = HatConstructor
toHatExprType 70 = HatIdentifier
toHatExprType _  = HatUnknown

fromHatExprType HatApplication = 0


%fun hatExprType :: HatExpression -> HatExprType
%call (foreign f hattrace,int nodenumber)
%code switchToHandle((int) hattrace); 
%     followSATs(nodenumber);
%     nodetype = getNodeType();
%result (<fromHatExprType/toHatExprType> (int nodetype))


toHatApplType 4 = Just HatApplied
toHatApplType 5 = Just HatBlackholed
toHatApplType 6 = Just HatComplete
toHatApplType _ = Nothing

fromHatApplType _ = 0


%fun hatApplType :: HatExpression -> Maybe HatApplType
%call (foreign f hattrace,int nodenumber)
%code filepointer appl;
%     switchToHandle((int) hattrace); 
%     appl = followSATs(nodenumber);
%     nodetype = getNodeType();      
%     if ((nodetype!=TRAPP)&&(nodetype!=TRNAM)) nodetype=0; else {
%       findAppSAT(appl);
%       nodetype = getNodeType();						 
%     }
%result (<fromHatApplType/toHatApplType> (int nodetype))

foreignObjToList :: HatTrace -> (Int,ForeignObj) -> [HatExpression]
foreignObjToList hattrace dat =
    foreignObjToList' 0 dat
    where foreignObjToList' i (count,ptr) =
	    if (i>=count) then [] else
		   (hattrace,(lookupArray ptr i)):(foreignObjToList' (i+1) (count,ptr))


%fun getEDTForeignObj :: HatExpression -> (Int,ForeignObj)
%call (foreign f hattrace,int nodenumber)
%code switchToHandle((int) hattrace); 
%     count = getEDTchildren(nodenumber,(int**) &resultArray);
%result (int count,foreign "freeArray" resultArray)

edtChildren :: HatExpression -> [HatExpression]
edtChildren (hattrace,node) = foreignObjToList hattrace (getEDTForeignObj (hattrace,node))

%fun getObserve :: HatTrace -> String -> String -> Int -> Int -> Int -> (Int,ForeignObj)
%call (foreign f hattrace) (string ident) (string topIdent) (int verbose) (int recursive) (int prec)
%code switchToHandle((int) hattrace); 
%     count = getObserve(ident,topIdent,verbose,1,recursive,prec,(int**) &resultArray);
%result (int count,foreign "freeArray" resultArray)

observe :: HatTrace -> String -> String -> Int -> Int -> Int -> [HatExpression]
observe hattrace ident topIdent verbose recursive precise =
  foreignObjToList hattrace 
		       (getObserve hattrace ident topIdent verbose recursive precise)

showExpression :: Maybe HatExpression -> IO ()
showExpression hatExpression =
    if (isNothing(hatExpression)) then putStr "Nothing" else
      putStr (hatExpressionStr (fromJust hatExpression) 1 100)

isApplication HatApplication = True
isApplication _ = False

showReduction :: Maybe HatExpression -> IO ()
showReduction maybehatExpression =
  showExpression maybehatExpression >>
  if (isJust maybehatExpression) then
    let res = (hatResult (fromJust maybehatExpression)) in
    if (isJust res) then
       putStr " = " >>
       (showExpression res) >>
       putStrLn ""
     else putStrLn ""
  else
   return ()

showReductionList :: [HatExpression] -> IO ()
showReductionList list = foldl (>>) (return ()) (map (showReduction . Just) list)




