module DeriveBinary (deriveBinary) where

import List
import Syntax
import MkSyntax(mkInt)
import IntState
import IdKind
import NT
import State
import DeriveLib
import TokenId(TokenId,tTrue,t_Tuple,t_Colon,t_List,
               tBinary,t_get,t_put,t_getF,t_sizeOf,
               t_putBits,t_getBits,t_getBitsF,
               t_gtgt,t_gtgteq,t_return,t_ltlt,t_plus,dropM)
import Nice(showsOp,showsVar)

deriveBinary tidFun cls typ tvs ctxs pos =
 getUnique >>>= \d ->
 let expD = ExpVar pos d
     iPut  = tidFun (t_put,Method)
     iGet  = tidFun (t_get,Method)
     iFGet = tidFun (t_getF,Method)
     iSize = tidFun (t_sizeOf,Method)

     expPut  = ExpVar pos iPut
     expGet  = ExpVar pos iGet
     expFGet = ExpVar pos iFGet
     expSize = ExpVar pos iSize

     expTrue   = ExpCon pos (tidFun (tTrue,Con))
     expPair   = ExpCon pos (tidFun (t_Tuple 2,Con))
     expCons   = ExpCon pos (tidFun (t_Colon,Con))
     expNil    = ExpCon pos (tidFun (t_List,Con))
     expPutBits = ExpVar pos (tidFun (t_putBits,Var))
     expGetBits = ExpVar pos (tidFun (t_getBits,Var))
     expGetBitsF  = ExpVar pos (tidFun (t_getBitsF,Var))
     expLtLt   = ExpVar pos (tidFun (t_ltlt,Var))
     expGtGt   = ExpVar pos (tidFun (t_gtgt,Var))
     expGtGtEq = ExpVar pos (tidFun (t_gtgteq,Var))
     expReturn = ExpVar pos (tidFun (t_return,Var))
     expPlus   = ExpVar pos (tidFun (t_plus,Var))
 in
  getInfo typ >>>= \ typInfo -> 
  mapS getInfo (constrsI typInfo) >>>= \ constrInfos ->
  let sizeC = ((ceiling . logBase 2 . fromIntegral . length) constrInfos)::Int
  in
    addInstMethod tBinary (tidI typInfo) t_put (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iPut >>>= \ funP ->
    addInstMethod tBinary (tidI typInfo) t_get (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iGet >>>= \ funG ->
    addInstMethod tBinary (tidI typInfo) t_getF (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iFGet >>>= \ funF ->
    addInstMethod tBinary (tidI typInfo) t_sizeOf (NewType tvs [] ctxs [NTcons typ (map NTvar tvs)]) iSize >>>= \ funS ->
    mapS (mkPutFun expTrue expPutBits expPut expGtGt expGtGtEq expReturn sizeC pos) (zip [0..] constrInfos) >>>= \ funPs ->
    mkGetFuns expTrue expGetBits expGet expGtGtEq expReturn expCons expNil sizeC pos typInfo constrInfos >>>= \ funGs ->
    mkFGetFuns expTrue expGetBitsF expFGet expLtLt expPair expCons expNil sizeC pos typInfo constrInfos >>>= \ funFs ->
    mapS (mkSizeFun sizeC expTrue expSize expPlus pos) constrInfos >>>= \ funSs ->
    unitS $
      DeclInstance pos (syntaxCtxs pos ctxs) cls (syntaxType pos typ tvs) $
        DeclsParse [DeclFun pos funP funPs
		   ,DeclFun pos funG funGs
		   ,DeclFun pos funF funFs 
		   ,DeclFun pos funS funSs]



mkPutFun expTrue expPutBits expPut expGtGt expGtGtEq expReturn sizeC pos (numC,constrInfo) =
  getUnique >>>= \bh->
  let 
    --conTid = dropM (tidI constrInfo)
      con = ExpCon pos (uniqueI constrInfo)
      expBH = ExpVar pos bh
      expPutCon = ExpApplication pos [expPutBits, expBH, mkInt pos sizeC, mkInt pos numC]
  in case ntI constrInfo of
     NewType _ _ _ [nt] -> -- This constructor has no arguments
       unitS (Fun [expBH,con] [(expTrue,expPutCon)] (DeclsParse []))
     NewType _ _ _ (_:nts) ->  -- We only want a list with one element for each argument, the elements themselves are never used
       mapS ( \ _ -> unitS (ExpVar pos) =>>> getUnique) nts >>>= \(fstarg:args) ->
       getUnique >>>= \ h ->
       let expH = ExpVar pos h
           expPutArg arg = ExpApplication pos [expPut,expBH,arg]
       in unitS (
          Fun [expBH,ExpApplication pos (con:fstarg:args)]
 	      [(expTrue, ExpApplication pos
                             [expGtGtEq, expPutCon,
                              ExpLambda pos [expH]
                                  (ExpApplication pos [expGtGt,
                                                       (foldl (\z arg -> ExpApplication pos [expGtGt,z,expPutArg arg])
                                                              (expPutArg fstarg)
                                                              args),
                                                       ExpApplication pos [expReturn,expH]])])]
              (DeclsParse []))


-- this code is modified from *showType*, not from *readsPrec*.
mkGetFuns expTrue expGetBits expGet expGtGtEq expReturn expCons expNil sizeC pos typInfo constrInfos =
  getUnique >>>= \ i ->
  getUnique >>>= \ bh ->
  let expI      = ExpVar pos i
      expBH     = ExpVar pos bh
      expGetCon = ExpApplication pos [expGetBits, expBH, mkInt pos sizeC]
  in
   --mkListExp pos expCons expNil expGtGtEq expGet expBH expReturn constrInfos >>>= \listExp->
     mkAltList pos expTrue (mkGetExp pos expGtGtEq expGet expBH expReturn) constrInfos >>>= \altList->
     unitS [Fun [expBH]
	        [(expTrue, ExpApplication pos
                             [expGtGtEq,
                              expGetCon,
                              ExpLambda pos [expI]
                                 (ExpCase pos expI altList)
                             ])]
	        (DeclsParse [])]

mkGetExp pos expGtGtEq expGet expBH expReturn expCon args constrInfo =
  foldr (\ arg z -> ExpApplication pos [expGtGtEq, (ExpApplication pos [expGet,expBH]), ExpLambda pos [arg] z])
        (ExpApplication pos [expReturn,
                             ExpApplication pos (expCon:args)])
        args


mkAltList pos expTrue mkExpFun constrInfos =
  mapS (\(n,constrInfo) ->
         let expCon = ExpCon pos (uniqueI constrInfo)
             expN   = mkInt pos n
         in
         case ntI constrInfo of
           NewType _ _ _ (_:nts) ->
             mapS ( \ _ -> unitS (ExpVar pos) =>>> getUnique) nts >>>= \args ->
             unitS (Alt expN
                        [(expTrue, mkExpFun expCon args constrInfo)]
                        (DeclsParse []))
       )
       (zip [0..] constrInfos)



-- this code is modified from *showType*, not from *readsPrec*.
mkFGetFuns expTrue expGetBitsF expFGet expLtLt expPair expCons expNil sizeC pos typInfo constrInfos =
  getUnique >>>= \ bh ->
  getUnique >>>= \ p ->
  getUnique >>>= \ p' ->
  getUnique >>>= \ n ->
  let expBH = ExpVar pos bh
      expP  = ExpVar pos p
      expP' = ExpVar pos p'
      expN  = ExpVar pos n
      expInit   = ExpApplication pos [expGetBitsF, expBH, mkInt pos sizeC, expP]
      expFGetBH = ExpApplication pos [expFGet, expBH]
  in
     mkAltList pos expTrue (mkGetFExp pos expLtLt expFGetBH expPair expP') constrInfos >>>= \altList->
     unitS [Fun [expBH,expP]
	        [(expTrue, ExpLet pos (DeclsParse [DeclPat (Alt (ExpApplication pos [expPair,expN,expP'])
                                                                [(expTrue,expInit)]
                                                                (DeclsParse []))])
                                      (ExpCase pos expN altList)
                )]
	        (DeclsParse [])]

mkGetFExp pos expLtLt expFGetBH expPair expP' expCon args constrInfo =
    foldl (\ acc arg -> ExpApplication pos [expLtLt, acc, expFGetBH])
          (ExpApplication pos [expPair,expCon,expP'])
          args


mkSizeFun sizeC expTrue expSize expPlus pos constrInfo =
  let
      con           = ExpCon pos (uniqueI constrInfo)
      expCsize      = mkInt pos sizeC
      expSizeOf arg = ExpApplication pos [expSize,arg]
  in case ntI constrInfo of
     NewType _ _ _ (_:nts) ->
       mapS ( \_ -> unitS (ExpVar pos) =>>> getUnique) nts >>>= \args ->
       unitS (
          Fun [ExpApplication pos (con:args)]
 	      [(expTrue, foldl (\z arg-> ExpApplication pos [expPlus,expSizeOf arg,z]) expCsize args)]
              (DeclsParse []))

