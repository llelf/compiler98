{-
Besides other things it implements the MAGIC of some function definitions.
Occurrences of these functions (in specific contexts) are turned into
a respective bytecode. 
-}
module PrimCode(primCode{-,rpsEval-},rpsseq) where

import Extra(pair,Pos(..),noPos,strPos,strace)
import Syntax
import State
import IntState
import TokenId
import PosCode
import SyntaxPos
import SysDeps(PackedString,packString,unpackPS)
import IdKind
import Id(Id)

------- (true if bool == Int, true if && || not is primitives,true if )

primCode :: (Bool,Bool,Bool) -- bool, logic, always
         -> Bool -- magic: create byte code instructions for some functions
         -> ((TokenId,IdKind) -> Int) 
         -> IntState 
         -> [(a,PosLambda)] 
         -> ([(a,PosLambda)],IntState)

primCode flags magic tidFun state code = 
  case mapS primBindingTop code (flags,magic,True,tidFun (tident,Var)) 
         (state,[]) of
    (bs,(state,_)) -> (concat bs,state)
  

primBindingTop :: (a,PosLambda) 
               -> ((Bool,Bool,Bool),Bool,b,Id) 
               -> (IntState,[(a,PosLambda)]) 
               -> ([(a,PosLambda)],(IntState,[c]))

primBindingTop (fun,lambda) =
    primStrict True >=>
    primLambda lambda >>>= \ lambda ->
    primTop >>>= \ bs ->
    unitS ((fun,lambda):bs)

primBinding (fun,lambda) =
  primLambda lambda >>>= \ lambda ->
    unitS (fun,lambda)

primBindings bindings =
  primBindings' [] (reverse bindings)
 where
  primBindings' acc [] = unitS (acc)
  primBindings' acc (b:bs) =
    primBinding b >>>= \ (b) ->
    primBindings' (b:acc) bs

primLambda (PosLambda pos free args@(_:_) exp) =
  primStrict True >=>  -- will be lifted later
  primExp exp >>>= \ (exp) ->
  unitS (PosLambda pos free args exp)
primLambda (PosLambda pos free args exp) =
  primExp exp >>>= \ (exp) ->
  unitS (PosLambda pos free args exp)
primLambda l@(PosPrimitive pos fun) =
  unitS l
primLambda l@(PosForeign pos fun t c ie) =
  unitS l

primExp (PosExpLambda pos envs args exp) = 
  primStrict True >=>  -- will be lifted later
  primExp exp >>>= \ exp ->
  unitS (PosExpLambda pos envs args exp)
primExp (PosExpLet pos bindings exp) =
  primExp exp >>>= \ exp ->
  (primStrict False >=> primBindings bindings) >>>= \ (bindings) ->
  unitS (PosExpLet pos bindings exp)
primExp (PosExpCase pos exp alts) =
  primStrict True >=> -- If a case is lazy then lift it
  mapS primAlt alts >>>= \ alts ->
  primExp exp >>>= \ exp ->
  unitS (PosExpCase pos exp alts)
primExp (PosExpFatBar b exp1 exp2) =
  primExp exp2 >>>= \ exp2 ->
  primExp exp1 >>>= \ exp1 ->
  unitS (PosExpFatBar b exp1 exp2)
primExp (PosExpFail) =
  unitS (PosExpFail)
primExp (PosExpIf  pos exp1 exp2 exp3) =
  primStrict True >=> -- If an contitional is lazy then lift it
  primExp exp2 >>>= \ exp2 ->
  primExp exp3 >>>= \ exp3 ->
  primExp exp1 >>>= \ exp1 ->
  unitS (PosExpIf pos exp1 exp2 exp3)
primExp (PosExpApp apos (PosVar pos fun:es)) =
--  (primStrict False >=> mapS primExp es) >>>= \ es ->
  primExpand pos fun es
primExp (PosExpApp pos (e:es)) =
  primExp e >>>= \ e ->
  (primStrict False >=> mapS primExp es) >>>= \ es ->
  unitS (PosExpApp pos (e:es))
primExp (PosVar pos fun) =
  primExpand pos fun []
primExp e =
  unitS e

primAlt (PosAltCon pos con args exp) = 
  primExp exp >>>= \ (exp) ->
  unitS (PosAltCon pos con args exp)
primAlt (PosAltInt pos int      exp) =
  primExp exp >>>= \ (exp) ->
  unitS (PosAltInt pos int exp)

---

strictPrim SEQ = True : repeat False
strictPrim _ = repeat True


primPrimitive pos prim arity es =
  mapS ( \ (s,e) -> primStrict s >=> primExp e) (zip (strictPrim prim) es) >>>= \ es ->
  let need = arity - (length es)
  in
    if need <= 0 then
      case splitAt arity es of
	(args,eargs) -> unitS (posExpApp pos (PosExpThunk pos (PosPrim pos prim:args) : eargs))
    else
      mapS ( \ _ -> primUnique ) (take need (repeat '_')) >>>= \ newargs ->
      unitS (PosExpLambda pos [] (map (pair pos) newargs) (PosExpThunk pos (PosPrim pos prim : es ++ map (PosVar pos) newargs)))
   

primApp pos fun es =
 (primStrict False >=> mapS primExp es) >>>= \ es ->
 unitS (posExpApp pos (PosVar pos fun:es))

-- All args are already processed

primExpand pos fun es =
  primFlags >>>= \ ((bool,logic,always),magic,strict) ->
  primTidArity fun >>>= \ (arity,tid) ->
  if not magic || (arity < 0 || not (strict || always)) then 
    -- this cannot be a primitive, or we don't translate unless strict
    primApp pos fun es
  else
    case tid of
      (Qualified3 (Qualified modcls cls) (Qualified modtyp typ) (Visible met)) 
		| modcls == rpsPrelude && modtyp == rpsPrelude ->
        if cls == rpsEq then
	  case (primOp bool typ,eqPrim met) of
	    (Just op,Just prim) -> primPrimitive pos (prim op) arity es
            _ -> primApp pos fun es
        else if cls == rpsOrd then
	  case (primOp bool typ,ordPrim met) of
	    (Just op,Just prim) -> primPrimitive pos (prim op) arity es
            _ -> primApp pos fun es
        else if cls == rpsNum then
	  case (primOp bool typ,numPrim met) of
	    (Just op,Just prim) -> primPrimitive pos (prim op) arity es
            _ -> primApp pos fun es
	else if cls == rpsIntegral then
	  case (primOp bool typ,integralPrim met) of
	    (Just op,Just prim) -> primPrimitive pos prim arity es
	    _ -> primApp pos fun es
        else if cls == rpsEnum then
	  if typ == rpsChar &&
	     (met == rpstoEnum || met == rpsfromEnum) then
	    case es of
	      (f:[]) -> unitS f
	      [] -> primIdent pos
          else 
	    primApp pos fun es
        else if cls == rpsFloating then
	  case (primOp bool typ,floatingPrim met) of
	    (Just op,Just prim) -> primPrimitive pos (prim op) arity es
            _ -> primApp pos fun es
        else if cls == rpsFractional then
	  case (primOp bool typ,fractionalPrim met) of
	    (Just op,Just prim) -> primPrimitive pos (prim op) arity es
            _ -> primApp pos fun es
     -- else if cls == rpsEval then
     --   case (evalPrim met) of
     --     (Just prim) -> primPrimitive pos prim 2 es
     --     _ -> primApp pos fun es
        else 
          primApp pos fun es

      (Qualified3 (Visible modcls) underscore (Visible met)) 
          | modcls == rpsPrelude && underscore == t_underscore && met == rpsseq ->
        primPrimitive pos SEQ 2 (dropDicts es)

  --  (Qualified3 (Qualified modcls cls) (Qualified modtyp typ) (Visible met)) 
  --      | modcls == rpsPrelude && cls == rpsEval && met == rpsseq ->
  --    primPrimitive pos SEQ 2 (dropDicts es)
          
      (Qualified mod met) | mod == rpsPrelude ->
             if met == rps_eqFloat then
	  primPrimitive pos (CMP_EQ OpFloat) 2 es
        else if met == rps_eqDouble then
	  primPrimitive pos (CMP_EQ OpDouble) 2 es
        else if met == rps_hGetStr then
	  primPrimitive pos HGETS 1 es
        else if met == rps_hGetChar then
	  primPrimitive pos HGETC 1 es
        else if met == rps_hPutChar then
	  primPrimitive pos HPUTC 2 es
        else if met == rps_fromEnum then
	  primPrimitive pos ORD 1 es
        else if met == rps_toEnum then
	  primPrimitive pos CHR 1 es
        else if met == rpsseq then
	  primPrimitive pos SEQ 2 (dropDicts es)
        else if logic then 
               if met == rpsAndAnd then
  	    primPrimitive pos AND 2 es
          else if met == rpsOrOr then
	    primPrimitive pos OR 2 es
          else if met == rpsnot then
	    primPrimitive pos NOT 1 es
          else
            primApp pos fun es
        else
          primApp pos fun es

      _ -> primApp pos fun es


-----------------

primTop down up@(state,bs) =
    (bs,(state,[]))

primUnique down up@(state,bs) =
  case uniqueIS state of
    (u,state) -> (u,(state,bs))

primIdent pos down@(flags,magic,strict,ident) up =
  (PosVar pos ident,up)

primFlags down@(flags,magic,strict,ident) up =
  ((flags,magic,strict),up)

primStrict s down@(flags,magic,strict,ident) up =
  ((flags,magic,s,ident),up)

primTidArity i down up@(state,bs) =
  case lookupIS state i of
    Just info -> ((arityIS state i,tidI info),up)	-- count ctx
    Nothing -> ((-1,error "arg"),up) -- It's an argument, don't look :-)

-- =============================================================

impRev str = packString (reverse str)

--------------

rpsEq  = impRev "Eq"
rpsOrd = impRev "Ord"
rpsNum = impRev "Num"
rpsFloating   = impRev "Floating"
rpsIntegral   = impRev "Integral"
rpsFractional = impRev "Fractional"
rpsEnum = impRev "Enum"
--rpsEval = impRev "Eval"		-- Removed in Haskell 98

rps_eqFloat = impRev "_eqFloat"
rps_eqDouble = impRev "_eqDouble"

rpsAndAnd = impRev "&&"
rpsOrOr = impRev "||"
rpsnot = impRev "not"
rps_fromEnum = impRev "_fromEnum"
rps_toEnum = impRev "_toEnum"
rps_hGetStr  = impRev "_hGetStr"
rps_hGetChar = impRev "_hGetChar"
rps_hPutChar = impRev "_hPutChar"

--------------

eqPrim met =
       if met == rpseq then Just CMP_EQ
  else if met == rpsne then Just CMP_NE
  else Nothing

rpseq = impRev "=="
rpsne = impRev "/="

--------------

ordPrim met =
       if met == rpslt then Just CMP_LT
  else if met == rpsle then Just CMP_LE
  else if met == rpsgt then Just CMP_GT
  else if met == rpsge then Just CMP_GE
  else Nothing

rpslt = impRev "<"
rpsle = impRev "<="
rpsgt = impRev ">"
rpsge = impRev ">="

--------------------

primOp bool typ =
       if typ == rpsInt    then Just OpWord
  else if typ == rpsChar   then Just OpWord
  else if bool && typ == rpsBool then Just OpWord
  else if typ == rpsDouble then Just OpDouble
  else if typ == rpsFloat  then Just OpFloat
  else Nothing

rpsInt    = impRev "Int"
rpsChar   = impRev "Char"
rpsBool   = impRev "Bool"
rpsDouble = impRev "Double"
rpsFloat  = impRev "Float"

-------------------

enumPrim met =
       if met == rpstoEnum   then Just CHR
  else if met == rpsfromEnum then Just ORD
  else Nothing

rpstoEnum   = impRev "toEnum"
rpsfromEnum = impRev "fromEnum"

--------------------

numPrim :: PackedString -> Maybe (PrimOp -> Prim)
numPrim met =
       if met == rpssignum then Just SIGNUM
  else if met == rpsabs    then Just ABS
  else if met == rpsnegate then Just NEG
  else if met == rpsadd    then Just ADD
  else if met == rpssub    then Just SUB
  else if met == rpsmul    then Just MUL
  else Nothing

rpsadd    = impRev "+"
rpssub    = impRev "-"
rpsmul    = impRev "*"
rpsabs    = impRev "abs"
rpssignum = impRev "signum"
rpsnegate = impRev "negate"

--------------

integralPrim :: PackedString -> Maybe Prim
integralPrim met =
       if met == rpsquot then Just QUOT
  else if met == rpsrem  then Just REM
  else Nothing 

rpsquot = impRev "quot"
rpsrem  = impRev "rem"

--------------


floatingPrim :: PackedString -> Maybe (PrimOp -> Prim)
floatingPrim met =
       if met == rpsexp  then Just EXP
  else if met == rpslog  then Just LOG
  else if met == rpssqrt then Just SQRT
  else if met == rpssin  then Just SIN
  else if met == rpscos  then Just COS
  else if met == rpstan  then Just TAN
  else if met == rpsasin then Just ASIN
  else if met == rpsacos then Just ACOS
  else if met == rpsatan then Just ATAN
  else Nothing

rpsexp = impRev "exp"
rpslog = impRev "log"
rpssqrt = impRev "sqrt"
rpssin = impRev "sin"
rpscos = impRev "cos"
rpstan = impRev "tan"
rpsasin = impRev "asin"
rpsacos = impRev "acos"
rpsatan = impRev "atan"

--------------

fractionalPrim :: PackedString -> Maybe (PrimOp -> Prim)
fractionalPrim met =
       if met == rpsslash then Just SLASH
  else Nothing

rpsslash = impRev "/"

--------------

evalPrim :: PackedString -> Maybe Prim
evalPrim met =
       if met == rpsseq then Just SEQ
  else Nothing

rpsseq = impRev "_seq"

---- ======================================================

dropDicts (PosExpDict _:es) = dropDicts es
dropDicts es = es
