module STGBuild(stgExpPush,stgBodyPush) where

import Extra
import State
import IntState hiding (Kind)
import PosCode
import SyntaxPos
import Gcode
import GcodeLow(con0,cap0,caf,fun,extra,profconstructor)
import StrPos
import STGState
import DbgId(tNTId, tNTConstr, tSR3)
import Machine(wsize)

stgExpPush exp = unitS fst =>>> buildExp True exp
stgBodyPush exp = buildBody True exp

buildBody pu (fun,PosLambda pos _ _ exp) =
   buildExp pu exp >>>= \ (build,ptr) ->
   updTOS pu fun >>>
   unitS (build,(fun,ptr))

buildExp pu (PosExpLet pos bindings exp) =
  \ down
    (Thread prof fun maxDepth failstack state env lateenv depth heap depthstack)
    ->
     let (bBuild_bEnv,Thread prof' fun' maxDepth' failstack' state' env' _ depth' heap' depthstack')
            = mapS (buildBody False) bindings
                   down (Thread prof fun maxDepth failstack state newEnv (addLate:lateenv) depth heap depthstack)
                   
         (bBuild,addLate) = unzip bBuild_bEnv
         addId = map fst bindings
 	 addEnv = map ( \ v -> (v,HeapLate)) addId
         newEnv = addEnv:env
    in
--      strace ("STGGBuild PosExpLet addLate " ++ show (map fst addLate) ++ " addId " ++ show addId) $
      (buildExp pu exp >>>= \ (eBuild,ptr) ->
       popEnv >>>
       unitS (concat bBuild ++ eBuild,ptr)
      ) down (Thread prof' fun' maxDepth' failstack' state' newEnv (addLate:lateenv) depth' heap' depthstack')


buildExp pu (PosExpThunk _ (tag@(PosCon _ v):args)) = -- Should evaluate strict arguments
#ifdef DBGTRANS
  gState >>>= \state ->                               -- (already done ?) !!! 
  let vid = tidIS state v in
  if vid == tNTId || vid == tNTConstr then
      case args of
          [PosInt _ cid] -> oneHeap True pu (HEAP_GLB "D_" cid)
  else if vid == tSR3 then
      getExtra v >>>= \(e, extra) ->
      case args of
            [PosInt _ cid] ->
	      -- Every source refererence needs 3 words (plus any extra profiling words)
  	      oneHeap True pu (HEAP_GLB ("D_SR_" ++ show (cid-1)) 0)
  else
#endif
      mapS (buildExp False) args >>>= \ build_ptr ->  
      incDepthIf pu >>>= \ sp ->
      case unzip build_ptr of
        (build,ptr) ->
--         strace ("buildExp " ++ show pu ++ "  " ++ show ptr) $ 
           getExtra v >>>= \ (e,extra) ->
	   updHeap (1+e+length ptr) >>>= \ hp ->
--         strace ("buildExp " ++ show pu ++ "  " ++ show hp) $ 
           unitS (concat build ++ pushHeapIf True pu (HEAP_CON v : extra ++ (zipWith (heapPtr sp) [hp+1+e .. ] ptr))
	         ,Heap hp
		 )

buildExp pu (PosExpThunk _ (tag@(PosVar _ v):args)) =
  mapS (buildExp False) args >>>= \ build_ptr ->  
  incDepthIf pu >>>= \ sp ->
  case unzip build_ptr of
    (build,ptr) ->
      getExtra v >>>= \ (e,extra) ->
      gArity v >>>= \(Just arity) -> -- Always a global here!
      let nargs = length ptr
      in
        updHeap (1+e+nargs) >>>= \ hp ->
        unitS (concat build ++  (if nargs == arity 
                                 then pushHeapIf False pu (HEAP_VAP v:extra)
                                 else pushHeapIf True pu (HEAP_CAP v (arity-nargs):extra)
                                ) ++ zipWith (heapPtr sp) [hp+1+e .. ] ptr
        ,Heap hp
        )

buildExp pu (PosExpThunk pos [e]) =
  buildExp pu e
buildExp pu (PosCon pos i) =
    oneHeap True pu (HEAP_GLB con0 i)
--  gArity i >>>= \ a ->
--  if isJust a && dropJust a == 0 then   
--      oneHeap True pu (HEAP_GLB con0 i)
--  else
--      -- Can only happen with a constructor wrapped in NTBuiltin
--      oneHeap True pu (HEAP_GLB profconstructor i)

buildExp pu (PosVar pos i) =
  incDepthIf pu >>>= \ sp ->
  gWhereAbs i >>>= \ w ->
  case w of
    Nothing -> gArity i >>>= \ a ->
		   if isJust a && dropJust a == 0 then 
		     oneHeap False pu (HEAP_GLB caf i)
		   else
		     oneHeap True pu (HEAP_GLB cap0 i)
    Just (Arg i) ->  oneHeap False pu (HEAP_ARG i)
    Just (Stack i) ->  -- Could be improved if we knew if Stack i is evaluated !!!
      if pu then
        updHeap 1 >>>= \ hp -> unitS ([PUSH_HEAP,HEAP (sp-i)], Heap hp)
      else
        unitS ([],Stack i)
    Just (Heap i) -> -- Could be improved if we knew if Heap i is evaluated !!!
      if pu then
        updHeap 1 >>>= \ hp -> unitS ([PUSH_HEAP,HEAP_OFF (i-hp)], Heap hp)
      else
        unitS ([],Heap i)
    Just (HeapLate) -> lateWhere i >>>= \ lw ->
      if pu then
        updHeap 1 >>>= \ hp -> unitS ([PUSH_HEAP,case lw of
                                                   Stack i -> HEAP (sp-i)
                                                   Heap i -> HEAP_OFF (i-hp)
						   Direct g -> g],Heap hp)
      else
        unitS ([],lw)

buildExp pu (PosInt pos i) = oneHeap True pu (HEAP_INT i)
buildExp pu (PosChar pos i) = oneHeap True pu (HEAP_CHAR i)
buildExp pu (PosFloat pos f) = oneHeap True pu (HEAP_FLOAT f )
buildExp pu (PosDouble pos d) = oneHeap True pu (HEAP_DOUBLE d)
#ifdef DBGTRANS
buildExp pu (PosInteger pos i) = oneHeap True pu (HEAP_INT (fromInteger i))
#else
buildExp pu (PosInteger pos i) = oneHeap True pu (HEAP_INTEGER i)
#endif
buildExp pu (PosString pos s) = oneHeap False pu (HEAP_STRING s)
buildExp pu (PosExpCase pos exp alts) =
  error ("buildExp Case " ++ strPos pos)
buildExp pu (PosExpFatBar esc exp1 exp2) =
  error ("buildExp FatBar ")
buildExp pu (PosExpFail) =
  error ("buildExp Fail ")
buildExp pu (PosExpIf  pos exp1 exp2 exp3) =
  error ("buildExp If " ++ strPos pos)
buildExp pu (PosExpThunk pos [PosPrim _ STRING,PosString _ s]) =
  error ("buildExp STRING " ++ strPos pos)
buildExp pu (PosExpThunk pos [PosPrim _ SEQ,a1,a2]) =
  error ("buildExp SEQ " ++ strPos pos)
buildExp pu (PosExpThunk pos (PosPrim _ p:args)) =
  error ("buildExp Prim " ++ strPos pos)
buildExp pu (PosExpApp pos (fun:args)) =
  error ("buildExp App " ++ strPos pos)

oneHeap True True  ptr = updHeap 1 >>>= \ hp -> unitS ([PUSH_HEAP,EVALUATED,ptr], Heap hp)
oneHeap False True  ptr = updHeap 1 >>>= \ hp -> unitS ([PUSH_HEAP,ptr], Heap hp)
oneHeap _ False ptr = unitS ([] , Direct ptr)

pushHeapIf True True gs = PUSH_HEAP : EVALUATED : gs
pushHeapIf False True gs = PUSH_HEAP : gs
pushHeapIf _ False gs = gs

heapPtr sp hp (Arg a)  = PUSH_ARG a
heapPtr sp hp (Stack i) = HEAP (sp-i)
heapPtr sp hp (Heap i) = HEAP_OFF (i-hp)
heapPtr sp hp (Direct ins) = ins
