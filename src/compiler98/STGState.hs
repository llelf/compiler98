module STGState where

import Extra(pair,isJust,dropJust,strace)
import State
import IntState
import PosCode
import SyntaxPos
import Gcode
import GcodeLow(con0,cap0,caf,fun,extra)
import StrPos

data Where = Arg Int | Stack Int | Heap Int | HeapLate | Direct Gcode

instance Show Where where
  showsPrec d (Arg i) = showString "Arg " . shows i
  showsPrec d (Stack i) = showString "Stack " . shows i
  showsPrec d (Heap i) = showString "Heap " . shows i
  showsPrec d (HeapLate ) = showString "HeapLate"
  showsPrec d (Direct g) = showString "Direct <gcode>"

              ---    prof fun maxDepth  failstack  state     env           lateenv         depth heap  depth stack
data Thread = Thread Bool Int Int     [(Int,Int)] IntState [[(Int,Where)]] [[(Int,Where)]] Int   Int  [Int]


sums xs = sums' 0 xs
   where
	sums' k [] = [k]
	sums' k (x:xs) = k : sums' (k+x) xs

needstack 0 gs = gs
needstack n gs = NEEDSTACK n : gs


------

pushFail down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  case uniqueIS state of
    (i,state) ->
      (i,Thread prof fun maxDepth ((i,d):fds) state env lateenv d h dhs)

popFail down up@(Thread prof fun maxDepth (_:fds) state env lateenv d h dhs) =
  Thread prof fun maxDepth fds state env lateenv d h dhs

getFail down up@(Thread prof fun maxDepth fds@[] state env lateenv d h dhs) =
  (Nothing,up)
getFail down up@(Thread prof fun maxDepth fds@((f,fd):_) state env lateenv d h dhs) =
  (Just (f,d-fd),up)

maxDepth down up@(Thread prof fun mD fds state env lateenv d h dhs) =
  (mD,up)

getDepth down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) = (d,up)

incDepthIf False down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) = (d,up)
incDepthIf True  down up@(Thread prof fun maxDepth fds state (e:env) lateenv d h dhs) =
  case d + 1 of
    d' ->
      if d' > maxDepth then
        (d',Thread prof fun d' fds state (((-1,Stack d'):e):env) lateenv d' h dhs)
      else
        (d',Thread prof fun maxDepth fds state (((-1,Stack d'):e):env) lateenv d' h dhs)

-- NOTE why is old_i sometimes -1, and if not why did the old code change i?
updTOS False i down up = up
updTOS True  i down up@(Thread prof fun maxDepth fds state (((old_i,w):e):env) lateenv d h dhs) | old_i == -1 =
  Thread prof fun maxDepth fds state (((i,w):e):env) lateenv d h dhs
updTOS True  i down up@(Thread prof fun maxDepth fds state (((old_i,w):e):env) lateenv d h dhs) | old_i == i = up
updTOS _ i down up@(Thread prof fun maxDepth fds state (e:env) lateenv d h dhs) =
   strace ("nhc98 is in deep trouble and might produce faulty code for "++show i++" \n  fun= " ++ show fun ++ "  e = " ++ show e ++ "\n") $ up

incDepth down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  case d + 1 of
    d' ->
      if d' > maxDepth then
        Thread prof fun d' fds state env lateenv d' h dhs
      else
        Thread prof fun maxDepth fds state env lateenv d' h dhs

decDepth s down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  Thread prof fun maxDepth fds state env lateenv (d-s) h dhs

clrHeap down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  Thread prof fun maxDepth fds state env lateenv d 0 dhs

getHeap down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (h,up)
  
updHeap s down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (h,Thread prof fun maxDepth fds state env lateenv d (h+s) dhs)

pushDH down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  Thread prof fun maxDepth fds state env lateenv d h (d:dhs)

cloneDH down up@(Thread prof fun maxDepth fds state env lateenv _ h dhs@(d:_)) =
  Thread prof fun maxDepth fds state env lateenv d h dhs

popDH down up@(Thread prof fun maxDepth fds state env lateenv _ h (d:dhs)) =
  Thread prof fun maxDepth fds state env lateenv d h dhs

pushEnv es down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  -- strace ("Pushenv (" ++ s ++ ") " ++ show es) $
  Thread prof fun maxDepth fds state (es:env) lateenv d h dhs


pushStack es down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  let es' = zip es (map Stack [d+1 .. ])
  in 
    -- strace ("pushStack (" {- ++ s ++ ") "-} ++ show es') $
    Thread prof fun maxDepth fds state (es':env) lateenv (d+length es') h dhs

popEnv down up@(Thread prof fun maxDepth fds state (_:env) lateenv d h dhs) =
  Thread prof fun maxDepth fds state env lateenv d h dhs


gUnique down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  case uniqueIS state of
    (u,state) -> (u,Thread prof fun maxDepth fds state env lateenv d h dhs)

gProf down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (prof,up)

setFun fun down up@(Thread prof _ maxDepth fds state env lateenv d h dhs) =
  (Thread prof fun maxDepth fds state env [] d h dhs)

getExtra c down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  if prof then
    ((4,[HEAP_STATIC fun c,HEAP_CREATE,HEAP_SPACE,HEAP_SPACE]),Thread prof fun maxDepth fds state env lateenv d h dhs)
  else 
    ((0::Int,[]),up)

gState down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (state,up)

gOnly con down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (((1==) . length . constrsI . dropJust . lookupIS state . belongstoI . dropJust . lookupIS state) con ,up)

gWhere i down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (case lookups i env of
    Nothing -> Nothing
    Just (Arg i)  -> Just (Arg i)
    Just (Stack i)-> Just (Stack (d-i))
    Just (Heap i) -> Just (Heap (i-h))  -- always incHeap before lookup
    Just (HeapLate) -> Just HeapLate
  ,up
  )

gWhereAbs i down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (lookups i env
  ,up
  )


lateWhere v down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  (case lookups v lateenv of
    Just x@(Heap i) -> x  -- always incHeap before lookup
    Just x@(Direct g) -> x
    Just x@(Stack i) -> x
    Just _  -> error ("lateWhere (just) failed on " ++ show v ++ " lateenv = " ++ show lateenv)
    _ -> error ("lateWhere (nothing) failed on " ++ show v ++ " lateenv = " ++ show lateenv)
  ,up
  )
            

            
updWhere i down up@(Thread prof fun maxDepth fds state (e:env) lateenv d h dhs) =
  -- strace ("updWhere: i = " ++ show i ++ "  e = " ++ show e) $
  Thread prof fun maxDepth fds state (((i,Stack d):e):env) lateenv d h dhs  -- hide old definition

gArity i down up@(Thread prof fun maxDepth fds state env lateenv d h dhs) =
  case lookups i env of
    Nothing -> (Just (arityIS state i),up)
    Just _ -> (Nothing,up)



lookups i [] = Nothing
lookups i (x:xs) =
  case lookup i x of
    Just v -> Just v
    Nothing -> lookups i xs
