-- a toy prelude for Hat tracing
-- wraps a few essential Prelude functions and types to test portable tracing
module TPrelude where

import Hat as T


tMain = T.mkModule "Prelude" "Prelude.hs"

data List a = Cons (R a) (T.R (List a)) | List  
  -- type constructor and empty list constructor need to have same name,
  -- because transformation doesn't distinguish the two

aCons = T.mkAtomId tMain 0 21 ":"
aList = T.mkAtomId tMain 0 3 "[]"

type Char = Prelude.Char

type String = List Char

type IO a = Prelude.IO (R a)


oputStr :: T.SR -> T.Trace 
        -> T.R (T.Trace -> (T.R String) -> T.R (IO ()))

oputStr pputStr tputStr =
  T.fun1 a8v1putStr wputStr pputStr tputStr

a8v1putStr = T.mkAtomId tMain 0 3 "putStr"

wputStr :: Trace -> T.R String -> T.R (IO ())
wputStr t os = R (do
  let s = unwrap os
  outputTrace t s
  putStr s
  return (T.R () t))
  t

unwrap :: T.R String -> Prelude.String
unwrap (T.R s _) = go s
  where
  go List = []
  go (Cons (R c _) (R rest _)) = c : go rest


-- error :: T.SR -> T.Trace -> T.R (Trace -> T.R String -> a)
-- should have special wrapper that terminates the program
-- writing message into trace


