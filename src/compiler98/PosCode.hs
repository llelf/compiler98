module PosCode(module Prim, module PosCode, Pos(..)) where

import Extra(Pos(..),noPos)
import Prim
import Foreign (ImpExp)
import Syntax (CallConv)
import SyntaxPos
	 
type PosCode = [PosBinding]

type PosBinding     = (Int, PosLambda)
	 
data PosLambda
   = PosLambda Pos [(Pos,Int)] [(Pos,Int)] PosExp
   | PosPrimitive Pos Int
   | PosForeign Pos Int String CallConv ImpExp

posExpApp pos [a] = a
posExpApp pos as  = PosExpApp pos as

posExpLet pos [] exp = exp
posExpLet pos bindings exp = PosExpLet pos bindings exp
	 
data PosExp
    = PosExpDict PosExp -- hack to mark dictionaries
    | PosExpLet  Pos [PosBinding] PosExp
    | PosExpCase Pos PosExp [PosAlt]
    | PosExpApp  Pos [PosExp]
    | PosExpThunk  Pos [PosExp]
    | PosExpFatBar  Bool PosExp PosExp  -- True if fail can escape fatbar
    | PosExpFail
    | PosExpIf   Pos PosExp PosExp PosExp
    | PosVar Pos Int 
    | PosCon Pos Int
    | PosInt Pos Int
    | PosChar Pos Int
    | PosFloat   Pos Float
    | PosDouble  Pos Double
    | PosInteger Pos Integer
    | PosString  Pos String
    | PosPrim    Pos Prim
 -- Only temporary !!
    | PosExpLambda  Pos [(Pos,Int)] [(Pos,Int)] PosExp

data PosAlt
    = PosAltCon Pos Int [(Pos,Int)] PosExp  -- Constructor numbers, new variables, expression
    | PosAltInt Pos Int             PosExp

isPosAtom (PosVar _ _) = True
isPosAtom (PosCon _ _) = True
isPosAtom (PosInt _ _) = True
isPosAtom (PosChar _ _) = True
isPosAtom (PosFloat   _ _) = True
isPosAtom (PosDouble  _ _) = True
isPosAtom (PosInteger _ _) = True
isPosAtom (PosString  _ _) = True
isPosAtom (PosPrim    _ _) = True
isPosAtom (PosExpThunk  _ [atom]) = isPosAtom atom -- thunks representing zero arity functions and constructors are atoms
isPosAtom _ = False

instance HasPos PosExp where
  getPos (PosExpDict exp) = getPos exp
  getPos (PosExpLet  pos _ _) = pos
  getPos (PosExpCase pos _ _) = pos
  getPos (PosExpApp  pos _) = pos
  getPos (PosExpThunk pos _) = pos
  getPos (PosExpFatBar _ e _) = getPos e
  getPos (PosExpFail) = noPos
  getPos (PosExpIf   pos _ _ _) = pos
  getPos (PosVar pos _) = pos
  getPos (PosCon pos _) = pos
  getPos (PosInt pos _) = pos
  getPos (PosChar pos _) = pos
  getPos (PosFloat   pos _) = pos
  getPos (PosDouble  pos _) = pos
  getPos (PosInteger pos _) = pos
  getPos (PosString  pos _) = pos
  getPos (PosPrim    pos _) = pos
  getPos (PosExpLambda  pos _ _ _) = pos
