{- ---------------------------------------------------------------------------
-- Transforms primitive FFI decls to introduce a function wrapper, which
-- is either an IO wrapper, or a pure wrapper.  We need to expand all
-- type synonyms at this stage in order to tell if we have an IO result type.
-- Hence, this phase needs to be after renaming (so that the type synonym
-- information we need is available in the symbol table).
--
-- This module also writes the type of the prim to the symbol table,
-- but not the type of the wrapper, which is done later during the
-- type extraction phase (we want the compiler to at least ensure
-- that the wrapper introduced here is type correct!).
-- ------------------------------------------------------------------------- -}

module FFITrans (ffiTrans) where

import IdKind
import TokenId
import IntState
import Syntax
import State
import NT
import AssocTree
import Id
import Extract   (type2NT)
import TypeSubst (substNT)
import Extra     (snub,dropJust)
import IExtract  (freeType)
import PackedString

type FFIMonad a = State In Out a Out
type In  = ((TokenId,IdKind) -> Id)      -- lookup fn
type Out = IntState

ffiTrans :: Decls Id                     -- input declarations
             -> ((TokenId,IdKind) -> Id) -- lookup name in symbol table
             -> IntState                 -- internal compiler state
             -> (Decls Id                -- modified declarations
                ,IntState)               -- modified internal state

ffiTrans topdecls lookup state =
    case ffiDecls topdecls lookup state of
        (decls', state') -> (decls', state')

ffiDecls :: Decls Id -> FFIMonad (Decls Id)
ffiDecls (DeclsParse ds) =
    unitS DeclsParse =>>>
    (mapS ffiDecl ds >>>= unitS . concat)

ffiDecl :: Decl Id -> FFIMonad [Decl Id]
ffiDecl d@(DeclForeignImp pos cname id _ cast typ x) =
    examineType pos typ   >>>= \(isIO,nt,typ',arity,adjarity)->
    if isIO then
      copyPrim id cname          >>>= \(id',cname') ->
      makeWrapper pos id' arity  >>>= \code->
      updVar pos id' (NewType (snub (freeType typ)) [] [] [nt]) adjarity >>>
      unitS [ DeclForeignImp pos cname' id' adjarity cast typ' x
            , DeclVarsType [(pos,id)] [] typ
            , DeclFun pos id [code]
            ]
    else
      updVar pos id (NewType (snub (freeType typ)) [] [] [nt]) arity >>>
      unitS [d]
ffiDecl d = unitS [d]


-- Copy original prim to new symtable location leaving original slot
-- for the new wrapper.  Return the new ID, together with the proper
-- C name for the foreign function (if it was originally blank, we must
-- use the Haskell name).
copyPrim :: Id -> String -> FFIMonad (Id,String)
copyPrim id cname lookup state =
    case uniqueIS state of
      (i, state1) ->
        let newname = Qualified m (packString ('#':unpackPS name))
            info    = InfoVar i newname fix IEnone NoType ar
            state2  = addIS i info state1
            cname'  = if null cname then reverse (unpackPS name) else cname
        in ((i, cname'), state2)
  where
    (Just (InfoVar _ (Qualified m name) fix ie nt ar)) = lookupIS state id


-- Report whether the result type is IO, after expanding all type synonyms.
-- Return also the NewType representation with all expansion completed,
-- the original Type representation likewise, the real arity, and
-- a possibly padded arity.  (Arity is padded from zero to one for IO
-- types, to ensure re-execution happens as expected.)

examineType :: Pos -> Type Id -> FFIMonad (Bool, NT, Type Id, Int, Int)
examineType pos t lookup state =
  let
    expand n@(NTvar v)      = n
    expand   (NTstrict nt)  = NTstrict (expand nt)
    expand   (NTapp t1 t2)  = NTapp (expand t1) (expand t2)
    expand   (NTcons c nts) =
        case (typeSynonymBodyI . dropJust . lookupIS state) c of
          Just nt -> expand (subst nt nts)
          Nothing -> NTcons c (map expand nts)
    expand   _              = error "Unexpected type error in foreign decl"

    subst (NewType free [] ctxs [nt]) nts = substNT (zip free nts) nt

    arrow = lookup (t_Arrow,TCon)
    io    = lookup (tIO,TCon)
    unit  = lookup (t_Tuple 0,TCon)

    isIO  (NTcons c nts)  | c==io    = True
    isIO  _                          = False
    rmIO  (NTcons c [nt]) | c==io    = nt
    rmIO  nt                         = nt

    toList (NTcons c nts) | c==arrow  = let [a,b] = nts in a: toList b
    toList (NTstrict nt)  = toList nt
    toList nt             = [nt]

    (resNT:argsNT) = (reverse . toList . expand . type2NT) t

    boolIO = isIO resNT

    adjustArity True 0 = 1	-- Change zero-arity (IO a) to (()->IO a)
    adjustArity _    n = n

    adjustArgs True [] = [NTcons unit []]
    adjustArgs _    as = reverse as

    finalNT =
      foldr (\a b-> NTcons arrow [a,b]) (rmIO resNT) (adjustArgs boolIO argsNT)
  in
    ( ( boolIO
      , finalNT
      , nt2type pos finalNT
      , length argsNT
      , adjustArity boolIO (length argsNT)
      )
    , state)

makeWrapper :: Pos -> Id -> Int -> FFIMonad (Fun Id)
makeWrapper pos ffn arity lookup state =
    ( Fun [] [(ExpCon pos (lookup (tTrue,Con))
              ,ExpApplication pos [ExpVar pos (lookup (t_mkIOok arity,Var))
                                  ,ExpVar pos ffn]
              )]
             (DeclsParse [])
    , state )

-- updVar :: Pos -> Int -> NewType -> FFIMonad ()
updVar pos id nt ar lookup state =
   (updVarArity pos id ar . updVarNT pos id nt) state

-- nt2type reverses the type2NT translation
nt2type :: Pos -> NT -> Type Id
nt2type p (NTapp t1 t2) = TypeApp (nt2type p t1) (nt2type p t2)
nt2type p (NTcons c ts) = TypeCons p c (map (nt2type p) ts)
nt2type p (NTstrict t)  = TypeStrict p (nt2type p t)
nt2type p (NTvar v)     = TypeVar p v
nt2type p _             = error "FFI transformation: nt2type"
