module NHCBackend
  ( cNhc
  , hNhc
--, genProcNHC
  ) where

#if defined(__NHC__) || defined(__HBC__)
import NonStdTrace
#else
#if defined(__GLASGOW_HASKELL__)
import IOExts (trace)
#endif
#endif
import Pretty
import PrettyUtils (textline,vcatMap,vsepMap,vsep,commaList,ppList)
import Decl        (Decl(..), Sig, Call, CCode, Fail, Result)
import DIS         (DIS(..), expandDIS, DISEnv, simplify)
import FillIn      (fillinProc, Consts, genConsts)
import Casm        (BaseTy(..), baseTyToCall, baseTyToRtn, baseToCType)
import Type        (ppType, isPureType)
import Maybe       (fromMaybe)
import Name
import NameSupply

cNhc, hNhc :: Bool -> DISEnv -> [String] -> Decl -> Doc
 
cNhc dbg disEnv pre (Haskell _) = empty
cNhc dbg disEnv pre (C c) = text c
cNhc dbg disEnv pre (DisDef _ _ _) = empty
cNhc dbg disEnv pre (Prefix _) = empty
cNhc dbg disEnv pre (Include _) = error "%#include is obsolete."
cNhc dbg disEnv pre (Constant ty ns) = --nyi "%const"
  vcat (map (cBit . fillinProc disEnv pre) (genConsts disEnv (ty,ns)))
cNhc dbg disEnv pre (ProcSpec sig mbcall mbcode mbfail mbresult) =
  cBit $
  fillinProc disEnv pre $
  (sig, mbcall, mbcode, fromMaybe [] mbfail, mbresult)
 
hNhc dbg disEnv pre (Haskell h) = text h
hNhc dbg disEnv pre (C _) = empty
hNhc dbg disEnv pre (DisDef _ _ _) = empty
hNhc dbg disEnv pre (Prefix _) = empty
hNhc dbg disEnv pre (Include _) = error "%#include is obsolete"
hNhc dbg disEnv pre (Constant ty ns) = --nyi "%const"
  vcat (map (haskellBit dbg . fillinProc disEnv pre) (genConsts disEnv (ty,ns)))
hNhc dbg disEnv pre (ProcSpec sig mbcall mbcode mbfail mbresult) =
  haskellBit dbg $
  fillinProc disEnv pre $
  (sig, mbcall, mbcode, fromMaybe [] mbfail, mbresult)


--genProcNHC :: (Sig, Call, CCode, Fail, Result) -> (Doc, Doc, Doc)
--genProcNHC arg =
--  (haskellBit False arg, cBit arg, empty)

haskellBit dbg ((name, typ), calls, code, fails, result) =
  let fnName    = text name
      grName    = text "gr_" <> fnName
      grResult  = text "gr_result"
      arity     = text (show (length argTypes))
      primTypes = ppList (text " -> ") rtnType argTypes
      argDISs   = map simplify calls
      rtnDIS    = simplify result
      argTypes  = concat $
                  fst $ initNS (mapM hargtypes argDISs) (nameSupply "a")
      (eqns,argPats) = fst $ initNS (hargpats argDISs) (nameSupply "tmp")
      argsCall  = fst $ initNS (mapM hargcall argDISs) (nameSupply "arg")
      rtnType   = fst $ initNS (hrtntype rtnDIS) (nameSupply "r")
      rtnCons   = fst $ initNS (hrtncons rtnDIS) (nameSupply "res")
      rtnPat    = fst $ initNS (hrtnpat rtnDIS)  (nameSupply "res")
      emit c x  = if c then trace x else id
  in
  emit dbg ("*****ARGS:\n"++unlines (map show argDISs)) $
  emit dbg ("*****RESULT:\n"++show rtnDIS++"\n") $
  grName <+> text "primitive" <+> arity <+> text "::" <+> primTypes $$
  text ""   $$
  fnName <+> text "::" <+> ppType typ   $$
  fnName <+> hsep argPats <+> equals    $$
  nest 2 (
    if isPureType typ then
      if clean rtnDIS && and (map clean argDISs) then
        grName <+> hsep argsCall
      else
        text "let" $$
        nest 4 (vsep eqns $$
                rtnPat <+> equals <+> grName <+> hsep argsCall) $$
        text "in"  <+> rtnCons
    else
      text "IO" <+>
      parens (
        text "\\_ ->"   $$
        nest 2 (
          text "let" $$
          nest 4 (vsep eqns $$
                  grResult <+> equals <+> grName <+> hsep argsCall) $$
          if clean rtnDIS then
            text "in" <+> text "seq" <+> grResult <+>
            parens (text "Right" <+> grResult)
          else
            nest 4 (rtnPat <+> equals <+> grResult)   $$
            text "in" <+> text "seq" <+> grResult <+>
            parens (text "Right" <+> rtnCons)
        )
      )
  ) $$
  text "\n"

cBit ((name, typ), calls, code, fails, result) =
  let argDISs  = map simplify calls
      argDecls = cdecls [] argDISs
      argDefs  = concatMap cdefs argDISs
      rtnDIS   = simplify result
      rtnDecls = cdecls [] [rtnDIS]
      rtnDefs  = crtn rtnDIS
  in
  text ""   $$
  text "C_HEADER" <+> parens (text "gr_" <> text name) $$
  cblock (
    (text "NodePtr nodeptr"):
    argDecls ++
    [ text ""   $$
      text "nodeptr = C_GETARG1" <> parens (text (show n)) <> semi $$
      text "IND_REMOVE" <> parens (text "nodeptr") <> semi $$
      cdef
    | (cdef,n) <- zip argDefs [1..] ] ++
    [ text ""   $$
      cblock (
        rtnDecls ++
        [ text ""   $$
          text "/* User code starts here */"   $$
          vcatMap text code   $$
          text "/* User code ends here */"   $$
          text ""   $$
          text "nodeptr = " <> rtnDefs <> semi   $$
          text "C_RETURN" <> parens (text "nodeptr")
        ]
      )
    ]
  )


--------
hargtypes ::  DIS  -> NSM  [Doc]
hrtntype  ::  DIS  -> NSM   Doc
hargpats  :: [DIS] -> NSM ([Doc],[Doc])
hrtncons  ::  DIS  -> NSM   Doc
hargcall  ::  DIS  -> NSM   Doc
hrtnpat   ::  DIS  -> NSM   Doc

hargtypes (Apply (BaseDIS (Foreign _)) ds)   = return $ [text "ForeignObj"]
hargtypes (Apply (BaseDIS StablePtr) ds)     = getNewName >>= \n->
                                               return [text "StablePtr" <+> text n]
hargtypes (Apply (BaseDIS Word) ds)          = getNewName >>= \n->
                                               return [text n]
hargtypes (Apply (BaseDIS b) ds)   = return $ [text (show b)]
hargtypes (Apply d ds)             = mapM hargtypes ds >>= return . concat
hargtypes (BaseDIS b)              = return $ []
hargtypes (Constructor c)          = return $ []
hargtypes (Declare cty d)          = return $ []
hargtypes (Exp e)                  = return $ []
hargtypes (Record n ns)            = return $ []
hargtypes (Tuple)                  = return $ [text "()"]
hargtypes (UserDIS f t)            = return $ []
hargtypes (Var v)                  = return $ []
--
hrtntype (Apply (BaseDIS (Foreign _)) ds)   = return $ text "ForeignObj"
hrtntype (Apply (BaseDIS StablePtr) ds)     = getNewName >>= return . (text "StablePtr" <+>) . text
hrtntype (Apply (BaseDIS Word) ds)          = getNewName >>= return . text
hrtntype (Apply (BaseDIS b) ds)    = return $ text (show b)
hrtntype (Apply d [r])             = hrtntype r
hrtntype (Apply d ds)              = mapM hrtntype ds >>=
                                     return . parens . commaList
hrtntype (BaseDIS b)               = return $ empty
hrtntype (Constructor c)           = return $ empty
hrtntype (Declare cty d)           = return $ empty
hrtntype (Exp e)                   = return $ empty
hrtntype (Record n ns)             = return $ empty
hrtntype (Tuple)                   = return $ text "()"
hrtntype (UserDIS f t)             = return $ empty
hrtntype (Var v)                   = return $ empty
--
{-
  hargpat (Apply Tuple ds)           = mapM hargpat ds >>=
                                       return . parens . commaList
  hargpat (Apply (Constructor c) ds) = mapM hargpat ds >>= \ps->
                                       return $ parens (text c <+> hsep ps)
  hargpat (Apply (Record n ns) ds)   = mapM hargpat ds >>= \ps->
                                       return $ parens (text n <+> feqList ns ps)
  hargpat (Apply (UserDIS f t) ds)   = mapM hargpat ds >>= return . hsep
--hargpat (Apply (UserDIS f t) ds) = getNewName >>= return . text
  hargpat (Apply d ds)               = mapM hargpat ds >>= return . hsep
  hargpat (BaseDIS b)                = return $ empty
  hargpat (Constructor c)            = return $ text c
  hargpat (Declare cty d)            = hargpat d
  hargpat (Exp e)                    = getNewName >>= return . text
  hargpat (Record n ns)              = return $ empty
  hargpat (Tuple)                    = return $ text "()"
  hargpat (UserDIS f t)              = return $ empty
  hargpat (Var v)                    = return $ text v
-}
hargpats ds = mapM hargpat ds >>= \xs->
              let (eqnss,pats) = unzip xs in
              return (concat eqnss, pats)
hargpat (Apply (UserDIS f t) ds)   = getNewName  >>= \n->
                                     hargpats ds >>= \(eqns,pats)->
                                     return ((hsep pats <+> equals <+>
                                              text f <+> text n): eqns,
                                             text n)
hargpat (Apply Tuple ds)           = hargpats ds >>= \(eqns,pats)->
                                     return (eqns, parens (commaList pats))
hargpat (Apply (Constructor c) ds) = hargpats ds >>= \(eqns,pats)->
                                     return (eqns,
                                             parens (text c <+> hsep pats))
hargpat (Apply (Record n ns) ds)   = hargpats ds >>= \(eqns,pats)->
                                     return (eqns,
                                             parens (text n <+> feqList ns pats))
hargpat (Apply d ds)               = hargpats ds >>= \(eqns,pats)->
                                     return (eqns, hsep pats)
hargpat (BaseDIS b)                = return $ ([],empty)
hargpat (Constructor c)            = return $ ([],text c)
hargpat (Declare cty d)            = hargpat d
hargpat (Exp e)                    = getNewName >>= \n-> return ([],text n)
hargpat (Record n ns)              = return $ ([],empty)
hargpat (Tuple)                    = return $ ([],text "()")
hargpat (UserDIS f t)              = return $ ([],empty)
hargpat (Var v)                    = return $ ([],text v)
--
hrtncons (Apply Tuple ds)          = mapM hrtncons ds >>=
                                     return . parens .  commaList
hrtncons (Apply (Constructor c) ds)= mapM hrtncons ds >>= \cs->
                                     return $ parens (text c <+> hsep cs)
hrtncons (Apply (Record n ns) ds)  = mapM hrtncons ds >>= \cs->
                                     return $ parens (text n <+> feqList ns cs)
hrtncons (Apply (UserDIS f t) ds)  = mapM hrtncons ds >>= \cs->
                                     return $ parens (text t <+> hsep cs)
hrtncons (Apply d ds)              = mapM hrtncons ds >>= return . hsep
hrtncons (BaseDIS b)               = return $ empty
hrtncons (Constructor c)           = return $ text c
hrtncons (Declare cty d)           = hrtncons d
hrtncons (Exp e)                   = getNewName >>= return . text
hrtncons (Record n ns)             = return $ empty
hrtncons (Tuple)                   = getNewName >>= return . text
                                     -- return $ text "()"
hrtncons (UserDIS f t)             = return $ empty
hrtncons (Var v)                   = return $ text v
--
{-hargcall (Apply (Declare cty (Var v)) ds) = text v-}
--hargcall (Apply (UserDIS f t) ds)  = mapM hargcall ds >>= \as->
--                                     return $ parens (text f <+> hsep as)
hargcall (Apply (BaseDIS StablePtr) [d]) = hargcall d >>= return . parens . (text "StablePtr" <+>)
hargcall (Apply d ds)              = mapM hargcall ds >>= return . hsep
hargcall (BaseDIS b)               = return $ empty
hargcall (Constructor c)           = return $ empty
hargcall (Declare cty d)           = hargcall d
hargcall (Exp e)                   = getNewName >>= return . text
hargcall (Record n ns)             = mapM (hargcall . Var) ns >>= return . hsep
hargcall (Tuple)                   = return $ text "()"
hargcall (UserDIS f t)             = return $ empty
hargcall (Var v)                   = return $ text v
--
hrtnpat (Apply (BaseDIS StablePtr) [r]) = hrtnpat r >>= return . parens . (text "StablePtr" <+>)
hrtnpat (Apply d [r])              = hrtnpat r
hrtnpat (Apply d ds)               = mapM hrtnpat ds >>=
                                     return . parens . commaList
hrtnpat (BaseDIS b)                = return $ empty
hrtnpat (Constructor c)            = return $ empty
hrtnpat (Declare cty d)            = hrtnpat d
hrtnpat (Exp e)                    = getNewName >>= return . text
hrtnpat (Record n ns)              = mapM (hrtnpat . Var) ns >>=
                                     return . parens . commaList
hrtnpat (Tuple)                    = getNewName >>= return . text
                                     -- return $ text "()"
hrtnpat (UserDIS f t)              = return $ empty
hrtnpat (Var v)                    = return $ text v

--------
cdecls :: [String] -> [DIS] -> [Doc]
cdefs  ::              DIS  -> [Doc]
crtn   ::              DIS  ->  Doc

cdecls env ((Apply d ds):rest) = cdecls env (d:ds++rest)
cdecls env ((Declare cty (Var v)):ds)
  | v `notElem` env = (text cty <+> text v): cdecls (v:env) ds
cdecls env (d:ds) = cdecls env ds
cdecls env  []    = []

--
cdefs (Apply (BaseDIS b) [r]) = [collect b (baseToCType b) r]
cdefs (Apply d ds) = cdefs d ++ concatMap cdefs ds
cdefs d = []

collect hty cty (Apply (Declare _ _) [r]) = collect hty cty r
collect hty cty (Declare cast d) = collect hty cast d
collect hty cty (Var v) = 
  text v      <+> equals <+> parens (text cty) <> text (baseTyToCall hty)
collect hty cty (Exp e) = 
  text (pc e) <+> equals <+> parens (text cty) <> text (baseTyToCall hty)
collect hty cty d = error ("BaseDIS "++show hty++" applied to "++show d)

pc :: String -> String
pc = filter (/='%')

--
crtn (Apply (BaseDIS b) [r])     = crtn' (baseTyToRtn b) r
crtn (Apply d [r])               = crtn r
crtn (Apply d ds)                =
  let rs = map crtn ds
      n  = length rs
  in
  text "mkTuple" <> text (show n) <> parens (commaList rs)
crtn (Tuple)                     = text "mkUnit()"
crtn d = empty

crtn' wrap (Apply d [r])         = crtn' wrap r
crtn' wrap (Declare cty d)       = crtn' wrap d
crtn' wrap (Exp e)               = text (wrap (pc e))
crtn' wrap (Var v)               = text (wrap v)
crtn' wrap d                     = error ("BaseDIS applied to complex DIS: "++
                                           wrap (show d))

--------
cblock :: [Doc] -> Doc
cblock ds =
  text "{" $$
  nest 2 (foldr (\a as-> a<>semi $$ as) empty ds) $$
  text "}" $$
  text ""

feqList :: [Name] -> [Doc] -> Doc
feqList ns pats = braces (commaList (zipWith feq ns pats))
  where feq name pat = text name <> text "=" <> pat

nyi s = error ("Not yet implemented: "++s)

clean :: DIS -> Bool
clean (Apply d ds)    = clean d && and (map clean ds)
clean (BaseDIS StablePtr) = True	-- previously False (until 1999-07-07)
clean (BaseDIS b)     = True
clean (Constructor c) = False
clean (Declare cty d) = True
clean (Exp e)         = True
clean (Record n ns)   = False
clean (Tuple)         = True
clean (UserDIS f t)   = False
clean (Var v)         = True

--------
