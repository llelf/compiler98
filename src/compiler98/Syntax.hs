module Syntax(module Syntax, Pos(..){-,PackedString-},TokenId) where

import Extra(Pos(..),strChr,strStr)
import PackedString(PackedString)
import TokenId(TokenId)
import Ratio

data Module id =
--     module modid [export] where { impdecls; fixdecls; topdecls }
       Module Pos id [Export id] [ImpDecl id] [FixDecl id] (Decls id)

data Export id =
       ExportEntity  Pos (Entity id)
     | ExportModid   Pos id

data ImpDecl id =
--     import ?qualified? modid ?as modid? ?hiding? (import,..)?
       Import    (Pos,id) (ImpSpec id)
     | ImportQ   (Pos,id) (ImpSpec id)
     | ImportQas (Pos,id) (Pos,id) (ImpSpec id)
     | Importas  (Pos,id) (Pos,id) (ImpSpec id)

data ImpSpec id =
       NoHiding [Entity id]
     | Hiding   [Entity id]

data Entity id =
       EntityVar      Pos id             -- varid
     | EntityTyConCls Pos id             -- TyCon(..) | TyCls(..)
     | EntityTyCon    Pos id [(Pos,id)]  -- TyCon | TyCon(conid,..,conid)
     | EntityTyCls    Pos id [(Pos,id)]  -- TyCls(varid,..,varid) 

data InfixClass a =
                  InfixDef
                | InfixL
                | InfixR 
                | Infix
                | InfixPre a

instance Eq (InfixClass a) where
        InfixDef   == InfixDef   = True
        InfixL     == InfixL     = True
        InfixR     == InfixR     = True
        Infix      == Infix      = True
        (InfixPre _) == (InfixPre _)  = True
        _          == _          = False

instance (Show a) => Show (InfixClass a) where
  showsPrec d InfixDef =  showString "infixl{-def-} "
  showsPrec d InfixL   =  showString "infixl "
  showsPrec d InfixR   =  showString "infixr "
  showsPrec d Infix    =  showString "infix  "
  showsPrec d (InfixPre a) =  showString "prefix " . shows a . showChar ' '


type FixDecl id = (InfixClass id,Int,[FixId id])

data FixId a =
   FixCon Pos a
 | FixVar Pos a

stripFixId (FixCon _ a) = a
stripFixId (FixVar _ a) = a


data Decls id =
   DeclsParse [Decl id]
 | DeclsScc [DeclsDepend id]

data DeclsDepend id =
       DeclsNoRec (Decl id)
     | DeclsRec   [Decl id]

data Decl id =
--        type   simple  = type
       DeclType (Simple id) (Type id)
--        {data/data unboxed/newtype} context => simple constrs deriving (tycls)
     | DeclData (Maybe Bool) [Context id] (Simple id) [Constr id] [(Pos,id)]
--        data primitive conid size
     | DeclDataPrim Pos id Int
	-- Introduced by Rename to mark that we might need to generate selector funktions
     | DeclConstrs Pos id [(Pos,id,id)]  -- position data/dataprim [(field,selector)]
--        class context => class where { csign; valdef }
     | DeclClass Pos [Context id] id id (Decls id)
--        instance context => tycls inst where { valdef }
     | DeclInstance Pos [Context id] id (Instance id) (Decls id)
--        default (type,..)
     | DeclDefault [Type id]
--        var primitive arity :: type
     | DeclPrimitive Pos id Int (Type id)
--        foreign import [callconv] [extfun] [unsafe|cast] var :: type
     | DeclForeignImp Pos String id Int Bool (Type id)
--        foreign export  callconv  [extfun]  var :: type
     | DeclForeignExp Pos String id (Type id)
--      vars :: context => type
     | DeclVarsType [(Pos,id)] [Context id] (Type id)
     | DeclPat (Alt id)
     | DeclFun Pos id [Fun id]
--     | DeclSelect id Int id  -- introduced with pattern elimination (id = select Int id)
--     Used for unimplemented things
     | DeclIgnore String
     | DeclError String
     | DeclAnnot (Decl id) [Annot id]
--     infix[rl] int id,..,id
     | DeclFixity (FixDecl id)

data ClassCode ctx id =
   CodeClass Pos id 		   -- cls
 | CodeInstance Pos id id [id] [ctx] [id]  -- cls typ arg ctxs methods


data Annot id = AnnotArity (Pos,id) Int
              | AnnotPrimitive (Pos,id) PackedString
              | AnnotNeed [[id]]
              | AnnotUnknown

--                 lhs pats, guarded exprs,   local defs  ???
data Fun id = Fun  [Pat id] [(Exp id,Exp id)] (Decls id)

data Alt id = Alt  (Pat id) [(Exp id,Exp id)] (Decls id)

data Type id =
       TypeCons         Pos id [Type id]
     | TypeApp          (Type id) (Type id)
     | TypeVar          Pos id
     | TypeStrict       Pos (Type id)

data Sig id = Sig [(Pos,id)] (Type id)

data Simple id = Simple Pos id [(Pos,id)]

data Context id = Context Pos id (Pos,id)

-- ConstrCtx is always used if forall is specified
-- the intention is to remove Constr completely when all of nhc13 have been updated 
--                          forall      context     constructor   argumentlist with fields if any
data Constr id = Constr                             Pos id        [(Maybe [(Pos,id)],Type id)]
               | ConstrCtx  [(Pos,id)] [Context id] Pos id        [(Maybe [(Pos,id)],Type id)]

type Instance id = Type id  -- Not TypeVar

data Stmt id =
    StmtExp  (Exp id)		-- exp
  | StmtBind (Exp id) (Exp id)	-- pat <- exp
  | StmtLet (Decls id)		-- let { decls ; }

data Exp id =
      ExpScc            String (Exp id)
    | ExpDict           (Exp id)         -- hack to mark dictionary arguments
    | ExpLambda         Pos [(Pat id)] (Exp id)  -- \ pat ... pat -> exp
    | ExpLet            Pos (Decls id) (Exp id)  -- let { decls ; } in exp
    | ExpDo             Pos [Stmt id]            -- do { stmts ; }
    | ExpCase           Pos (Exp id) [Alt id]    -- case exp of { alts; }
    | ExpFatbar         (Exp id) (Exp id)
    | ExpFail
    | ExpIf             Pos (Exp id) (Exp id) (Exp id) 	-- if exp then exp else exp
    | ExpType           Pos (Exp id) [Context id] (Type id)     		-- exp :: context => type
--- Above only in expressions
    | ExpRecord	        (Exp id) [Field id]
    | ExpApplication    Pos [Exp id]
    | ExpInfixList      Pos [Exp id]                            		-- Temporary removed during rename
    | ExpVar            Pos id
    | ExpCon            Pos id
    | ExpVarOp          Pos id
    | ExpConOp          Pos id
    | ExpLit            Pos (Lit Boxed)
    | ExpList           Pos [Exp id]
--- after typechecker
    | Exp2              Pos id id						-- e.g.   Ord.Eq      or Eq.Int
--- Below only in patterns
    | PatAs             Pos id (Pat id)
    | PatWildcard       Pos
    | PatIrrefutable    Pos (Pat id)
-- (n+k) pattern - store:   n  n' (k<=n')  (n'-k)
    | PatNplusK		Pos id id (Exp id) (Exp id) (Exp id)

data Field id = FieldExp  Pos id (Exp id)
              | FieldPun  Pos id	-- H98 removes (retained for error msgs)

data Boxed = Boxed | UnBoxed

instance Eq Boxed where
  Boxed == Boxed = True
  UnBoxed == UnBoxed = True
  _     == _       = False

instance Show Boxed where
  showsPrec d Boxed   = id
  showsPrec d UnBoxed = showChar '#'


data Lit boxed =
      LitInteger  boxed Integer
    | LitRational boxed Rational
    | LitString   boxed String
    | LitInt      boxed Int
    | LitDouble   boxed Double
    | LitFloat    boxed Float
    | LitChar     boxed Char

instance (Eq b) => Eq (Lit b) where
     a == a' = litEqual a a'   -- litEqual needed in Symbols to force correct type in gofer

litEqual (LitInteger  b i) (LitInteger  b' i') = i == i' && b == b'
litEqual (LitRational b i) (LitRational b' i') = i == i' && b == b'
litEqual (LitString   b s) (LitString   b' s') = s == s' && b == b'
litEqual (LitInt      b i) (LitInt      b' i') = i == i' && b == b'
litEqual (LitDouble   b f) (LitDouble   b' f') = f == f' && b == b'
litEqual (LitFloat    b f) (LitFloat    b' f') = f == f' && b == b'
litEqual (LitChar     b c) (LitChar     b' c') = c == c' && b == b'
litEqual _                  _            = False

instance (Show b) => Show (Lit b) where
  showsPrec d lit = litshowsPrec d lit  -- litshowsPrec needed in Symbols to force correct type in gofer


litshowsPrec d (LitInteger  b i) = showsPrec d i . showChar 'L' . shows b
litshowsPrec d (LitRational b i) = showsPrec d i . shows b
litshowsPrec d (LitString b str)= showString (strStr str) . shows b
litshowsPrec d (LitInt    b i)  = showsPrec d i . shows b
litshowsPrec d (LitDouble b f)  = showsPrec d f . shows b
litshowsPrec d (LitFloat  b f)  = showsPrec d f . shows b
litshowsPrec d (LitChar   b chr)= showString (strChr chr). shows b

data Qual id =
--       pat <- exp
      QualPatExp (Pat id) (Exp id)
--       pat
    | QualExp (Exp id)
--	 let decls
    | QualLet (Decls id)

type Pat id = Exp id

--------------------

data Interface id = 
--      interface modid where {iimpdecl; fixdecl; itopdecl }
       Interface Pos id [IImpDecl id] [FixDecl id] (IDecls id)

type IImpDecl id = ImpDecl id -- No Hiding in ImpSpec

type IDecls id = Decls id -- No Valdef

