module AuxLabelAST
  ( auxLabelSyntaxTree
  ) where


import AuxFile
import AuxFixity
import TraceId
import Flags
import Syntax
import SyntaxUtil (infixFun)
import TokenId (TokenId,visImpRev)
import AssocTree


-- `auxLabelSyntaxTree' relabels the entire abstract syntax tree,
-- replacing TokenId with TraceId (which basically adds arity and
-- binding information for each variable name).  Along the way, we
-- also need to resolve the fixity of all applications (originally
-- done in a later phase of the compiler).

auxLabelSyntaxTree :: Flags -> Module TokenId -> IO (Module TraceId)
auxLabelSyntaxTree flags
	mod@(Module _ _ _ imports fixdecls (DeclsParse decls)) =
  do
    let (toIdent,_) = mkIdentMap decls
    let localenv = extendEnv vi
                             toIdent initAT (map DeclFixity fixdecls ++ decls)
    totalenv <- getImports (\_->vi) localenv flags imports
    let env = reorderAT stripType totalenv
    return (relabel env mod)
  where
    stripType env (Con _ con, aux)    = addAT env const (Con "" con) aux
    stripType env (Method _ met, aux) = addAT env const (Var met) aux
    stripType env (v@(Var _), aux)    = addAT env const v aux


-- `im' is a degenerate IdentMap.  We only need to map constructors
-- back to their type, or methods to their class, when dealing with
-- the export list in writing the .hx file.  But when relabeling the
-- syntax tree, the type is irrelevant, so we map all constructors to
-- the empty type.  Likewise, `vi' is a degenerate visibility function.

im :: IdentMap
im = \v-> let name = show v in if isCon name then Con "" name else Var name

vi :: Visibility
vi = \_-> True


-- `letVar' and `lookEnv' build a TraceId for any TokenId by
-- looking up the arity information in the environment.  For letVar,
-- the distinction between let-bound and lambda-bound variables
-- is decided by the caller, but for `lookEnv', it is decided by the
-- environment.

letVar :: AuxTree -> TokenId -> TraceId
letVar env id =
  let v = mkVar id in
  case lookupAT env v of
    Just info | letBound info -> id `plus` info
    _ -> error ("AuxLabelAST.letVar: "++show v++" not let-bound in env")

lookEnv env mk id =
  let v = mk id in
  case lookupAT env v of
    Just info -> id `plus` info
    _ -> error ("AuxLabelAST.lookEnv: "++show v++" not in environment")

mkVar, mkCon :: TokenId -> Identifier
mkVar id = Var (show id)
mkCon id = Con "" (show id)

-- The class `Relabel' walks the abstract syntax tree, relabelling all
-- TokenId to TraceId.  Most instances are pretty trivial - the only
-- interesting ones are at the end of this section.

class Relabel g where
  relabel :: AuxTree -> g TokenId -> g TraceId

instance Relabel Module where
  relabel env (Module p mod exps imps fixs decls) =
    Module p (just mod) (map (relabel env) exps) (map (relabel env) imps)
			[] (relabel env decls)
					-- fixdecls are folded into the decls

instance Relabel Export where
  relabel env (ExportEntity pos entity) = ExportEntity pos (relabel env entity)
  relabel env (ExportModid pos id)      = ExportModid pos (just id)

instance Relabel ImpDecl where
  relabel env (Import (pos,id) impspec) =
		Import (pos,just id) (relabel env impspec)
  relabel env (ImportQ (pos,id) impspec) =
		ImportQ (pos,just id) (relabel env impspec)
  relabel env (ImportQas (p1,id1) (p2,id2) impspec) =
		ImportQas (p1,just id1) (p2,just id2) (relabel env impspec)
  relabel env (Importas (p1,id1) (p2,id2) impspec) =
		Importas (p1,just id1) (p2,just id2) (relabel env impspec)

instance Relabel ImpSpec where
  relabel env (NoHiding entities) = NoHiding (map (relabel env) entities)
  relabel env (Hiding entities)   = Hiding (map (relabel env) entities)

instance Relabel Entity where
  relabel env (EntityVar p id)		= EntityVar p (just id)
  relabel env (EntityTyConCls p id)	= EntityTyConCls p (just id)
  relabel env (EntityTyCon p id cons)	= EntityTyCon p (just id)
						(relabelPosIds cons)
  relabel env (EntityTyCls p id vars)	= EntityTyCls p (just id)
						(relabelPosIds vars)

instance Relabel InfixClass where
  relabel env InfixDef = InfixDef
  relabel env InfixL = InfixL
  relabel env InfixR = InfixR
  relabel env Infix  = Infix
  relabel env (InfixPre x) = (InfixPre (just x))

instance Relabel FixId where
  relabel env (FixCon p i) = FixCon p (just i)
  relabel env (FixVar p i) = FixVar p (just i)

instance Relabel Decls where
  relabel env (DeclsParse decls) = DeclsParse (map (relabel env) decls)
  relabel env (DeclsScc deps)    = DeclsScc (map (relabel env) deps)

instance Relabel DeclsDepend where
  relabel env (DeclsNoRec decl) = DeclsNoRec (relabel env decl)
  relabel env (DeclsRec decls)  = DeclsRec (map (relabel env) decls)

instance Relabel Decl where
  relabel env (DeclType s typ)      = DeclType (relabel env s) (relabel env typ)
  relabel env (DeclTypeRenamed p n) = DeclTypeRenamed p n
  relabel env (DeclData mb ctxs simp constrs pis) =
	DeclData mb (map (relabel env) ctxs) (relabel env simp)
		(map (relabel env) constrs) (relabelPosIds pis)
  relabel env (DeclDataPrim p i n) =
	DeclDataPrim p (just i) n
  relabel env (DeclConstrs p i piis) =
	DeclConstrs p (just i) (map (\(p,i1,i2)->(p,just i1,just i2)) piis)
  relabel env (DeclClass p ctxs cls var decls) =
	DeclClass p (map (relabel env) ctxs) (just cls) (just var)
							 (relabel env decls)
  relabel env (DeclInstance p ctxs cls inst decls) =
	DeclInstance p (map (relabel env) ctxs) (just cls) (relabel env inst)
							 (relabel env decls)
  relabel env (DeclDefault typs) =
	DeclDefault (map (relabel env) typs)
  relabel env (DeclPrimitive p i n typ) =
	DeclPrimitive p (letVar env i) n (relabel env typ)
  relabel env (DeclForeignImp p str i1 n fspec typ i2) =
	DeclForeignImp p str (letVar env i1) n fspec (relabel env typ) (just i2)
  relabel env (DeclForeignExp p str id typ) =
	DeclForeignExp p str (just id) (relabel env typ)
  relabel env (DeclVarsType pis ctxs typ) =
	DeclVarsType (relabelPosIds pis) (map (relabel env) ctxs)
							(relabel env typ)
  relabel env (DeclPat alt@(Alt (ExpInfixList p exps) rhs decls)) =
	case infixFun exps of
	    Just (e1,pos,fun,e2) ->
		 relabel env (DeclFun pos fun [Fun [e1,e2] rhs decls])
	    Nothing -> DeclPat (relabel env alt)
  relabel env (DeclPat alt) =
	DeclPat (relabel env alt)
  relabel env (DeclFun p f funs) =
	DeclFun p (letVar env f) (map (relabel env) funs)
  relabel env (DeclIgnore str) = DeclIgnore str
  relabel env (DeclError str) = DeclError str
  relabel env (DeclAnnot decl annots) =
	DeclAnnot (relabel env decl) (map (relabel env) annots)
  relabel env (DeclFixity (fixclass,n,fixids)) =
	DeclFixity (relabel env fixclass, n, map (relabel env) fixids)

instance Relabel Annot where
  relabel env (AnnotArity (p,id) n)        = AnnotArity (p,just id) n
  relabel env (AnnotPrimitive (p,id) pstr) = AnnotPrimitive (p,just id) pstr
  relabel env (AnnotNeed idss)             = AnnotNeed (map (map just) idss)
  relabel env (AnnotUnknown)               = AnnotUnknown

instance Relabel Rhs where
  relabel env (Unguarded exp)  = Unguarded (relabel env exp)
  relabel env (Guarded gdexps) =
	Guarded (map (\(gd,exp)-> (relabel env gd, relabel env exp)) gdexps)

instance Relabel Type where
  relabel env (TypeCons p c typs) = TypeCons p (just c) (map (relabel env) typs)
  relabel env (TypeApp t1 t2)     = TypeApp (relabel env t1) (relabel env t2)
  relabel env (TypeVar p v)       = TypeVar p (just v)
  relabel env (TypeStrict p t)    = TypeStrict p (relabel env t)

instance Relabel Sig where
  relabel env (Sig pis typ)       = Sig (relabelPosIds pis) (relabel env typ)

instance Relabel Simple where
  relabel env (Simple p id pis)   = Simple p (just id) (relabelPosIds pis)

instance Relabel Context where
  relabel env (Context p id (p2,id2)) = Context p (just id) (p2, just id2)

instance Relabel Constr where
  relabel env (Constr p id mbs) =
	Constr p (just id) (map locust mbs)
		where locust (Nothing,typ) = (Nothing,relabel env typ)
		      locust (Just pis,typ) = (Just (relabelPosIds pis)
						,relabel env typ)
  relabel env (ConstrCtx fvs ctxs p id mbs) =
	ConstrCtx (relabelPosIds fvs) (map (relabel env) ctxs) p
						 (just id) (map locust mbs)
		where locust (Nothing,typ) = (Nothing,relabel env typ)
		      locust (Just pis,typ) = (Just (relabelPosIds pis)
						,relabel env typ)

instance Relabel Field where
  relabel env (FieldExp p id exp) = FieldExp p (just id) (relabel env exp)
  relabel env (FieldPun p id)     = FieldPun p (just id)

instance Relabel Stmt where
  relabel env (StmtExp exp)      = StmtExp (relabel env exp)
  relabel env (StmtBind pat exp) = StmtBind (relabel env pat) (relabel env exp)
  relabel env (StmtLet decls)    = StmtLet (relabel env decls)

instance Relabel Qual where
  relabel env (QualPatExp pat exp) = 
	QualPatExp (relabel env pat) (relabel env exp)
  relabel env (QualExp exp)   =  QualExp (relabel env exp)
  relabel env (QualLet decls) =  QualLet (relabel env decls)



-- This is where the non-trivial syntax types start.  Local defns (let-bound)
-- and lhs patterns (lambda-bound) must be added to the environment before
-- relabeling the rhs of a function or binding.

instance Relabel Fun where
  relabel env (Fun pats rhs ds@(DeclsParse decls)) =
	let newEnv = foldr addPat (extendEnv vi im env decls) pats in
	Fun (map (relabel newEnv) pats) (relabel newEnv rhs) (relabel newEnv ds)

instance Relabel Alt where
  relabel env (Alt pat rhs ds@(DeclsParse decls)) =
	let newEnv = addPat pat (extendEnv vi im env decls) in
	Alt (relabel newEnv pat) (relabel newEnv rhs) (relabel newEnv ds)

instance Relabel Exp where
  relabel env (ExpScc str exp)  = ExpScc str (relabel env exp)
  relabel env (ExpDict exp)     = ExpDict (relabel env exp)
  relabel env (ExpFatbar e1 e2) = ExpFatbar (relabel env e1) (relabel env e2)
  relabel env (ExpFail)         = ExpFail
  relabel env (ExpLambda p pats exp) =
	let newEnv = foldr addPat env pats in
	ExpLambda p (map (relabel newEnv) pats) (relabel newEnv exp)
  relabel env (ExpLet p ds@(DeclsParse decls) exp) =
	let newEnv = extendEnv vi im env decls in
	ExpLet p (relabel newEnv ds) (relabel newEnv exp)
  relabel env (ExpDo p stmts) =
	ExpDo p (doStmts env stmts)
    where doStmts env [] = []
	  doStmts env (s@(StmtExp _):ss) = relabel env s: doStmts env ss
	  doStmts env (s@(StmtBind pat _):ss) =
		let newEnv = addPat pat env in
		relabel newEnv s: doStmts newEnv ss
	  doStmts env (s@(StmtLet (DeclsParse decls)):ss) =
		let newEnv = extendEnv vi im env decls in
		relabel newEnv s: doStmts newEnv ss
  relabel env (ExpCase p exp alts) =
	ExpCase p (relabel env exp) (map (relabel env) alts)
  relabel env (ExpIf p cond thn els) =
	ExpIf p (relabel env cond) (relabel env thn) (relabel env els)
  relabel env (ExpType p exp ctxs typ) =
	ExpType p (relabel env exp) (map (relabel env) ctxs) (relabel env typ)
  relabel env (ExpRecord exp fields) =
	ExpRecord (relabel env exp) (map (relabel env) fields)
  relabel env (ExpApplication p exps) =
	ExpApplication p (map (relabel env) exps)
  relabel env (ExpVar p id)   = ExpVar p (lookEnv env mkVar id)
  relabel env (ExpCon p id)   = ExpCon p (lookEnv env mkCon id)
  relabel env (ExpVarOp p id) = ExpVarOp p (lookEnv env mkVar id)
  relabel env (ExpConOp p id) = ExpConOp p (lookEnv env mkCon id)
  relabel env (ExpInfixList p exps) =
	relabel env (fixInfixList env exps)
  relabel env (ExpLit p lit)   = ExpLit p lit
  relabel env (ExpList p exps) = ExpList p (map (relabel env) exps)
  relabel env (PatAs p id pat) =
	PatAs p (lookEnv env mkVar id) (relabel env pat)
  relabel env (PatWildcard p)  = PatWildcard p
  relabel env (PatIrrefutable p pat) = PatIrrefutable p (relabel env pat)
  relabel env (PatNplusK p id1 id2 exp1 exp2 exp3) =	-- *** No, No, NO
	PatNplusK p (just id1) (just id2)
		 (relabel env exp1) (relabel env exp2) (relabel env exp3)




-- `relabelPosIds' does the simplest possible renaming of a list of
-- position/id pairs from the TokenId type to TraceId type.
relabelPosIds :: [(Pos,TokenId)] -> [(Pos,TraceId)]
relabelPosIds = map (\(p,i)->(p,just i))


-- `addPat' extends the environment with a lambda-bound variable
-- (e.g. pattern)
--
addPat :: Pat TokenId -> AuxTree -> AuxTree
addPat (ExpRecord (ExpCon p id) fields) env = foldr addField env fields
addPat (ExpRecord (ExpVar p id) fields) env = foldr addField
						(extendEnvPat id env) fields
addPat (ExpApplication p exps) env = foldr addPat env exps
addPat (ExpVar p id) env           = extendEnvPat id env
addPat (ExpCon p id) env           = env
addPat (ExpInfixList p exps) env   = foldr addPat env exps
addPat (ExpVarOp p id) env         = extendEnvPat id env
addPat (ExpConOp p id) env         = env
addPat (ExpList p exps) env        = foldr addPat env exps
addPat (PatAs p id pat) env        = addPat pat (extendEnvPat id env)
addPat (PatIrrefutable p pat) env  = addPat pat env
addPat (PatNplusK p id1 id2 exp1 exp2 exp3) env = env	-- not correct
addPat _ env = env

addField (FieldExp p id exp) env = addPat exp env
addField (FieldPun p id) env     = extendEnvPat id env

extendEnvPat id env =
    addAT env lambdaBound (Var (show id)) (emptyAux {letBound=False})
  where
    lambdaBound aux1 aux2 = aux2 { letBound=False }

