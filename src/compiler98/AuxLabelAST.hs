module AuxLabelAST
  ( auxLabelSyntaxTree
  ) where


import List (nubBy)
import Char (isUpper)
import Maybe (isJust)
import AuxTypes(Environment,mkIdentMap,useIdentMap,Identifier(..)
               ,AuxiliaryInfo(letBound))
import AuxFile(Visibility,PatSort(Refutable,Irrefutable)
              ,extendEnv,getImports,addPat,foldrPat)
import AuxFixity(fixInfixList)
import TraceId
import Flags(Flags)
import Syntax
import SyntaxUtil (infixFun)
import TokenId (TokenId(..))
import AssocTree
import Extra (Pos,noPos)


-- `auxLabelSyntaxTree' relabels the entire abstract syntax tree,
-- replacing TokenId with TraceId (which basically adds arity and
-- binding information for each variable name).  Along the way, we
-- also need to resolve the fixity of all applications (originally
-- done in a later phase of the compiler).

auxLabelSyntaxTree :: Flags -> Module TokenId -> IO (Module TraceId)
auxLabelSyntaxTree flags
	mod@(Module p modId exports imports fixdecls (DeclsParse decls)) =
  do
    let (identMap,_) = mkIdentMap decls
    let (irrefutableIds,localenv) = 
          extendEnv vi (initAT,identMap) (map DeclFixity fixdecls ++ decls)
    totalenv <- getImports (\_->vi) localenv flags imports
--  let env = reorderAT stripType totalenv
    let Module p modId exports imports fixdecls decls = relabel totalenv mod
    return $ Module p modId exports imports fixdecls 
               (addVars totalenv irrefutableIds decls)
--where
--  stripType env (Con _ con, aux)    = addAT env const (Con "" con) aux
--  stripType env (Method _ met, aux) = addAT env const (Var met) aux
--  stripType env (Field _ f, aux)    = addAT env const (Var f)   aux
--  stripType env (v@(Var _), aux)    = addAT env const v aux


-- `vi' is a degenerate visibility function.  When writing the .hx file,
-- visibility in the export list is important, but when relabeling the
-- syntax tree, everything is visible.
vi :: Visibility
vi = \_-> True


-- `letVar' and `lookEnv' build a TraceId for any TokenId by
-- looking up the arity information in the environment.  For letVar,
-- the distinction between let-bound and lambda-bound variables
-- is decided by the caller, but for `lookEnv', it is decided by the
-- environment.

-- The following hack is needed, because identMap only knows about global
-- identifiers and because variables, field labels and methods share the
-- same name space.
useEnvironment :: Environment -> TokenId -> Maybe AuxiliaryInfo
useEnvironment (env,identMap) id =
  let v = useIdentMap identMap id in
  case v of
    -- a field or method may be shadowed by a normal variable
    -- hence have to search for such a variable first
    Field _ name -> case lookupAT env (Var name) of
                      Just info -> Just info
                      Nothing -> lookupAT env v
    Method _ name -> case lookupAT env (Var name) of
                       Just info -> Just info
                       Nothing -> lookupAT env v
    _ -> lookupAT env v

letVar :: Environment -> TokenId -> TraceId
letVar env id =
  case useEnvironment env id of
    Just info | letBound info -> id `plus` info
    _ -> error ("AuxLabelAST.letVar: "++show id++" not let-bound in env")

lookEnv :: Environment -> TokenId -> TraceId
lookEnv env id =
  case useEnvironment env id of
    Just info -> id `plus` info
    _ -> case id of
           -- this is a horrible hack to by-pass qualified names.
           Qualified _ x -> case useEnvironment env (Visible x) of
                              Just info -> id `plus` info
                              _ -> stop
           _ -> stop
  where
  stop = error ("Variable or constructor not in scope: " ++ show id)

lookupTyCls :: Environment -> TokenId -> TraceId
lookupTyCls (auxTree,_) id =
  case lookupAT auxTree (TypeClass (show id)) of
    Just info -> id `plus` info
    _ -> error ("Type or class not in scope: " ++ show id)

-- The class `Relabel' walks the abstract syntax tree, relabelling all
-- TokenId to TraceId.  Most instances are pretty trivial - the only
-- interesting ones are at the end of this section.

class Relabel g where
  relabel :: Environment -> g TokenId -> g TraceId

instance Relabel Module where
  relabel env (Module p mod Nothing imps fixs decls) =
    Module p (mkLambdaBound mod) Nothing (map (relabel env) imps) [] (relabel env decls)
  relabel env (Module p mod (Just exps) imps fixs decls) =
    Module p (mkLambdaBound mod) (Just (map (relabel env) exps))
			(map (relabel env) imps) [] (relabel env decls)
					-- fixdecls are folded into the decls

instance Relabel Export where
  relabel env (ExportEntity pos entity) = ExportEntity pos (relabel env entity)
  relabel env (ExportModid pos id)      = ExportModid pos (mkLambdaBound id)

instance Relabel ImpDecl where
  relabel env (Import (pos,id) impspec) =
		Import (pos,mkLambdaBound id) (relabel env impspec)
  relabel env (ImportQ (pos,id) impspec) =
		ImportQ (pos,mkLambdaBound id) (relabel env impspec)
  relabel env (ImportQas (p1,id1) (p2,id2) impspec) =
		ImportQas (p1,mkLambdaBound id1) (p2,mkLambdaBound id2) (relabel env impspec)
  relabel env (Importas (p1,id1) (p2,id2) impspec) =
		Importas (p1,mkLambdaBound id1) (p2,mkLambdaBound id2) (relabel env impspec)

instance Relabel ImpSpec where
  relabel env (NoHiding entities) = NoHiding (map (relabel env) entities)
  relabel env (Hiding entities)   = Hiding (map (relabel env) entities)

instance Relabel Entity where
  relabel env (EntityVar p id)	     = EntityVar p (mkLambdaBound id)
  relabel env (EntityConClsAll p id) = EntityConClsAll p (lookupTyCls env id)
  relabel env (EntityConClsSome p id cons) = EntityConClsSome p
						(lookupTyCls env id)
						(relabelPosIds cons)

instance Relabel InfixClass where
  relabel env InfixDef = InfixDef
  relabel env InfixL = InfixL
  relabel env InfixR = InfixR
  relabel env Infix  = Infix
  relabel env (InfixPre x) = (InfixPre (mkLambdaBound x))

instance Relabel FixId where
  relabel env (FixCon p i) = FixCon p (lookEnv env i)
  relabel env (FixVar p i) = FixVar p (lookEnv env i)

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
	DeclDataPrim p (mkLambdaBound i) n
  relabel env (DeclConstrs p i piis) =
	DeclConstrs p (mkLambdaBound i) (map (\(p,i1,i2)->(p,mkLambdaBound i1,mkLambdaBound i2)) piis)
  relabel env (DeclClass p ctxs cls var decls) =
	DeclClass p (map (relabel env) ctxs) (mkLambdaBound cls) (mkLambdaBound var)
							 (relabel env decls)
  relabel env (DeclInstance p ctxs cls inst decls) =
	DeclInstance p (map (relabel env) ctxs) (mkLambdaBound cls) (relabel env inst)
							 (relabel env decls)
  relabel env (DeclDefault typs) =
	DeclDefault (map (relabel env) typs)
  relabel env (DeclPrimitive p i n typ) =
	DeclPrimitive p (letVar env i) n (relabel env typ)
  relabel env (DeclForeignImp p callConv str i1 n fspec typ i2) =
	DeclForeignImp p callConv str (letVar env i1) n fspec (relabel env typ)
          (mkLambdaBound i2)
  relabel env (DeclForeignExp p callConv str id typ) =
	DeclForeignExp p callConv str (mkLambdaBound id) (relabel env typ)
  relabel env (DeclVarsType pis ctxs typ) =
	DeclVarsType (relabelRealPosIds env pis) (map (relabel env) ctxs)
							(relabel env typ)
  relabel env (DeclPat (Alt (ExpInfixList p exps) rhs decls))
    | isJust infixFunExps 
    = relabel env (DeclFun pos fun [Fun [e1,e2] rhs decls])
    where
    infixFunExps = infixFun exps
    Just (e1,pos,fun,e2) = infixFunExps
  relabel env (DeclPat (Alt pat rhs ds@(DeclsParse decls))) =
    let (irrefutableDeclIds,newEnv) = extendEnv vi env decls
    in  DeclPat 
          (Alt (relabel newEnv pat) (relabel newEnv rhs) 
            (addVars newEnv (nubBy (\x y -> show x == show y) -- for scope 
              irrefutableDeclIds) 
              (relabel newEnv ds)))
          -- dont' use relable Alt, because that extends environment also
          -- by variables in the pattern
  relabel env (DeclFun p f funs) =
	DeclFun p 
          -- ensure that defined id always with arity, even in class/instance
          (modArity (letVar env f) (funArity . head $ funs))
          (map (relabel env) funs)
  relabel env (DeclIgnore str) = DeclIgnore str
  relabel env (DeclError str) = DeclError str
  relabel env (DeclAnnot decl annots) =
	DeclAnnot (relabel env decl) (map (relabel env) annots)
  relabel env (DeclFixity (fixclass,n,fixids)) =
	DeclFixity (relabel env fixclass, n, map (relabel env) fixids)

instance Relabel Annot where
  relabel env (AnnotArity (p,id) n)        = AnnotArity (p,mkLambdaBound id) n
  relabel env (AnnotPrimitive (p,id) pstr) = AnnotPrimitive (p,mkLambdaBound id) pstr
  relabel env (AnnotNeed idss)             = AnnotNeed (map (map mkLambdaBound) idss)
  relabel env (AnnotUnknown)               = AnnotUnknown

instance Relabel Rhs where
  relabel env (Unguarded exp)  = Unguarded (relabel env exp)
  relabel env (Guarded gdexps) =
	Guarded (map (\(gd,exp)-> (relabel env gd, relabel env exp)) gdexps)

instance Relabel Type where
  relabel env (TypeCons p c typs) = TypeCons p (mkLambdaBound c) (map (relabel env) typs)
  relabel env (TypeApp t1 t2)     = TypeApp (relabel env t1) (relabel env t2)
  relabel env (TypeVar p v)       = TypeVar p (mkLambdaBound v)
  relabel env (TypeStrict p t)    = TypeStrict p (relabel env t)

instance Relabel Sig where
  relabel env (Sig pis typ)       = Sig (relabelPosIds pis) (relabel env typ)

instance Relabel Simple where
  relabel env (Simple p id pis)   = Simple p (mkLambdaBound id) (relabelPosIds pis)

instance Relabel Context where
  relabel env (Context p id (p2,id2)) = Context p (mkLambdaBound id) (p2, mkLambdaBound id2)

instance Relabel Constr where
  relabel env (Constr p id mbs) =
	Constr p (lookEnv env id) (map locust mbs)
		where locust (Nothing,typ) = (Nothing,relabel env typ)
		      locust (Just pis,typ) = (Just (relabelPosIds pis)
						,relabel env typ)
  relabel env (ConstrCtx fvs ctxs p id mbs) =
	ConstrCtx (relabelPosIds fvs) (map (relabel env) ctxs) p
		  (lookEnv env id) (map locust mbs)
		where locust (Nothing,typ) = (Nothing,relabel env typ)
		      locust (Just pis,typ) = (Just (relabelPosIds pis)
						,relabel env typ)

instance Relabel Field where
  relabel env (FieldExp p id exp) = FieldExp p (mkLambdaBound id)
                                               (relabel env exp)
  relabel env (FieldPun p id)     = FieldPun p (mkLambdaBound id)

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
    let (irrefutableDeclIds,declEnv) = extendEnv vi env decls
        (irrefutableIds,newEnv) = 
          foldrPat (addPat Refutable vi) declEnv pats 
    in
      Fun (map (relabel newEnv) pats) (relabel newEnv rhs) 
        (addVars newEnv 
          (nubBy (\x y -> show x == show y) -- for shadowing of scopes
            (irrefutableDeclIds++irrefutableIds)) 
          (relabel newEnv ds))

instance Relabel Alt where
  -- only uses for Alts in cases, not in pattern bindings
  relabel env (Alt pat rhs ds@(DeclsParse decls)) =
    let (irrefutableDeclIds,declEnv) = extendEnv vi env decls
        (irrefutableIds,newEnv) = 
          addPat Refutable vi pat declEnv
    in
      Alt (relabel newEnv pat) (relabel newEnv rhs) 
        (addVars newEnv (nubBy (\x y -> show x == show y) -- for scope 
          (irrefutableDeclIds++irrefutableIds)) 
        (relabel newEnv ds))

instance Relabel Exp where
  relabel env (ExpScc str exp)  = ExpScc str (relabel env exp)
  relabel env (ExpDict exp)     = ExpDict (relabel env exp)
  relabel env (ExpFatbar e1 e2) = ExpFatbar (relabel env e1) (relabel env e2)
  relabel env (ExpFail)         = ExpFail
  relabel env (ExpLambda p pats exp) =
	let (irrefutableIds,newEnv) = 
              foldrPat (addPat Refutable vi) env pats in
	ExpLambda p (map (relabel newEnv) pats) 
          ((if null irrefutableIds 
             then id else ExpLet p (addVars newEnv irrefutableIds noDecls))
            (relabel newEnv exp))
  relabel env (ExpLet p ds@(DeclsParse decls) exp) =
	let (irrefutableIds,newEnv) = extendEnv vi env decls in
	ExpLet p (addVars newEnv irrefutableIds (relabel newEnv ds)) 
          (relabel newEnv exp)
  relabel env (ExpDo p stmts) =
	ExpDo p (doStmts env stmts)
    where doStmts env [] = []
	  doStmts env (s@(StmtExp _):ss) = relabel env s: doStmts env ss
	  doStmts env (s@(StmtBind pat _):ss) =
		let (irrefutableIds,newEnv) = addPat Refutable vi pat env in
		relabel newEnv s : 
                (if null irrefutableIds 
                   then id 
                   else (StmtLet (addVars newEnv irrefutableIds noDecls):))
                  (doStmts newEnv ss)
	  doStmts env (s@(StmtLet (DeclsParse decls)):ss) =
		let (irrefutableIds,newEnv) = extendEnv vi env decls 
                    StmtLet reDecls = relabel newEnv s
                in (StmtLet (addVars newEnv irrefutableIds reDecls) 
                   : doStmts newEnv ss)
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
  relabel env (ExpVar p id) = ExpVar p (lookEnv env id)
  relabel env (ExpCon p id)   = ExpCon p (lookEnv env id)
  relabel env (ExpVarOp p id) = ExpVarOp p (lookEnv env id)
  relabel env (ExpConOp p id) = ExpConOp p (lookEnv env id)
  relabel env (ExpInfixList p exps) = relabel env (fixInfixList env exps)
  relabel env (ExpLit p lit)   = ExpLit p lit
  relabel env (ExpList p exps) = ExpList p (map (relabel env) exps)
  relabel env (PatAs p id pat) =
	PatAs p (lookEnv env id) (relabel env pat)
  relabel env (PatWildcard p)  = PatWildcard p
  relabel env (PatIrrefutable p pat) = PatIrrefutable p (relabel env pat)
  relabel env (PatNplusK p id1 id2 exp1 exp2 exp3) =	-- *** No, No, NO
	PatNplusK p (mkLambdaBound id1) (mkLambdaBound id2)
		 (relabel env exp1) (relabel env exp2) (relabel env exp3)




-- `relabelPosIds' does the simplest possible renaming of a list of
-- position/id pairs from the TokenId type to TraceId type.
relabelPosIds :: [(Pos,TokenId)] -> [(Pos,TraceId)]
relabelPosIds = map (\(p,i)->(p,mkLambdaBound i))

-- `relabelRealPosIds' does the correct (as opposed to simplest) renaming
-- of a list of position/id pairs from the TokenId type to TraceId type.
relabelRealPosIds :: Environment -> [(Pos,TokenId)] -> [(Pos,TraceId)]
relabelRealPosIds env = map (\(p,i)->(p,lookEnv env i))

-- Create definitions for let-bound variants of irrefutable variables.
addVars :: Environment -> [TokenId] -> Decls TraceId -> Decls TraceId
addVars env irrefutableIds (DeclsParse ds) =
  DeclsParse (map mkDef irrefutableIds ++ ds)
  where
  mkDef id = DeclFun noPos traceId
               [Fun [] 
                 (Unguarded (ExpVar noPos (modLambdaBound traceId))) noDecls]
    where  
    traceId = lookEnv env id
