{- ---------------------------------------------------------------------------
Parser for Haskell 98 Syntax
-}
module Parse(parseProg) where

import Extra(pair,triple,noPos,Pos(..),isJust,strace,strPos)
import Lex
import Lexical(PosTokenPre(..),LexState(..),PosToken(..))
import Syntax
import MkSyntax	( mkAppExp, mkAppInst, mkCase, mkDeclClass
	, mkDeclFun, mkDeclPat, mkDeclPatFun, mkEnumFrom
	, mkEnumThenFrom, mkEnumToFrom, mkEnumToThenFrom
	, mkExpListComp, mkIf, mkInfixList
	, mkInstList, mkInt, mkParExp, mkParInst, mkParType
	, mkTypeList, mkPatNplusK -- , mkParLhs
	)
import Parse2
import ParseLib
import ParseLex
import SyntaxPos
import TokenId (t_nplusk,t_Arrow)

optSemi = () `parseChk` semi
                `orelse`
           parse ()


parseProg :: Parser (Module TokenId) [PosToken] a
parseProg = {- many parsePragma `revChk` -} (parseModule `chkCut` eof)


parseModule :: Parser (Module TokenId) [PosToken] a
parseModule =
    (uncurry Module) `parseChk` lit L_module `apCut` bigModId `ap` parseExports
                 `chk` lit L_where `chk` lcurl
                         `apCut` parseImpDecls
                         `apCut` parseFixDecls
                         `apCut` parseTopDecls
                `chk` optSemi `chk` rcurl


parseTopDecls :: Parser (Decls TokenId) [PosToken] a
parseTopDecls =
  semi `revChk` parseTopDecls
    `orelse`
  DeclsParse `parseAp` manysSep semi parseTopDecl


parseTopDecl :: Parser (Decl TokenId) [PosToken] a
parseTopDecl =
  cases [
  (L_type, \pos -> DeclType `parseAp` parseSimple `chk` equal `ap` parseType),
  (L_newtype, \pos -> 
    DeclData Nothing `parseAp` parseContexts `ap` parseSimple `chk` 
      equal `apCut` ( (:[]) `parseAp` parseConstr) `apCut` parseDeriving),
  (L_data, \pos -> 
      (\ (pos,conid) size -> DeclDataPrim pos conid size) `parseChk` 
        k_primitive `ap` conid `chk` equal `apCut` intPrim
    `orelse`
      (DeclData . Just) `parseAp` unboxed `ap` parseContexts `ap` 
        parseSimple `chk` equal `apCut`
        someSep pipe parseConstr `apCut` parseDeriving
    `orelse`
      (\ simpl -> DeclData (Just False) [] simpl [] []) `parseAp` parseSimple ),
  (L_class, \pos -> 
    mkDeclClass `parseAp` parseContexts `ap` aconid `ap` avarid `ap` 
      (id `parseChk` lit L_where `chk` lcurl `ap` parseCDecls `chk` rcurl
       `orelse` 
       parse (DeclsParse [])
      )), 
  (L_instance, \pos->  
    (\ctx (pos',cls) -> DeclInstance pos' ctx cls) `parseAp` 
      parseContexts `ap` aconid `ap` parseInst `ap` 
      (lit L_where `revChk` lcurl `revChk` parseValdefs `chk` rcurl
       `orelse`
       parse (DeclsParse [])
      )),
  (L_default, \pos -> 
    DeclDefault `parseChk` lpar `apCut` 
      manySep comma parseType `chk` rpar
    `orelse`
    (\x->DeclDefault [x]) `parseAp` parseType)
  ]
  (uncurry DeclPrimitive `parseAp` varid `chk` k_primitive `apCut` 
     intPrim `chk` coloncolon `ap` parseType
   `orelse`
   parseForeign
   `orelse`
   parseDecl)


parseSig :: Parser (Sig TokenId) [PosToken] a 
parseSig = Sig `parseAp` someSep comma varid `chk` coloncolon `ap`  
  parseStrict parseType


parseForeign :: Parser (Decl TokenId) [PosToken] a
parseForeign =
  k_foreign `revChk`
    ((k_import `revChk` 
        ((\(_,conv) (_,tf) (_,LitString _ str) (p,v) t -> 
            DeclForeignImp p conv str v (calcArity t) tf t v)
        `parseAp` callconv `ap` safety `ap` entity `ap` varid `chk` 
        coloncolon `apCut` parseType))
    `orelse`
    (k_export `revChk` callconv `revChk`
      ((\(_,conv) (_,LitString _ str) (p,v) t-> DeclForeignExp p conv str v t)
      `parseAp` callconv `ap` entity `apCut` varid `chk` coloncolon 
      `ap` parseType))
    `orelse`
    (k_import `revChk`	-- old syntax, will be removed in the future
        ((\(_,conv) (_,LitString _ str) (_,tf) (p,v) t -> 
            strace ("Deprecated FFI syntax used at "++strPos p) $
            DeclForeignImp p conv str v (calcArity t) tf t v)
        `parseAp` (callconv `orelse` conv C) `ap` entity `ap`
        safety `apCut` varid `chk` coloncolon `ap` parseType))
 -- `orelse`
 --   (k_cast `revChk` 
 --     ((\(p,v) t-> DeclForeignImp p Cast "" v (calcArity t) Safe t v)
 --     `parseAp` varid `chk` coloncolon `ap` parseType))
     )
  where
  callconv = (k_ccall `revChk` conv C)
               `orelse` 
             (k_cast `revChk` conv Cast)
               `orelse`
             (k_noproto `revChk` conv Noproto)
               `orelse`
             (k_haskellcall `revChk` conv Haskell)
               `orelse` 
             (k_stdcall `revChk` conv (Other "stdcall"))
               `orelse` 
             (k_cplusplus `revChk` conv (Other "cplusplus"))
               `orelse` 
             (k_dotnet `revChk` conv (Other "dotnet"))
               `orelse` 
             (k_jvm `revChk` conv (Other "jvm"))
          --   `orelse` 
          -- (conv C)  -- previously the default, now the name is mandatory
  entity   = string `orelse` parse (noPos, LitString UnBoxed "")
  safety   = (k_unsafe `revChk` conv Unsafe)
               `orelse`
             (k_safe `revChk` conv Safe)
               `orelse`
             (k_threadsafe `revChk` conv ThreadSafe)
               `orelse`
             (parse noPos `revChk` conv Safe)	-- default is Safe
  conv tf   = parse (noPos,tf)
  calcArity (TypeCons p c ts) | c == t_Arrow  = 1 + calcArity (ts!!1)
  calcArity _                 | otherwise     = 0


parseVarsType :: Parser (Decl TokenId) [PosToken] a
parseVarsType =
  DeclVarsType `parseAp` someSep comma varid `chk` coloncolon `ap` 
    parseContexts `ap` parseType

{-
parseNewConstr =
    (\ (pos,op) a ->  [Constr pos op [(Nothing,a)]]) `parseAp` conid `ap` parseInst
-}


-- parseCSigns = DeclsParse `parseAp` manySep semi parseCSign
-- parseCSign = parseVarsType

parseCDecls :: Parser (Decls TokenId) [PosToken] a 
parseCDecls = DeclsParse `parseAp` (manysSep semi parseCDecl)	-- H98 added

parseCDecl :: Parser (Decl TokenId) [PosToken] a
parseCDecl = parseVarsType `orelse` parseValdef
		 `orelse` (DeclFixity `parseAp` parseFixDecl)


parseValdefs :: Parser (Decls TokenId) [PosToken] a
parseValdefs =
  semi `revChk` parseValdefs
  `orelse`
  DeclsParse `parseAp` manysSep semi parseValdef


parseValdef :: Parser (Decl TokenId) [PosToken] a
parseValdef =
 mkDeclPat `parseAp` varid `ap` anyop `ap` parsePat `ap` 
   parseRhs equal `apCut` parseWhere
 `orelse` 
 mkDeclFun `parseAp` varid `ap` parsePats `ap` parseRhs equal `apCut` 
   parseWhere
 `orelse` 
 mkDeclPatFun `parseAp` parseAlt equal


parseWhere :: Parser (Decls TokenId) [PosToken] a
parseWhere =
    lit L_where `revChk` lcurl `revChk` parseDecls `chk` rcurl
        `orelse`
    parse (DeclsParse [])


parseDecls = DeclsParse `parseAp` (manysSep semi parseDecl)

parseDecl =
    parseVarsType
        `orelse`
    parseValdef
        `orelse`			-- added in H98
    DeclFixity `parseAp` parseFixDecl	-- added in H98
 {-	`orelse`
    parsePragma				-- added by MW, Sept 2000
  -}


parseExp :: Parser (Exp TokenId) [PosToken] a
parseExp =
    parseExp0 `revAp` parseExpType

parseExpType =
      (\pos ctx t e-> ExpType pos e ctx t) `parseAp` coloncolon `apCut`
              parseContexts `ap` parseType
        `orelse`
      parse id

parseExp0 = mkInfixList `parseAp` some (anyop `orelse` parseExp10)

parseStmt =
   (lit L_let `into` \_-> lcurl `into` \_-> parseDecls `into`
                 \decls-> rcurl `into` \_->
	((lit L_in `into` \_-> parseExp `into`
                  \exp-> parse (StmtExp (ExpLet (getPos decls) decls exp)))
	    `orelse`
	  parse (StmtLet decls)))
	`orelse`
   StmtBind `parseAp` parsePat `chk` larrow `apCut` parseExp
	`orelse`
   StmtExp `parseAp` parseExp

parseExp10 =
    cases 
        [(L_Lambda,\pos -> (ExpLambda pos) `parseAp` parsePats
                                           `chk` rarrow `apCut` parseExp),
         (L_let,   \pos -> (ExpLet pos) `parseChk` lcurl `ap` parseDecls 
                                        `chk` optSemi `chk` rcurl
                                        `chk` lit L_in `ap` parseExp),
         (L_do,   \pos -> (ExpDo pos) `parseChk` lcurl
                                        `ap` somesSep semi parseStmt
                                    `chk` optSemi `chk` rcurl),
         (L_if,    \pos -> (mkIf pos) `parseAp` parseExp 
                              `chk` lit L_then `ap` parseExp
                              `chk` lit L_else `ap` parseExp),
         (L_case,  \pos -> (mkCase pos) `parseAp` parseExp `chk` lit L_of
                                        `chk` lcurl
                                        `ap` (somesSep semi (parseAlt rarrow)) 
                                        `chk` optSemi `chk` rcurl)]
         parseFExp

parseFExp  = mkAppExp `parseAp` some parseAExpR1

parseAExpR1 =
  parseAExp `into` parseAExpR

parseAExpR exp = 
   (ExpRecord exp `parseChk` lcurl `ap` manySep comma parseFieldExp `chk` rcurl)            `into` parseAExpR
	`orelse`
   parse exp

parseAExp = 
    aanyid
        `orelse`
    cases 
         [(L_LBRACK, \pos -> parseBrackExp0 pos),
          (L_LPAR,   \pos -> (mkParExp pos) `parseAp` manySep comma parseExp
                                            `chk` rpar)]
    (uncurry ExpLit `parseAp`
            (integer `orelse` rational `orelse` char `orelse` string))


parseFieldExp =
    varid `into` (\(pos,ident)-> (FieldExp pos ident `parseChk` equal
                                                     `ap` parseExp)
					`orelse`		-- H98 removes
				 parse (FieldPun pos ident)	-- H98 removes
                 )

parseBrackExp0 pos =                -- found '['
    (ExpList pos []) `parseChk` rbrack
        `orelse`
    parseExp `revAp` parseBrackExp1 pos

parseBrackExp1 pos =                -- found '[e'
    (\e -> ExpList pos [e]) `parseChk` rbrack
        `orelse`
    mkEnumFrom pos `parseChk` dotdot `chk` rbrack
        `orelse`
    mkExpListComp pos `parseChk` pipe `ap` somesSep comma parseQual `chk` rbrack
        `orelse`
    mkEnumToFrom pos `parseChk` dotdot `ap` parseExp `chk` rbrack
        `orelse`
    comma `revChk` (parseExp `revAp` parseBrackExp2 pos)

parseBrackExp2 pos =                -- found '[e,e'
    (\e2 e1 -> ExpList pos [e1,e2]) `parseChk` rbrack
        `orelse`
    mkEnumThenFrom pos `parseChk` dotdot `chk` rbrack
        `orelse`
    mkEnumToThenFrom pos `parseChk` dotdot `ap` parseExp `chk` rbrack
        `orelse`
    (\es e2 e1 -> ExpList pos (e1:e2:es)) `parseChk` comma
                                          `ap` manySep comma parseExp
                                          `chk` rbrack

parseQual =
   (lit L_let `into` \_-> lcurl `into` \_-> parseDecls `into`
                 \decls-> rcurl `into` \ _ ->
	((lit L_in `into` \_-> parseExp `into` \exp->
                        parse (QualExp (ExpLet (getPos decls) decls exp)))
		`orelse`
	 parse (QualLet decls)))
	`orelse`
    QualPatExp `parseAp` parsePat `chk` larrow `apCut` parseExp
        `orelse`
    QualExp `parseAp` parseExp

parseAlt del =
    Alt `parseAp` parsePat `ap` parseRhs del `apCut` parseWhere


parseRhs :: Parser Pos [PosToken] a -> Parser (Rhs TokenId) [PosToken] a
parseRhs del =
    Unguarded `parseChk` del `apCut` parseExp
        `orelse`
    Guarded `parseAp` some (parseGdExp del)


parseGdExp :: Parser Pos [PosToken] a  
              -> Parser (Exp TokenId, Exp TokenId) [PosToken] a
parseGdExp del =
    pair `parseChk` pipe `apCut` parseExp `chk` del `apCut` parseExp


parsePats = some parseAPat


-- Pat can not contain cut! It brakes parseStmt if it does.

manySafe p = someSafe p `orelse` parse []
someSafe p = (:) `parseAp` p `ap` manySafe p

manySepSafe' s p = s `revChk` someSepSafe s p
                 `orelse`
               parse []
manySepSafe s p = someSepSafe s p `orelse` parse []
someSepSafe s p = (:) `parseAp` p `apCut` manySepSafe' s p

parsePat =
    mkPatNplusK `parseAp` varid `chk` literal (L_AVAROP t_nplusk) `ap` integer
        `orelse`
    parsePat0

parsePat0 = mkInfixList `parseAp` someSafe (parseOpPat `orelse` parseFPat)

parseOpPat = anyop

parseFPat =
    (\(pos,c) args -> ExpApplication pos (ExpCon pos c:args))
                                          `parseAp` conid `ap` some parseAPat
        `orelse`
    parseAPat

parseAPat = parseAPat2 `into` parseAPat1

parseFieldPat =
    varid `into` (\(pos,ident)-> FieldExp pos ident `parseChk` equal
                                                    `ap` parsePat
					`orelse`		-- H98 removes
				   parse (FieldPun pos ident)	-- H98 removes
                 )

parseAPat1 exp =
   (ExpRecord exp `parseChk` lcurl `ap` manySepSafe comma parseFieldPat
                  `chk` rcurl) `into` parseAPat1
	`orelse`
   parse exp
  

parseAPat2 =
    varid `revAp` ((\e (pos,i) -> PatAs pos i e) `parseChk` lit L_At
                                                 `ap` parseAPat
                        `orelse`
                    parse (\ (pos,e) -> ExpVar pos e)
                  )
        `orelse`
    (\(pos,e) -> ExpCon pos e) `parseAp` aconid
        `orelse`
    PatWildcard `parseAp` lit L_Underscore
        `orelse`
    mkParExp `parseAp` lpar `ap` manySepSafe comma parsePat `chk` rpar
        `orelse`
    ExpList `parseAp` lbrack `ap` manySepSafe comma parsePat `chk` rbrack
        `orelse`
    PatIrrefutable `parseAp` lit L_Tidle `ap` parseAPat
        `orelse`
    (uncurry ExpLit `parseAp`
         (integer `orelse` rational `orelse` char `orelse` string))
 --     `orelse`
 -- mkParLhs `parseAp` lpar `ap` parsePat0 `chk` rpar `ap` someSafe parseAPat

{- End Module Parse ---------------------------------------------------------}
