module PPSyntax(ppModule,ppInterface,ppFun,ppDecl,ppImpDecls,ppDecls,ppClassCodes,ppExp) where 

import Extra(Pos(..),mixComma,mixSpace)
import PPLib
import Syntax
import StrSyntax
import IntState(IntState,lookupIS)
import Nice(niceInt,niceNT,mkAL)
import Info(Info(InfoData))
import NT(NewType(NewType))


{-
If boolean argument true, then positions are inserted as comments 
for most language constructs.
IntState is needed to convert an Id into a String (StrId).
-}
ppModule :: (StrId a, Show a) => Bool -> IntState -> Module a -> Doc

ppModule d p (Module pos id exports impdecls fixdecls topdecls) = 
        pp ("module " ++ ((pStd p) id) ++ strExports d p exports ++ " where {")
        `nl` ppImpDecls d p impdecls
        `nl` ppFixDecls d p fixdecls
        `nl` ppTopDecls d p topdecls
        `nl` pp "}\n"

ppImpDecls d p impdecls =
        ppVertSemi (map (ppImpDecl d p) impdecls)
ppFixDecls d p fixdecls =
        ppVertSemi (map (pp . strFixDecl d p) fixdecls)

ppImpDecl d p (Import (pos,id) impspec) =
        pp ("import " ++ (pStd p) id ++ strImpSpec d p impspec)
ppImpDecl d p (ImportQ (pos1,id1) impspec) =
        pp ("import qualified " ++ (pStd p) id1 ++ strImpSpec d p impspec)
ppImpDecl d p (ImportQas (pos1,id1) (pos2,id2) impspec) =
        pp ("import qualified " ++ (pStd p) id1 ++ " as " ++ (pStd p) id2
            ++ strImpSpec d p impspec)
ppImpDecl d p (Importas (pos1,id1) (pos2,id2) impspec) =
        pp ("import " ++ (pStd p) id1 ++ " as " ++ (pStd p) id2
            ++ strImpSpec d p impspec)

ppTopDecl x = ppDecl x
ppValdef  x = ppDecl x
ppVarstrType x = ppDecl x
ppTopDecls x = ppDecls x
ppValdefs x  = ppDecls x
ppVarstrTypes x = ppDecls x

noDecls :: Decls id -> Bool
noDecls (DeclsParse decls) = null decls
noDecls (DeclsScc decls) = null decls

ppDecls d p (DeclsParse decls) = ppVertSemi (map (ppDecl d p) decls)
ppDecls d p (DeclsScc decls) =   ppVertSemi (map (ppDepend d p) decls)

ppDepend d p (DeclsNoRec decl)  =
  pp "-- not recursive" `nl` ppDecl d p decl
ppDepend d p (DeclsRec   decls) =
  pp "--     recursive" `nl` ppVertSemi (map (ppDecl d p) decls)


--ppDecl :: (StrId a, Show a) => Bool -> IntState -> Decl a -> Doc

ppDecl d p (DeclType s t) =
  pp ("type " ++ strTypeSimple d p s ++ " = " ++ strType d p t)

ppDecl d p (DeclTypeRenamed pos tyconid) =
  pp ("type " ++ niceInt Nothing p tyconid (' ' : mixSpace (map snd al)) ++ 
        " = " ++ niceNT Nothing p al nt)
  where
  Just (InfoData _ _ _ newType _) = lookupIS p tyconid
  NewType univQuantTyVars _ _ [nt] = newType
  al = mkAL univQuantTyVars

ppDecl d p (DeclDataPrim pos conid size) =
  pp ("data primitive " ++ ppPos d pos ++ (pStd p) conid ++ " = " ++ show size)
ppDecl d p (DeclData dk ctxs s [] ds) =
  pp ("data " ++ strContexts d p ctxs ++ strTypeSimple d p s)
    `cl` pp (strDerivings d p ds)
ppDecl d p (DeclData dk ctxs s cs ds) =
  pp ("data " ++ strContexts d p ctxs ++ strTypeSimple d p s)
    `nl` ppConstrs d p cs
    `cl` pp (strDerivings d p ds)

ppDecl d p (DeclConstrs pos did cs) =
  pp ("-- data/dataprim [(field,selector)] = "
      ++ pStd p did ++ " ["
      ++ mixComma (map ( \ (pos,field,sel) -> 
           "(" ++ pStd p field ++ "," ++ pStd p sel ++ ")")  cs) ++ "]")
ppDecl d p (DeclClass pos ctxs cls arg decls) =
  pp ("class "++strContexts d p ctxs++" "++(pStd p) cls++" "
    ++ 't':show arg++" ") `cl` ppWhere d p decls
ppDecl d p (DeclInstance pos cxs tycls insts valdefs) =
  pp ("instance " ++ strContexts d p cxs ++ " " ++ (pStd p) tycls ++ " " 
    ++ strInst d p insts) `cl` ppWhere d p valdefs
ppDecl d p (DeclDefault ts) =
  pp ("default (" ++ mixComma (map (strType d p) ts) ++ ")")
ppDecl d p dec@(DeclPrimitive pos ident arity t) =
  pp ( (pStd p) ident ++ "primitive " ++ show arity ++ " :: " ++ strType d p t)
ppDecl d p dec@(DeclForeignImp pos str ident arity cast t _) =
  pp ("foreign import \"" ++ str ++ "\" " ++ show cast ++ " "
    ++ (pStd p) ident ++ " :: " ++ strType d p t)
ppDecl d p dec@(DeclForeignExp pos str ident t) =
  pp ("foreign export " ++ (pStd p) ident ++ " :: " ++ strType d p t)
ppDecl d p dec@(DeclVarsType ids cxs t) =
  pp (strVarsType d p dec)
ppDecl d p (DeclPat alt) =
  pp "{- pattern -}" `nl` ppAlt "=" d p alt
ppDecl d p (DeclFun pos fun funs) =
  pp "{- function -}" `nl`
  pp (ppPos d pos ++ (pStd p) fun) `nl`
  nest (ppVertSemi (map (ppFun d p) funs)) `nl`
  pp ("-- end " ++ (pStd p) fun)
ppDecl d p (DeclIgnore s) =
  pp ("Ignoring " ++ s)
ppDecl d p (DeclError s) =
  pp ("ERROR:  " ++ s)
ppDecl d p (DeclAnnot decl annots) =
  ppDecl d p decl `cl` pp (strAnnots d p annots)
ppDecl d p (DeclFixity f) =
  pp (strFixDecl d p f)

ppWhere d p decls =
  if noDecls decls then
    pp ""
  else
    pp " where {" `nl`
        nest (nest (ppDecls d p decls) `nl`
    pp "}")

ppClassCodes d p decls = ppVertSemi (map (ppClassCode d p) decls)


ppClassCode d p (CodeClass pos cls) =
  pp ("code class "++ (pStd p) cls)
ppClassCode d p (CodeInstance pos cls typ arg ecs methods) =
  pp ("code instance "++ (pStd p) cls ++ " " ++ (pStd p) typ ++ " ? = {?:"++ mixComma (map (pStd p) methods) ++"}")

ppFun d p (Fun pats gds w) =
   ppSpaceSep (map (ppPat d p) pats) `cl` 
   ppVert (map (ppGdPat "=" d p) gds) `cl` ppWhere d p w

ppConstrs d p [] =
  nest ( pp " {- no constructors -} ")
ppConstrs d p [c] =
  nest ( pp ("= " ++ strConstr d p c))
ppConstrs d p (c:cs) =
  nest (pp ("= " ++ strConstr d p c) `nl`
        ppVert (map (\c -> pp ("| " ++ strConstr d p c)) cs)
       )

ppGdRhss d p [gd] = ppGdRhs d p gd
ppGdRhss d p gds = pp "" `nl` nest (ppVert (map (ppGdRhs d p) gds))

ppGdRhs d p (e1,e2) =
  pp " | " `cl` ppExp d p e1 `cl` pp " = " `cl` ppExp d p e2


ppStmts d p stmts =
        ppVertSemi (map (ppStmt d p) stmts)

ppStmt d p (StmtExp exp) = ppExp d p exp
ppStmt d p (StmtBind pat exp) = ppPat d p pat `cl` pp " <- " `cl` ppExp d p exp
ppStmt d p (StmtLet ds) =  
  (pp ("let {") `cl` nest (ppDecls d p ds)) `nl` pp "}"

ppPat x = ppExp x
 
ppPos d pos = if d then "{"++show pos++"}" else ""


ppExp :: (StrId a, Show a) => Bool -> IntState -> Exp a -> Doc

ppExp d p (ExpLambda    pos pats e) =
  pp (ppPos d pos ++ "(\\ ") `cl` 
  ppSpaceSep (map (ppPat d p) pats) `cl` pp " -> " `cl` 
  ppExp d p e `cl` pp ")"
ppExp d p (ExpDo       pos stmts) =
  (pp (ppPos d pos ++ "do {") `nl` nest (ppStmts d p stmts)) `nl` pp "}"
ppExp d p (ExpLet       pos ds e) =
  (pp (ppPos d pos ++ "let {") `nl` nest (ppDecls d p ds)) `nl`
  pp "} in (" `cl` ppExp d p e `cl` pp ")"
ppExp d p (ExpCase      pos e alts) =
  (pp (ppPos d pos ++ "case ") `cl` ppExp d p e` cl` pp " of {") `nl`
       nest (ppVert (map (ppAlt "->" d p) alts)) `nl`
  pp "}"
ppExp d p (ExpFail) =  (pp "fail")
ppExp d p (ExpFatbar e1 e2) = 
  pp "fatbar " `nl`
  ppExp d p e1 `nl`
  pp "--" `nl`
  ppExp d p e2
ppExp d p (ExpIf      pos c e1 e2) =
  pp (ppPos d pos ++ "if ") `cl` ppExp d p c `nl`
  pp "then " `cl` ppExp d p e1 `nl`
  pp "else " `cl` ppExp d p e2
ppExp d p (ExpType      pos e cxs t) =
  pp (ppPos d pos) `cl` ppExp d p e `cl` 
  pp ("::" ++ strContexts d p cxs ++ strType d p t)
ppExp d p (ExpRecord exp fields) =
  ppExp d p exp `cl` pp " {" `cl` ppSemiSep (map (ppField d p) fields) `cl` 
  pp "}"
ppExp d p (ExpApplication pos (f:xs)) =
  pp (ppPos d pos ++ "(") `cl` ppExp d p f `cl` pp " " `cl` 
  ppSpaceSep (map (ppExp d p) xs) `cl` pp ")"
ppExp d p (ExpApplication pos x) =
  pp (ppPos d pos ++ "{- ExpAp -}(") `cl` ppSpaceSep (map (ppExp d p) x) `cl`
  pp ")"
ppExp d p (ExpInfixList         pos xs) =
  pp (ppPos d pos ++ "{-IL-}(") `cl` ppSpaceSep (map (ppExp d p) xs) `cl` 
  pp ")"
ppExp d p (ExpConOp pos id) = 
  pp (ppPos d pos ++ "{cop `" ++ (pStd p) id ++ "`}")
ppExp d p (ExpVarOp pos id) = 
  pp (ppPos d pos ++ "{vop `" ++ (pStd p) id ++ "`}")
ppExp d p (ExpCon pos id) = pp (ppPos d pos ++ "{-C-}"++(pStd p) id)
ppExp d p (ExpVar pos id) = pp (ppPos d pos ++ (pStd p) id)
ppExp d p (ExpDict exp) = pp "{d}" `cl` ppExp d p exp
ppExp d p (ExpLit pos lit) = pp (ppPos d pos ++ show lit)
--ppExp d p (ExpTuple     pos es) =
--  pp (ppPos d pos ++ "(") `cl` ppCommaSep (map (ppExp d p) es) `cl` pp ")"
ppExp d p (ExpList      pos es) =
  pp (ppPos d pos ++ "[") `cl` ppCommaSep (map (ppExp d p) es) `cl` pp "]"
ppExp d p (Exp2  pos id1 id2) = 
  pp (ppPos d pos ++ (pStd p) id1 ++ '.':(pStd p) id2)
ppExp d p (PatAs        pos id pat) =
  pp (ppPos d pos ++ (pStd p) id ++ "@") `cl` ppExp d p pat
ppExp d p (PatWildcard  pos) = pp (ppPos d pos ++ "_")
ppExp d p (PatIrrefutable pos pat) = 
  pp (ppPos d pos ++ "~") `cl` ppExp d p pat
ppExp d p (PatNplusK pos n n' k _ _) = 
  pp (ppPos d pos ++ "{-N+K-}(" ++ (pStd p) n ++ "+")  `cl`  ppExp d p k  `cl` 
  pp ")"


ppField d p (FieldExp pos var exp) =
  pp (ppPos d pos ++ " " ++ (pStd p) var ++ " <- ") `cl` ppExp d p exp
ppField d p (FieldPun pos var) =
  pp (ppPos d pos ++ " " ++ pStd p var)

ppQual d p (QualPatExp pat e) =
  ppPat d p pat `cl` pp " <- " `cl` ppExp d p e
ppQual d p (QualExp e) =
  ppExp d p e
ppQual d p (QualLet ds) =
  (pp ("let {") `cl` nest (ppDecls d p ds)) `nl`
  pp "}"

ppAlt del d p (Alt pat gds w) =
  ppPat d p pat `cl` ppVert (map (ppGdPat del d p) gds) `cl` ppWhere d p w

ppGdPat del d p (e1,e2) =
  pp " | " `cl` ppExp d p e1 `cl` pp (' ':del++" ") `cl` ppExp d p e2



-----------------------------------

ppInterface d p (Interface pos id impdecls fixdecls topdecls) =
        pp ("interface " ++ ((pStd p) id) ++  " where {")
        `nl` ppImpDecls d p impdecls
        `nl` ppFixDecls d p fixdecls
        `nl` ppTopDecls d p topdecls
        `nl` pp "}\n"
