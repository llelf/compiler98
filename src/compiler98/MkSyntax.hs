{- ---------------------------------------------------------------------------
Make parts of Haskell syntax tree
-}
module MkSyntax
	( mkAppExp, mkAppInst, mkCase, mkDeclClass
	, mkDeclFun, mkDeclPat, mkDeclPatFun, mkEnumFrom
	, mkEnumThenFrom, mkEnumToFrom, mkEnumToThenFrom
	, mkExpListComp, mkIf, mkInfixList
	, mkInstList, mkInt, mkParExp, mkParInst, mkParType
	, mkTypeList, mkPatNplusK
	) where

import Extra(Pos(..),noPos,strPos)
import TokenId
import Syntax
import SyntaxPos(HasPos(getPos))


mkParType :: Pos -> [Type TokenId] -> Type TokenId

mkParType p [t] = t
mkParType p ts  = TypeCons p (t_Tuple (length ts)) ts


mkAppInst :: (Pos,a) -> [(Pos,a)] -> Type a

mkAppInst (p,c) ts = TypeCons p c (map (uncurry TypeVar) ts)


mkInfixList [e] = e
mkInfixList es = ExpInfixList (getPos es) es


mkParInst p [t] = error ("mkParInst on singleton list")
mkParInst p ts  = TypeCons p (t_Tuple (length ts)) (map (uncurry TypeVar) ts)


mkInstList p id = TypeCons p t_List [TypeVar p id]


mkDeclPat :: (Pos,a) -> Exp a -> Exp a -> Rhs a -> Decls a -> Decl a

mkDeclPat (pv,var) op e@(ExpInfixList pos es) gdexps w =
	DeclPat (Alt (ExpInfixList pos [ExpVar pv var,op,e]) gdexps w)
mkDeclPat (pv,var) op e gdexps w =
	DeclPat (Alt (ExpInfixList pv [ExpVar pv var,op,e]) gdexps w)


mkDeclFun :: (Pos,a) -> [Pat a] -> Rhs a -> Decls a -> Decl a

--mkDeclFun (pv,var) [] gdexps w =
--	DeclPat (Alt (ExpVar pv var) gdexps w)
mkDeclFun (pv,var) pats gdexps w =
        DeclFun pv var [Fun pats gdexps w]


mkDeclPatFun :: Alt a -> Decl a

mkDeclPatFun  (Alt (ExpVar pos fun) gdexps w) =
        DeclFun pos fun [Fun [] gdexps w]
--        DeclPat (Alt (ExpVar pos fun) gdexps w)
mkDeclPatFun  (Alt (ExpInfixList _ [ExpVar pos fun]) gdexps w) =
  DeclFun pos fun [Fun [] gdexps w]
mkDeclPatFun  (Alt (ExpInfixList _ (ExpVar pos fun:qop:args)) gdexps w) 
  | notOp qop = DeclFun pos fun [Fun (qop:args) gdexps w]
mkDeclPatFun alt = DeclPat alt

notOp (ExpConOp _ _) = False
notOp (ExpVarOp _ _) = False
notOp _ = True

--mkRevTypeArrow p a b = TypeCons p t_Arrow [b,a]
mkTypeArrow a p b = TypeCons p t_Arrow [a,b]
mkTypeList p t = TypeCons p t_List [t]


mkExpTrue :: Exp TokenId
mkExpTrue  = ExpCon noPos tTrue

mkExpFalse :: Exp TokenId
mkExpFalse = ExpCon noPos tFalse


mkIf pos c e1 e2 = ExpIf pos c e1 e2
mkCase pos exp alts = ExpCase pos exp alts 

mkEnumFrom pos eFrom =
        ExpApplication pos [ExpVar pos tenumFrom,eFrom]
mkEnumToFrom pos eTo eFrom =
        ExpApplication pos [ExpVar pos tenumFromTo,eFrom,eTo]
mkEnumThenFrom pos eThen eFrom =
        ExpApplication pos [ExpVar pos tenumFromThen,eFrom,eThen]
mkEnumToThenFrom pos eTo eThen eFrom =
        ExpApplication pos [ExpVar pos tenumFromThenTo,eFrom,eThen,eTo]


mkAppExp [] = error "mkAppExp"
mkAppExp [e] = e
mkAppExp es@(e:_)  = ExpApplication (getPos e) es


mkParExp pos [ExpConOp pos' id] = ExpCon pos' id
mkParExp pos [ExpVarOp pos' id] = ExpVar pos' id
mkParExp pos [e] = e
mkParExp pos es  = ExpApplication pos (ExpCon pos (t_Tuple (length es)):es)


-- combineGroups (DeclsParse d1) (DeclsParse d2) = DeclsParse (d1++d2)
-- 
-- mkDeclClass ctx (pos,cls) (_,arg) (csigns,valdefs) =
--             DeclClass pos ctx cls arg (combineGroups csigns valdefs)

-- changed in H98 to:
mkDeclClass ctx (pos,cls) (_,arg) cdecls = DeclClass pos ctx cls arg cdecls


mkExp_Colon :: Pos -> Exp TokenId
mkExp_Colon pos  = ExpCon pos t_Colon

mkExp_filter :: Pos -> Exp TokenId
mkExp_filter pos  = ExpVar pos t_filter

mkExp_foldr :: Pos -> Exp TokenId
mkExp_foldr pos  = ExpVar pos t_foldr

mkExp_x :: Pos -> Exp TokenId
mkExp_x pos  = ExpVar pos t_x
mkExp_y :: Pos -> Exp TokenId
mkExp_y pos  = ExpVar pos t_y


mkExpListComp pos qs e = ExpApplication noPos [trans pos qs e,ExpList noPos []]
 where
  trans pos [] e =
    ExpApplication pos
        [mkExp_Colon pos
        ,e
        ]
  trans pos (QualLet decls:qs) e =
    ExpLet pos decls (trans pos qs e)
  trans pos (QualExp exp:qs) e =
    ExpApplication pos
        [mkExp_filter pos
        ,exp
        ,trans pos qs e
        ]
  trans pos (QualPatExp pat exp:qs) e =
    ExpApplication pos
        [mkExp_foldr pos
        ,ExpLambda pos
            [mkExp_x noPos,mkExp_y pos]
            (ExpCase noPos (mkExp_x pos)
              [Alt pat
                   (Unguarded (ExpApplication pos [trans pos qs e,mkExp_y pos]))
                   (DeclsParse [])
              ,Alt (PatWildcard pos)
                   (Unguarded (mkExp_y pos))
                   (DeclsParse [])
              ]
            )
        ,exp
        ]


mkInt pos i = ExpLit pos (LitInt Boxed i)

mkPatNplusK (pos,tid) (posi,integer) =
    PatNplusK pos tid undefined (ExpLit posi integer) undefined undefined
-- While parsing (n+k), can't choose a unique replacement identifier n',
-- so leave some fields to be filled in later.

--  let k = ExpLit posi integer in
--  PatNplusK pos tid undefined k (ExpApplication pos [t_lessequal,k])
--                                (ExpApplication pos [t_subtract,k])

{- End Module MkSyntax ------------------------------------------------------}
