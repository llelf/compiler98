
module Core.Convert(makeCore) where

import Id
import Util.Extra(mixLine,mixSpace,mix)
import PosCode
import StrPos
import List
import Char
import Util.Extra
import Error
import IntState
import Maybe
import NT

import Yhc.Core


-- generate the core
makeCore :: [String] -> IntState -> [Id] -> [(Id, PosLambda)] -> Core
makeCore imports state datas code = Core modu imports
            (datas2core state datas)
            (bind2core True modu state [] code)
    where modu = getModuleId state


datas2core :: IntState -> [Id] -> [CoreData]
-- reverse because they seem to get generated in reverse order
datas2core state datas = map (data2core state) (reverse datas)

data2core :: IntState -> Id -> CoreData
data2core state i = CoreData (fixTuple $ show name)
                             (map strTVar free)
                             (map (ctor2core state) childs)
    where
        NewType free _ _ _ = typ

        (InfoData _ name _ typ children) = fst $ getInfo i () state
        childs = case children of
                    (DataNewType _ x) -> x
                    (Data _ x) -> x

ctor2core :: IntState -> Id -> CoreCtor
ctor2core state i = CoreCtor (fixTuple $ show name) (zip (map g targs) (map f fields))
    where
        NewType _ _ _ targs = typ
        (InfoConstr _ name _ _ typ fields _) = fst $ getInfo i () state

        f Nothing = Nothing
        f (Just x) = Just $ dropModule $ getName state x
        
        g x = strNT (getName state) strTVar x

fixTuple :: String -> String
fixTuple x = let CoreCon y = con2core x in y


bind2core :: Bool -> String -> IntState -> [String] -> [(Id, PosLambda)] -> [CoreFunc]
bind2core top modu state free xs = map (lam2core top modu state free) xs


getName :: IntState -> Id -> String
getName state v = strIS state v


lam2core :: Bool -> String -> IntState -> [String] -> (Id, PosLambda) -> CoreFunc
lam2core top modu state free (i, PosLambda pos int fvs bvs e) =
    CoreFunc name args (wrapPos pos $ exp2core modu state (args++free) e)
    where
        args = (map (getName state . snd) bvs)
        name = getRealName modu state i

lam2core top modu state free (i, item) =
        CorePrim name arity
    where
        name = getRealName modu state i
        arity = arityIS state i


getRealName :: String -> IntState -> Id -> String
getRealName modu state i = if isLambda name then modu ++ "._" ++ name else name
    where name = getName state i


wrapPos :: Pos -> CoreExpr -> CoreExpr
wrapPos pos x | pos == noPos = x
              | otherwise    = CorePos (show pos) x




data Exp2Core = Exp2Core {failExpr :: CoreExpr, failCount :: Int, freeVars :: [String]}



exp2core :: String -> IntState -> [String] -> PosExp -> CoreExpr
exp2core modu state origfree x = f (Exp2Core undefined 0 origfree) x
    where
        f dat@Exp2Core{failExpr=failExpr, failCount=failCount, freeVars=freeVars} x = case x of
            -- constants
            PosInt     _ i -> CoreInt i
            PosInteger _ i -> CoreInteger i
            PosChar    _ c -> CoreChr (chr c)
            PosString  _ s -> CoreStr s
            PosFloat   _ i -> CoreFloat i
            PosDouble  _ i -> CoreDouble i
        
            -- simple stuff
            PosExpDict e -> f2 e
            PosExpThunk p _ args -> f2 $ PosExpApp p args
            PosCon _ i -> con2core $ getName state i
            PosPrim _ prim _ -> CoreFun $ strPrim prim
            PosVar _ i -> if nam `elem` freeVars then CoreVar nam else CoreFun nam
                          where nam = getRealName modu state i
            
            PosExpApp _ (a:rgs) | f2 a == CoreFun "STRING" -> f2 (head rgs)
                                | otherwise -> CoreApp (f2 a) (map f2 rgs)
            
            -- Let bindings
            PosExpLet _ _ [] e -> f2 e
            PosExpLet _ _ xs e -> CoreLet binds $ f dat{freeVars = free2} e
                where
                    binds = [(x,y) | CoreFunc x _ y <- bind2core False modu state free2 xs]
                    free2 = map fst binds ++ freeVars

            -- Choice
            PosExpIf pos g e1 e2 e3 -> CoreCase (f2 e1)
                [(CoreApp (con2core "Prelude.True") [], f2 e2), (CoreApp (con2core "Prelude.False") [], f2 e3)]
            PosExpCase pos e args -> CoreCase (f2 e) (map g args)
                where
                g (PosAltInt pos i False e) = (CoreChr (chr i), f2 e)
                g (PosAltInt pos i True  e) = (CoreInt i, f2 e)
                g (PosAltCon pos c args e) =
                    (CoreApp (con2core $ getName state c) (map CoreVar free2), f dat{freeVars=free2++freeVars} e)
                    where free2 = map (getName state . snd) args
 
            -- Fat bar stuff
            PosExpFatBar _ e1@(PosExpCase{}) PosExpFail ->
                CoreCase a (b ++ [(CoreVar "_", failExpr)])
                where CoreCase a b = f2 e1

            PosExpFatBar _ e1 PosExpFail -> f2 e1
            
            PosExpFatBar pos e1 e2 -> CoreLet [(var, f dat{failCount=failCount+1} e2)]
                                      (f dat{failCount=failCount+1, failExpr=CoreVar var} (PosExpFatBar pos e1 PosExpFail))
                where var = "v_fail_" ++ show failCount
            
            PosExpFail -> failExpr

            other -> raiseError $ ErrorInternal "Core.Core.exp2core.f" (strPExp (getName state) "" other)
            
            where
                f2 = f dat


-- fix tupling
con2core :: String -> CoreExpr
con2core "()" = CoreCon "Prelude.()"
con2core ('P':'r':'e':'l':'u':'d':'e':'.':xs)
    | all isDigit xs = CoreCon $ "Prelude." ++ tupName (read xs)
con2core x = CoreCon x

tupName :: Int -> String
tupName 1 = "1()"
tupName n = "(" ++ replicate (n-1) ',' ++ ")"

isLambda :: String -> Bool
isLambda x = "LAMBDA" `isPrefixOf` x


pos2core :: Pos -> CoreExpr -> CoreExpr
pos2core p = CorePos (show p)
