module Lexical(lexical,lexicalCont,Lex{-,PackedString-}
                ,LexState(..),PosToken(..),PosTokenPre(..),Pos(..)) where

import Extra(Pos(..),toPos,strPos)
import Lex
import LexPre
import PackedString(PackedString,packString,unpackPS)
import TokenId

type PosToken = (Pos,Lex, LexState, [PosTokenPre])
type LexState = [Int]

-- 0 : no active indentation (explicit layout)

lexical :: Bool -> [Char] -> [Char] -> [PosToken]
lexical u file l =
    case packString file of
        file' -> 
            case lexPre u file' l of
                lp@((f,r,c,L_module):_) ->  iLex [0] 0 lp
                lp@((f,r,c,L_interface):_) ->  iLex [0] 0 lp
                lp ->  iLex [0] 0 ((file',1,0,L_module):(file',1,0,L_ACONID tMain):(file',1,0,L_where):lp)

lexicalCont :: PosToken -> Either String [PosToken]
lexicalCont (p,t,(i:s@(i':_)),r) =
                if i > 0
                then -- Right ((p,t,s,r) : iLex s i' r) -- not correct?
                     case r of
                       ((f,_,_,_):_) -> Right (piLex f s i' p t r)
                else Left "Layout }"
lexicalCont (p,t, []  ,r) = 
                Left "Layout }"

---  local

iLex s i [] = []
iLex s i ((f,r,c,t):pt) = 
  seq p $
  if c > i then
    piLex f s i p t pt
  else if c == i && i /= 0 && t /= L_in then
    (p,L_SEMI',s,pt) : piLex f s i p t pt
  else if c == 0 && i == 0 then
    piLex f s i p t pt
  else
    (p,L_RCURL',s,pt) : iLex s' i' ((f,r,c,t):pt)
  where
    (_:s'@(i':_)) = s
    p = toPos r c

piLex :: PackedString -> LexState -> Int -> Pos -> Lex -> [PosTokenPre] -> [PosToken]
piLex file s i p tok tr@((f,r,c,t'):pt)
      | tok `elem` [L_let, L_where, L_of, L_do] =
          (p,tok,s,tr)
          : if t' == L_LCURL then
                let p' = toPos r c in seq p' (p',L_LCURL, s,pt)
                : iLex (0:s) 0 pt 
            else
                (p, L_LCURL',s,tr)
                : if c > i then
                    let p' = toPos r c in seq p' $ piLex f (c:s) c p' t' pt
                  else
                    (p, L_RCURL',s,tr)
                    : iLex s i tr
piLex file s i p L_LCURL  pt =
          (p,L_LCURL,s,pt)
          : iLex (0:s) 0 pt
piLex file s i p L_RCURL  pt = 
      if i == 0
      then case s of 
             (_:s'@(i':_)) -> (p,L_RCURL,s,pt) : iLex s' i' pt
             _             -> failPos file p "Unbalanced '}' (Stack empty)."
      else failPos file p "Unbalanced '}' (No explicit '{' in scope)"
piLex file s i p t pt  =
          (p,t,s,pt)
          : iLex s i pt


failPos file p msg = error ("Internal in " ++ unpackPS file ++ " at " ++ strPos p ++ ": " ++ msg ++ "\n")
