module Maybe where

--    unfoldr f' (foldr f z xs) == (xs,z)
--
-- if the following holds:
--
--    f' (f x y) = Just (x,y)
--    f' z       = Nothing
unfoldr                :: (a -> Maybe (b, a)) -> a -> ([b],a)
unfoldr f x =
  case f x of
  Just (y,x') -> let (ys,x'') = unfoldr f x' in (y:ys,x'')
  Nothing     -> ([],x)
