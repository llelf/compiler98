module Random
  ( RandomGen (next,split,genRange)
  , StdGen, mkStdGen
  , Random (random, randomR, randoms, randomRs, randomIO, randomRIO)
  , getStdRandom, getStdGen, setStdGen, newStdGen
  ) where

import Warning

--  The old library signature
--random   :: (Integer,Integer) -> Integer -> [Integer]
--randomIO :: (Integer,Integer) -> IO [Integer]
--
--random   (lo,hi) seed = warning "Random.random: not implemented" []
--randomIO (lo,hi)      = warning "Random.randomIO: not implemented" (return [])

-- The new library signature

class RandomGen g where
  next  :: g -> (Int, g)
  split :: g -> (g, g)
  genRange :: g -> (Int,Int)

-- constraints:
--  (1) if (a,b) = genRange g, then a<b
--  (2) genRange (snd (next g))  = genRange g
--      genRange (fst (split g)) = genRange g
--      genRange (snd (split g)) = genRange g
--  (3) the Int output of (next g) is equally likely to be any of
--      the Ints in the range (genRange g), including both endpoints.


data StdGen = StdGen Int	-- abstract
    deriving (Read,Show)

mkStdGen :: Int -> StdGen
mkStdGen = StdGen

instance RandomGen StdGen

class Random a where
  randomR   :: RandomGen g => (a,a) -> g -> (a,g)
  random    :: RandomGen g =>          g -> (a,g)

  randomRs  :: RandomGen g => (a,a) -> g -> [a]
  randoms   :: RandomGen g =>          g -> [a]

  randomRIO :: (a,a) -> IO a
  randomIO  ::          IO a

instance Random Int
instance Random Integer
instance Random Float
instance Random Double
instance Random Bool
instance Random Char

newStdGen      :: IO StdGen
setStdGen      :: StdGen -> IO ()
getStdGen      :: IO StdGen
getStdRandom   :: (StdGen->(a,StdGen)) -> IO a

newStdGen      = warning "Random.newStdGen: not implemented" (return (StdGen 0))
setStdGen _    = warning "Random.setStdGen: not implemented" (return ())
getStdGen      = warning "Random.getStdGen: not implemented" (return (StdGen 0))
getStdRandom f = warning "Random.getStdRandom: not implemented" (return (fst (f (StdGen 0))))

