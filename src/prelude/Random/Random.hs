module Random
  ( RandomGen (next,split)
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

