{-# LANGUAGE ScopedTypeVariables #-}
module Random where

import Data.Maybe (fromJust)
import qualified Data.Hashable as Hash
import System.Random (StdGen, mkStdGen, uniform)
import Control.Monad.Random.Lazy (Rand, liftRand, withRand)
import Data.Bits (Bits(bitSizeMaybe), rotate, xor)


type Seed = Int

-- | Get random generator from a given seed
getGen :: Seed -> StdGen
getGen = mkStdGen

-- | A hacky way to make a random generator local:
--   Run localRand (x, y) to get your generator
--   to act in a way that depends on (x, y):
localRand :: (Int, Int) -> Rand StdGen ()
localRand (x, y) = do
  -- Convert x and y to unsigned.
  let (xUnsigned :: Word) = fromIntegral x
  let (yUnsigned :: Word) = fromIntegral y
  -- `xor` x and a rotated version of y,
  -- to get a different value for each coordinate pair (up to sufficiently large coordinates)
  let coordMask = xUnsigned `xor` rotate yUnsigned (fromJust (bitSizeMaybe xUnsigned) `div` 2)
  -- Now we need to combine our local mask with our global random generator.
  -- There doesn't seem to be a direct way to combine a `Word` with a `StdGen`,
  -- so what I do is:
  -- 1. Get a `Word` from our `StdGen` using `uniform`
  globalValue <- liftRand uniform
  -- 2. Combine both `Word`s with a simple `xor`
  let localValue = fromIntegral $ globalValue `xor` coordMask
  -- 3. Convert that `Word` back to a random generator:
  let localSeed = mkStdGen localValue
  -- 4. Then we set the generator to be `localSeed`
  withRand (const localSeed) $ return ()
