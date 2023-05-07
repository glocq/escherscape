module Chunk where

-- General Haskell symbols
import GHC.Float    (int2Float)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader)
-- Containers
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.Vector as Vector
import qualified Data.Set    as Set
-- Random
import System.Random (uniformR, mkStdGen)
import Control.Monad.Random (liftRand, evalRand)
-- Raylib
import qualified Raylib.Types as RL
import Raylib.Util.Math ((/*/), matrixIdentity, matrixTranslate, matrixScale)
import Raylib.Util.Lenses (_vector3'x, _vector3'y)
-- Internal symbols
import qualified Scene
import Model (InContext, model, shader, (|*))
import Random (localRand)




-- | Length of a chunk
chunkSide :: Float
chunkSide = 3

-- | Distance in chunks
chunkDist :: Float
chunkDist = 10

type ChunkCoordinates = (Int, Int)
type ChunkMap = HashMap ChunkCoordinates Scene.Scene



visibleChunks :: RL.Vector3 -> Set.Set ChunkCoordinates
visibleChunks pos = Set.fromList [(i, j) | i <- [floor $ x/chunkSide - chunkDist .. ceiling $ x/chunkSide + chunkDist]
                                         , j <- [floor $ y/chunkSide - chunkDist .. ceiling $ y/chunkSide + chunkDist]]
  where x = pos ^. _vector3'x
        y = pos ^. _vector3'y


chunkAt :: ChunkCoordinates -> Int -> InContext Scene.Scene
chunkAt (i, j) globalSeed = Scene.transformScene (matrixTranslate (chunkSide * int2Float i) (chunkSide * int2Float j) 0) <$> do
  tree   <- model "tree"
  ground <- model "ground"
  return . Scene.Scene . Vector.fromList . flip evalRand (mkStdGen 0) $ do
    localRand (i, j) -- get a random generator just for this chunk
    s <- liftRand $ uniformR (0, 1) -- random scale factor
    return $ -- just the ground, and a randomly-sized tree
      [ (tree  , matrixScale $ RL.Vector3 s         s         s)
      , (ground, matrixScale $ RL.Vector3 chunkSide chunkSide 1)
      ]
