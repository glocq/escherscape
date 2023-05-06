module Chunk where

-- General Haskell symbols
import GHC.Float    (int2Float)
import Control.Lens ((^.))
import Control.Monad.Reader (Reader)
-- Containers
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.Vector as Vector
import qualified Data.Set    as Set
-- Raylib
import qualified Raylib.Types       as RL
import qualified Raylib.Util.Math   as RL
import qualified Raylib.Util.Lenses as RL
-- Internal symbols
import qualified Scene
import Model (InContext, model, shader, (|*))




-- | Length of a chunk
chunkSide :: Float
chunkSide = 5

-- | Distance in chunks
chunkDist :: Float
chunkDist = 10

type ChunkCoordinates = (Int, Int)
type ChunkMap = HashMap ChunkCoordinates Scene.Scene



visibleChunks :: RL.Vector3 -> Set.Set ChunkCoordinates
visibleChunks pos = Set.fromList [(i, j) | i <- [floor $ x/chunkSide - chunkDist .. ceiling $ x/chunkSide + chunkDist]
                                         , j <- [floor $ y/chunkSide - chunkDist .. ceiling $ y/chunkSide + chunkDist]]
  where x = pos ^. RL._vector3'x
        y = pos ^. RL._vector3'y


chunkAt :: ChunkCoordinates -> InContext Scene.Scene
chunkAt (i, j) = do
  basic <- shader "basic"
  tree   <- model "tree"
  ground <- model "ground"
  return . Scene.Scene . Vector.fromList $
    [ (tree   |* basic, RL.matrixTranslate (chunkSide * int2Float i) (chunkSide * int2Float j) 0)
    , (ground |* basic, RL.matrixTranslate (chunkSide * int2Float i) (chunkSide * int2Float j) 0)
    ]
