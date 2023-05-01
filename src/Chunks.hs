module Chunks where

-- General Haskell symbols
import GHC.Float    (int2Float)
import Control.Lens ((^.))
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
import qualified Models
import qualified Shaders
import Shaders ((|*))




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


chunkAt :: Models.ModelMap -> Shaders.ShaderMap -> ChunkCoordinates -> Scene.Scene
chunkAt models shaders (i, j) = Scene.Scene $ Vector.fromList
  [ ((models ! "tree"  ) |* (shaders ! "basic"), RL.matrixTranslate (chunkSide * int2Float i) (chunkSide * int2Float j) 0)
  , ((models ! "ground") |* (shaders ! "basic"), RL.matrixTranslate (chunkSide * int2Float i) (chunkSide * int2Float j) 0)
  ]
