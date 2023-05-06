module Render (initGraphics, worldgenLoop, renderLoop) where

-- | High-level control functions
import Control.Monad (forever, unless)
import Control.Monad.Reader (runReader)
import Control.Lens (view)
import Data.Foldable (foldl', foldlM)
-- Concurrency
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (yield)
-- Containers
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
-- Raylib
import qualified Raylib.Core as RL
import Raylib.Core.Camera (updateCamera)
import Raylib.Util (WindowResources)
import Raylib.Types (Color (Color), Model, CameraMode (CameraModeCustom))
-- My own symbols
import qualified GameState as GS
import qualified Viewpoint as VP
import Model (ModelMap, ShaderMap, loadModelMap, loadShaderMap)
import Chunk (ChunkCoordinates, visibleChunks, chunkAt)
import Scene (Scene, drawScene)




initGraphics :: IO WindowResources
initGraphics = do
  window <- RL.initWindow 1600 900 "Bofill"
  RL.setTargetFPS 60
  return window


-- | Update the list of chunks that should be displayed
worldgenLoop :: STM.TVar GS.GameState                         -- ^ Game logic-related state; used for player location
             -> ModelMap                                      -- ^ Contains our models loaded from files on disk
             -> ShaderMap                                     -- ^ Contains our shaders loaded from files on disk
             -> STM.TVar (Map.HashMap ChunkCoordinates Scene) -- ^ Chunks to display
             -> IO ()
worldgenLoop state modelMap shaderMap chunks = forever $ do
  -- Look up player position
  playerCoords <- view (GS.viewpoint . VP.position) <$> STM.atomically (STM.readTVar state)
  -- Compute set of visible coordinates
  let visible = visibleChunks playerCoords
  -- Update chunk collection
  STM.atomically $ STM.modifyTVar chunks (updateChunks visible)
  -- Prevent this loop from monopolizing control
  yield
  where
    updateChunks :: Set.Set ChunkCoordinates           -- ^ set of now-visible chunks
                 -> Map.HashMap ChunkCoordinates Scene -- ^ collection of chunks to update
                 -> Map.HashMap ChunkCoordinates Scene
    updateChunks visibleCoords formerChunks = do
      -- Remove obsolete chunks
      let severed = Map.filterWithKey (\key _ -> Set.member key visibleCoords) formerChunks
      -- Get collection of chunks to add to collection
      let toAdd = Set.filter (not . (`Map.member` severed)) visibleCoords
      -- Add them
      foldl'
        -- How to add the chunks with given coordinates:
        (\chks coords -> Map.insert coords (runReader (chunkAt coords) (modelMap, shaderMap)) chks)
        -- What to add chunks to:
        severed
        -- Coordinates of the chunks to add:
        toAdd


-- | Render scene
renderLoop :: STM.TVar GS.GameState
           -> Map.HashMap String Model
           -> STM.TVar (Map.HashMap (Int, Int) Scene)
           -> IO ()
renderLoop state modelMap chunks = do
  -- Deduce camera properties from game state
  camera <- VP.viewpointCamera . view GS.viewpoint <$> STM.atomically (STM.readTVar state)
  -- Prepare to draw
  updateCamera camera CameraModeCustom
  RL.beginDrawing
  RL.clearBackground $ Color 51 153 255 255
  RL.beginMode3D camera
  -- Get chunks to draw
  world <- STM.atomically $ STM.readTVar chunks
  -- Draw chunks
  foldlM (const drawScene) () world
  -- Terminate frame
  RL.endMode3D
  RL.endDrawing
  -- Prevent render loop from monopolizing control
  yield
  -- Iterate again unless window should close
  RL.windowShouldClose >>= (`unless` renderLoop state modelMap chunks)
