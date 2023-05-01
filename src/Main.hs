module Main where

import Control.Monad (forever, unless)
import qualified Data.Set            as Set
import qualified Data.HashMap.Strict as Map
import Control.Applicative ((<|>))
import Control.Concurrent (yield)
import Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import qualified Control.Concurrent.STM as STM

import Data.Foldable (foldl', foldlM)
import GHC.Float (double2Float)
import Control.Lens (view, (&), (%~))
import Data.HashMap.Strict ((!))

import qualified Raylib.Core          as RL
import qualified Raylib.Core.Camera   as RL
import qualified Raylib.Core.Models   as RL
import qualified Raylib.Types         as RL
import qualified Raylib.Util          as RL
import qualified Raylib.Util.Colors   as RL
import qualified Raylib.Util.Lenses   as RL
import qualified Raylib.Util.Math     as RL

import Models (ModelMap, drawModel, loadModelMap)
import Shaders (ShaderMap, loadShaderMap)
import Input (inputLoop)
import qualified Viewpoint as VP
import qualified GameState as GS
import Scene (Scene, demoScene, drawScene)
import Chunks (visibleChunks, chunkAt, ChunkCoordinates)

import qualified Raylib.Core.Text as RL



main :: IO ()
main = do
  -- Initialization
  window <- initGraphics
  modelMap  <- loadModelMap  window "assets/models"
  shaderMap <- loadShaderMap window "assets/shaders"
  gameState <- STM.newTVarIO GS.initialGameState
  chunks    <- STM.newTVarIO Map.empty
  -- Running
  runConcurrently $
    Concurrently (inputLoop    gameState                          ) <|>
    Concurrently (worldgenLoop gameState modelMap shaderMap chunks) <|>
    Concurrently (renderLoop   gameState modelMap chunks          )
  -- Termination
  RL.closeWindow window



initGraphics :: IO RL.WindowResources
initGraphics = do
  window <- RL.initWindow 800 450 "Bofill"
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
        (\chks coords -> Map.insert coords (chunkAt modelMap shaderMap coords) chks)
        -- What to add chunks to:
        severed
        -- Coordinates of the chunks to add:
        toAdd



renderLoop :: STM.TVar GS.GameState
           -> Map.HashMap String RL.Model
           -> STM.TVar (Map.HashMap (Int, Int) Scene)
           -> IO ()
renderLoop state modelMap chunks = do
  -- Deduce camera properties from game state
  camera <- VP.viewpointCamera . view GS.viewpoint <$> STM.atomically (STM.readTVar state)
  -- Prepare to draw
  RL.updateCamera camera RL.CameraModeCustom
  RL.beginDrawing
  RL.clearBackground $ RL.Color 51 153 255 255
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
