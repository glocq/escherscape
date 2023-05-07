module Main (main) where

-- Concurrency
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (Concurrently), runConcurrently)
import Control.Concurrent.STM (newTVarIO)
-- Raylib
import Raylib.Core (closeWindow)
-- Internal symbols
import Input (inputLoop)
import Render (initGraphics, worldgenLoop, renderLoop)
import GameState (initialGameState)
import Model (loadModelMap, loadShaderMap)


main :: IO ()
main = do
  -- Initialization
  window <- initGraphics
  modelMap  <- loadModelMap  window "assets/models"
  shaderMap <- loadShaderMap window "assets/shaders"
  gameState <- newTVarIO initialGameState
  chunks    <- newTVarIO mempty
  let seed = 0
  -- Running
  runConcurrently $
    Concurrently (inputLoop    gameState                               ) <|>
    Concurrently (worldgenLoop gameState modelMap shaderMap chunks seed) <|>
    Concurrently (renderLoop   gameState modelMap chunks               )
  -- Termination
  closeWindow window
