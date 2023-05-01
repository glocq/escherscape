-- | Keyboard input management
module Input (inputLoop) where

-- General Haskell symbols
import Control.Lens        ((^.), (%~))
import GHC.Float           (double2Float)
import Control.Monad.Loops (iterateM_)
import Control.Concurrent  (yield)
import qualified Control.Concurrent.STM as STM
-- Raylib symbols
import qualified Raylib.Core  as RL
import qualified Raylib.Types as RL
-- Internal symbols
import qualified Config
import qualified GameState as GS
import qualified Viewpoint as VP



-- | Conditionally apply a function. If the coondition is False,
--   we just apply identity.
--   This function exists in more recent versions of base (Data.Function)
applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False _ x = x


-- | The input loop listens to keyboard input
--   and updates the game state accordingly
inputLoop :: STM.TVar GS.GameState -> IO ()
inputLoop gs = RL.getTime >>= iterateM_ (inputIteration gs)


-- | Modify game state based on keyboard input.
--   Returns the time the action was run,
--   So the next run know how much time has passed.
inputIteration :: STM.TVar GS.GameState -> Double -> IO Double
inputIteration state formerTime = do
  -- Look up current time and compute elapsed time
  newTime <- RL.getTime
  let deltatime = double2Float $ newTime - formerTime
  -- Mutate state based on keyboard input
  mutateState <- processKeys deltatime
  STM.atomically . STM.modifyTVar state $ mutateState
  -- Allow other loops to do their thing.
  -- I don't fully understand how `yield` works,
  -- but omitting this call degrades performance drastically.
  yield
  -- Provide next iteration with the time measured here
  return newTime


-- | Modification of the game state based on keyboard input
processKeys :: Float -> IO (GS.GameState -> GS.GameState)
processKeys deltatime = do
  keyW      <- RL.isKeyDown RL.KeyW
  keyS      <- RL.isKeyDown RL.KeyS
  keyA      <- RL.isKeyDown RL.KeyA
  keyD      <- RL.isKeyDown RL.KeyD
  keySpace  <- RL.isKeyDown RL.KeySpace
  keyShiftL <- RL.isKeyDown RL.KeyLeftShift
  keyShiftR <- RL.isKeyDown RL.KeyRightShift
  keyUp     <- RL.isKeyDown RL.KeyUp
  keyDown   <- RL.isKeyDown RL.KeyDown
  keyLeft   <- RL.isKeyDown RL.KeyLeft
  keyRight  <- RL.isKeyDown RL.KeyRight
  return $ \gs ->
    let speed        = gs ^. GS.config . Config.speed
        angularSpeed = gs ^. GS.config . Config.angularSpeed
    in
    applyWhen keyW      (GS.viewpoint %~ VP.moveForward (         speed * deltatime)) .
    applyWhen keyS      (GS.viewpoint %~ VP.moveForward (       - speed * deltatime)) .
    applyWhen keyA      (GS.viewpoint %~ VP.moveLeft    (         speed * deltatime)) .
    applyWhen keyD      (GS.viewpoint %~ VP.moveLeft    (       - speed * deltatime)) .
    applyWhen keySpace  (GS.viewpoint %~ VP.moveUp      (         speed * deltatime)) .
    applyWhen keyShiftL (GS.viewpoint %~ VP.moveUp      (       - speed * deltatime)) .
    applyWhen keyShiftR (GS.viewpoint %~ VP.moveUp      (       - speed * deltatime)) .
    applyWhen keyUp     (GS.viewpoint %~ VP.turnUp      (  angularSpeed * deltatime)) .
    applyWhen keyDown   (GS.viewpoint %~ VP.turnUp      (- angularSpeed * deltatime)) .
    applyWhen keyLeft   (GS.viewpoint %~ VP.turnLeft    (  angularSpeed * deltatime)) .
    applyWhen keyRight  (GS.viewpoint %~ VP.turnLeft    (- angularSpeed * deltatime)) $
    gs
