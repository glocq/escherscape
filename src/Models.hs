-- | Load all models in a given directory,
--   and collect them in a HashMap indexed by the file basenames.
module Models (ModelMap, drawModel, loadModelMap) where

import qualified System.Directory    as Dir
import qualified System.FilePath     as FP
import qualified Data.HashMap.Strict as Map
import qualified Raylib.Types        as RL
import qualified Raylib.Util         as RL
import qualified Raylib.Core.Models  as RL
import qualified Raylib.Util.Math    as RL
import qualified Raylib.Util.Lenses  as RL
import qualified Raylib.Util.Colors  as RL

import Control.Monad (foldM)
import System.FilePath ((</>), (<.>))
import Control.Lens ((%~))
import Raylib.Util.Math ((/*/))



type ModelMap = Map.HashMap String RL.Model



-- | Draw model at a given position
drawModel :: RL.Model
          -> RL.Vector3
          -> IO ()
drawModel m v = RL.drawModel m v 1 RL.white


-------------------
-- Model loading --
-------------------

-- | Looks for all .glb files in a given directory and loads them.
--   The result is a hash map containing all the models,
--   indexed by the basenames (i.e. the key of "tree.glb" is "tree").
loadModelMap :: RL.WindowResources
             -> FilePath -- ^ The directory containing our models
             -> IO ModelMap
loadModelMap window modelDir = do
  ids <- listGLB modelDir
  foldM
    (\modelMap ident -> do
        newModel <- loadGLB window modelDir ident
        return $ Map.insert ident newModel modelMap)
    Map.empty ids


-- | List IDs of all .glb models in the given directory
listGLB :: FilePath -> IO [String]
listGLB dir =
  fmap FP.takeBaseName               .
  filter ("glb" `FP.isExtensionOf`) <$>
  Dir.listDirectory dir


-- | Load a model whose file name matches the given ID
loadGLB :: RL.WindowResources
        -> FilePath    -- ^ directory that contains our models
        -> String      -- ^ ID/basename of our model
        -> IO RL.Model
loadGLB window modelDir ident =
  -- Swap axes X, Y and Z because .glb files are not oriented the right way
  -- This is done by applying a rotation around the (1, 1, 1) axis
  (RL._model'transform %~ (RL.matrixRotate (RL.Vector3 1 1 1) (2*pi/3) /*/)) <$>
  -- Load the model with given ID
  RL.loadModel (modelDir </> ident <.> "glb") window
