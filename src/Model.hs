module Model where

-- High-level control
import Data.List (foldl')
import Control.Monad (foldM)
import Control.Monad.Reader (Reader, reader)
import Control.Lens ((%~))
-- Containers
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict ((!))
-- Filesystem
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, isExtensionOf, (</>), (<.>))
-- Raylib
import qualified Raylib.Types       as RL
import qualified Raylib.Core.Models as RLModels (loadModel, drawModel)
import Raylib.Util.Colors (white)
import Raylib.Core (loadShader)
import Raylib.Util (WindowResources, setMaterialShader)
import Raylib.Util.Lenses (_model'transform)
import Raylib.Util.Math (matrixRotate, (/*/))




type InContext a = Reader (ModelMap, ShaderMap) a

model :: String -> InContext RL.Model
model id = reader $ \(modelMap, _) -> modelMap ! id

shader :: String -> InContext RL.Shader
shader id = reader $ \(_, shaderMap) -> shaderMap ! id

(|*) :: RL.Model -> RL.Shader -> RL.Model
mod |* shad = setMaterialShader mod 1 shad


-------------
-- Shaders --
-------------

type ShaderMap = Map.HashMap String RL.Shader

-- | Looks for all shaders in a given directory and loads them.
--   Only shaders with extension ".vert" or ".frag" will be considered!
--   The result is a hash map containing all the shaders,
--   indexed by the basenames (i.e. the key of "shad.vert" is "shad").
loadShaderMap :: WindowResources
              -> FilePath
              -> IO ShaderMap
loadShaderMap window dir = do
  flagMap <- listShaders dir
  sequence $ Map.mapWithKey (loadShaders window dir) flagMap


-- | Makes a collection with all shader IDs in a given directory.
--   The resulting collection associates, to each ID, a pair of flags:
--   The first flag is `True` when there is a vertex shader with the given ID,
--   and the second flag is `True` when there is a fragment shader with the given ID.
listShaders :: FilePath -> IO (Map.HashMap String (Bool, Bool))
listShaders dir = do
  contents <- listDirectory dir
  return $ foldl' addID Map.empty contents

  where addID flagMap fileName = do
          let ident = takeBaseName fileName
          let v1 = "vert" `isExtensionOf` fileName
          let f1 = "frag" `isExtensionOf` fileName
          let (v2, f2) = flagMap ! ident
          Map.insert ident (v1||v2, f1||f2) flagMap


-- | Load shaders whose file name match the given ID
loadShaders :: WindowResources
            -> FilePath     -- ^ directory that contains our models
            -> String       -- ^ ID/basename of our shader
            -> (Bool, Bool) -- ^ (custom vertex shader?, custom fragment shader?)
            -> IO RL.Shader
loadShaders window shaderDir id (vFlag, fFlag) = do
  let vertexPath   = if vFlag then Just $ shaderDir </> id <.> "vert" else Nothing
  let fragmentPath = if fFlag then Just $ shaderDir </> id <.> "frag" else Nothing
  loadShader vertexPath fragmentPath window

type ModelMap = Map.HashMap String RL.Model





------------
-- Models --
------------



-- | Draw model at a given position
drawModel :: RL.Model
          -> RL.Vector3
          -> IO ()
drawModel m v = RLModels.drawModel m v 1 white


-- | Looks for all .glb files in a given directory and loads them.
--   The result is a hash map containing all the models,
--   indexed by the basenames (i.e. the key of "tree.glb" is "tree").
loadModelMap :: WindowResources
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
  fmap takeBaseName               .
  filter ("glb" `isExtensionOf`) <$>
  listDirectory dir


-- | Load a model whose file name matches the given ID
loadGLB :: WindowResources
        -> FilePath    -- ^ directory that contains our models
        -> String      -- ^ ID/basename of our model
        -> IO RL.Model
loadGLB window modelDir ident =
  -- Swap axes X, Y and Z because .glb files are not oriented the right way
  -- This is done by applying a rotation around the (1, 1, 1) axis
  (_model'transform %~ (matrixRotate (RL.Vector3 1 1 1) (2*pi/3) /*/)) <$>
  -- Load the model with given ID
  RLModels.loadModel (modelDir </> ident <.> "glb") window
