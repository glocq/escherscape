module Shaders (ShaderMap, loadShaderMap, (|*)) where

import Data.List (foldl')
import System.FilePath ((</>), (<.>))
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as Map
import qualified System.Directory    as Dir
import qualified System.FilePath     as FP
import qualified Raylib.Util  as RL
import qualified Raylib.Types as RL
import qualified Raylib.Core  as RL



type ShaderMap = Map.HashMap String RL.Shader

-- | Apply shader to model
(|*) :: RL.Model -> RL.Shader -> RL.Model
model |* shader = RL.setMaterialShader model 1 shader

-- | Looks for all shaders in a given directory and loads them.
--   Only shaders with extension ".vert" or ".frag" will be considered!
--   The result is a hash map containing all the shaders,
--   indexed by the basenames (i.e. the key of "shad.vert" is "shad").
loadShaderMap :: RL.WindowResources
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
  contents <- Dir.listDirectory dir
  return $ foldl' addID Map.empty contents

  where addID flagMap fileName = do
          let ident = FP.takeBaseName fileName
          let v1 = "vert" `FP.isExtensionOf` fileName
          let f1 = "frag" `FP.isExtensionOf` fileName
          let (v2, f2) = flagMap ! ident
          Map.insert ident (v1||v2, f1||f2) flagMap


-- | Load shaders whose file name match the given ID
loadShaders :: RL.WindowResources
            -> FilePath     -- ^ directory that contains our models
            -> String       -- ^ ID/basename of our shader
            -> (Bool, Bool) -- ^ (custom vertex shader?, custom fragment shader?)
            -> IO RL.Shader
loadShaders window shaderDir id (vFlag, fFlag) = do
  let vertexPath   = if vFlag then Just $ shaderDir </> id <.> "vert" else Nothing
  let fragmentPath = if fFlag then Just $ shaderDir </> id <.> "frag" else Nothing
  RL.loadShader vertexPath fragmentPath window
