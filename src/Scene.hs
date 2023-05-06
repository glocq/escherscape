{-# LANGUAGE InstanceSigs #-}
module Scene where

-- General symbols
import Control.Lens (Lens', mapped, _2, (&), (^.), (<>~), (%~))
-- Containers
import Data.HashMap.Strict (HashMap, (!))
import Data.Vector (Vector, fromList)
-- Raylib
import Raylib.Util.Math ((/*/))
import qualified Raylib.Types       as RL
import qualified Raylib.Util        as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Util.Math   as RL
import qualified Raylib.Util.Colors as RL
import qualified Raylib.Util.Lenses as RL




newtype Scene = Scene { _contents :: Vector (RL.Model, RL.Matrix) }

instance Semigroup Scene where
  (<>) :: Scene -> Scene -> Scene
  sc1 <> sc2 = sc1 & contents <>~ (sc2 ^. contents)

instance Monoid Scene where
  mempty :: Scene
  mempty = Scene mempty



transformScene :: RL.Matrix -> Scene -> Scene
transformScene matrix scene = scene & contents -- get vector of (model, transformation) pairs
                                    . mapped   -- get to each item of the vector
                                    . _2       -- get second item of the (model, transformation) pair
                                    %~ (/*/ matrix) -- combine transform with already existing transforms


demoScene :: HashMap String RL.Model -> Scene
demoScene modelMap = Scene (fromList [(modelMap ! "tree", RL.matrixTranslate i j 0) | i <- [-10, -5 .. 10]
                                                                                    , j <- [-10, -5 .. 10]])


drawScene :: Scene -> IO ()
drawScene scene = mapM_ (uncurry drawSceneItem) $ scene ^. contents

drawSceneItem :: RL.Model -> RL.Matrix -> IO ()
drawSceneItem model transform = RL.drawModel model' RL.zero 1 RL.white
  where model' = model & RL._model'transform %~ (/*/ transform)


-- Lenses. Defining them manually here to void resorting to Template Haskell

contents :: Lens' Scene (Vector (RL.Model, RL.Matrix))
contents f (Scene cont) = (\cont' -> Scene cont') <$> f cont
{-# INLINE contents #-}
