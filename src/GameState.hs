module GameState
  ( GameState
  , viewpoint, config
  , initialGameState
  ) where

import Control.Lens (Lens')
import Viewpoint (Viewpoint, startingViewpoint)
import Config (Config, defaultConfig)


-- | Game state; contains all state related to the game logic.
--   Does not include e.g. stuff directly related to graphics.
data GameState = GameState
  { _viewpoint :: Viewpoint
  , _config    :: Config
  }

-- | State at the beginning of the game
initialGameState :: GameState
initialGameState = GameState startingViewpoint defaultConfig


-- Lenses. Defining them manually here to void resorting to Template Haskell

viewpoint :: Lens' GameState Viewpoint
viewpoint f (GameState vp conf) = (\vp' -> GameState vp' conf) <$> f vp
{-# INLINE viewpoint #-}

config :: Lens' GameState Config
config f (GameState vp conf) = (\conf' -> GameState vp conf') <$> f conf
{-# INLINE config #-}
