module Config
  ( Config
  , speed, angularSpeed
  , defaultConfig
  ) where

import Control.Lens (Lens')

-- | Our main type
data Config = Config
  { _speed        :: Float -- ^ player translation speed (in m/s)
  , _angularSpeed :: Float -- ^ player head rotation speed (in rad/s)
  }

-- | Reasonable default values;
--   Can be changed using the lenses
defaultConfig :: Config
defaultConfig = Config
  { _speed        = 1.5
  , _angularSpeed = 2
  }


-- Lenses. Defining them manually here to void resorting to Template Haskell

speed :: Lens' Config Float
speed f (Config s as) = (\s' -> Config s' as) <$> f s
{-# INLINE speed #-}

angularSpeed :: Lens' Config Float
angularSpeed f (Config s as) = (\as' -> Config s as') <$> f as
{-# INLINE angularSpeed #-}
