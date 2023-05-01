module Viewpoint
  ( Viewpoint
  , position, orientation, elevation
  , startingViewpoint
  , moveLeft, moveForward, moveUp, turnLeft, turnUp
  , viewpointCamera
  ) where


import Control.Lens (Lens', (^.), (%~))
import Data.Fixed (mod')

import Raylib.Util.Math ((|+|))
import qualified Raylib.Types       as RL
import qualified Raylib.Util.Lenses as RL



-- Position and orientation of a player's head;
-- basically a version of the Raylib camera better suited to my needs.
data Viewpoint = Viewpoint
  { _position :: RL.Vector3 -- ^ In meter. Z is the vertical coordinate.
                            --   Space is right-handed.
  , _orientation :: Float -- ^ In radian. North is pi/2, West is 0,
                          --   South is -pi/2, East is pi
  , _elevation :: Float -- ^ In radian. Looking towards the horizon is 0,
                        --   looking up is pi/2, looking down is -pi/2
  }


startingViewpoint :: Viewpoint
startingViewpoint = Viewpoint
  { _position = RL.Vector3 0 0 1.80
  , _orientation = 0
  , _elevation = 0
  }


-- | Move the player's position along the left-right axis
--   relative to their orientation in the horizontal plane.
--   Use a negative input value to move to the right.
moveLeft :: Float -> Viewpoint -> Viewpoint
moveLeft distance vp =
  let shiftX = (-distance) * sin (vp ^. orientation)
      shiftY =   distance  * cos (vp ^. orientation)
  in position %~ (|+| RL.Vector3 shiftX shiftY 0) $ vp

-- | Move the player's position along the front-back axis
--   relative to their orientation in the horizontal plane.
--   Use a negative input value to move backwards.
moveForward :: Float -> Viewpoint -> Viewpoint
moveForward distance vp = position %~ (|+| RL.Vector3 shiftX shiftY 0) $ vp
  where shiftX = distance * cos (vp ^. orientation)
        shiftY = distance * sin (vp ^. orientation)

-- | Move the player's position along the vertical axis.
--   Use a negative input value to move down.
moveUp :: Float -> Viewpoint -> Viewpoint
moveUp distance = position . RL._vector3'z %~ (+ distance)

-- | Turn the player's head around the pitch axis.
--   Use a negative input value to turn down.
turnUp :: Float -> Viewpoint -> Viewpoint
turnUp angle = normalizeViewpoint . (elevation %~ (+ angle))

-- | Change the player's orientation in the horizontal plane.
--   Use a negative input value to turn right.
turnLeft :: Float -> Viewpoint -> Viewpoint
turnLeft angle = normalizeViewpoint . (orientation %~ (+ angle))


-- | The Raylib camera corresponding to a given viewpoint:
viewpointCamera :: Viewpoint -> RL.Camera3D
viewpointCamera vp = RL.Camera3D
  { RL.camera3D'position   =  vp ^. position
  , RL.camera3D'target     = (vp ^. position) |+| targetVector vp
  , RL.camera3D'up         = upVector vp
  , RL.camera3D'fovy       = 90
  , RL.camera3D'projection = RL.CameraPerspective
  }


-- | The Raylib camera target corresponding to a givenviewpoint:
targetVector :: Viewpoint -> RL.Vector3
targetVector vp = RL.Vector3 (cos or * cos elev)
                             (sin or * cos elev)
                             (         sin elev)
  where or   = vp ^. orientation
        elev = vp ^. elevation

-- | The Raylib camera "up" vector corresponding to a givenviewpoint:
upVector :: Viewpoint -> RL.Vector3
upVector vp = RL.Vector3 ((-1) * cos or * sin elev)
                         ((-1) * sin or * sin elev)
                         (                cos elev)
  where or   = vp ^. orientation
        elev = vp ^. elevation



-- | Get angle to be in the (-pi, pi] range:
normalizeAngle :: Float -> Float
normalizeAngle angle = - (((-angle + pi) `mod'` (2*pi)) - pi)

-- | Normalize orientation, and make sure that elevation
--   is in the [-pi/2, pi/2] range:
normalizeViewpoint :: Viewpoint -> Viewpoint
normalizeViewpoint = (orientation %~ normalizeAngle) .
                     (elevation   %~ limitElev     )
  where limitElev elev = max (min elev (pi/2)) (-pi/2)



-- Lenses. Defining them manually here to void resorting to Template Haskell

position :: Lens' Viewpoint RL.Vector3
position f (Viewpoint pos or elev) =
  (\pos' -> Viewpoint pos' or elev) <$> f pos
{-# INLINE position #-}

orientation :: Lens' Viewpoint Float
orientation f (Viewpoint pos or elev) =
  (\or' -> Viewpoint pos or' elev) <$> f or
{-# INLINE orientation #-}

elevation :: Lens' Viewpoint Float
elevation f (Viewpoint pos or elev) =
  (\elev' -> Viewpoint pos or elev') <$> f elev
{-# INLINE elevation #-}
