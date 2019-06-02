{-# language PatternSynonyms #-}
module Graphics.FreeType.Math
( Angle
, pattern ANGLE_PI
, pattern ANGLE_2PI
, pattern ANGLE_PI2
, pattern ANGLE_PI4
, angleDiff

, Fixed(..)
-- , mulFix
-- , divFix

, Matrix(..)
, matrixMultiply
, matrixInvert

, Vector(..)
, vectorTransform
-- , vectorUnit
-- , vectorRotate
-- , vectorLength
-- , vectorPolarize
-- , vectorFromPolar
) where

import Graphics.FreeType.Internal
import Numeric.Fixed
