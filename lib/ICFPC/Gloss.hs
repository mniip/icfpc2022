module ICFPC.Gloss where

import Codec.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Picture
import Data.Vector.Storable qualified as V

import ICFPC.Pairs
import ICFPC.Tracer

-- scaled to fit in [-1..1]x[-1..1]
imageToGloss :: Image PixelRGBA8 -> Picture
imageToGloss image = Scale (2 / fromIntegral dimen) (2 / fromIntegral dimen)
  $ bitmapOfForeignPtr
    (imageWidth image)
    (imageHeight image)
    (BitmapFormat TopToBottom PxRGBA)
    (fst $ V.unsafeToForeignPtr0 $ imageData image)
    True
  where dimen = imageWidth image `max` imageHeight image

toGlossCoords :: XY Int -> Int -> Float
toGlossCoords (XY width height) x = 2 / fromIntegral dimen * fromIntegral x - 1
  where dimen = width `max` height

fromGlossCoords :: XY Int -> Float -> Float
fromGlossCoords (XY width height) x = (x + 1) / 2 * fromIntegral dimen
  where dimen = width `max` height

wireframe :: XY (MinMax Float) -> Picture
wireframe (XY (MinMax xmin xmax) (MinMax ymin ymax)) = Line
  [ (xmin, ymin)
  , (xmin, ymax)
  , (xmax, ymax)
  , (xmax, ymin)
  , (xmin, ymax)
  ]
