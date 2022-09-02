module ICFPC.Render where

import Codec.Picture.Types
import Control.Monad.ST
import Control.Monad.Reader
import Data.Vector.Storable.Mutable qualified as V

import ICFPC.Tracer

newtype RenderM s a = RenderM { runRenderM :: MutableImage s PixelRGBA8 -> ST s a }
  deriving (Functor, Applicative, Monad)
    via (ReaderT (MutableImage s PixelRGBA8) (ST s))

instance MonadReader (XY Int) (RenderM s) where
  ask = RenderM $ \image -> pure (XY (mutableImageWidth image) (mutableImageHeight image))
  local = error "local"

instance MonadCommand (RenderM s) where
  onXCut _ _ = pure ()
  onYCut _ _ = pure ()
  onPCut _ = pure ()
  onColor (XY (MinMax xmin xmax) (MinMax ymin ymax)) (r, g, b, a) = do
    forM_ [ymin .. ymax - 1] $ \y ->
      forM_ [xmin .. xmax - 1] $ \x ->
        RenderM $ \image -> writePixel image x y rgba
    where
      rgba = PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  onSwap (XY (MinMax xmin1 xmax1) (MinMax ymin1 ymax1)) (XY (MinMax xmin2 _) (MinMax ymin2 _)) = do
    forM_ [0 .. ymax1 - ymin1 - 1] $ \dy ->
      forM [0 .. xmax1 - xmin1 - 1] $ \dx ->
        RenderM $ \image -> do
          c1 <- readPixel image (xmin1 + dx) (ymin1 + dy)
          c2 <- readPixel image (xmin2 + dx) (ymin2 + dy)
          writePixel image (xmin2 + dx) (ymin2 + dy) c1
          writePixel image (xmin1 + dx) (ymin1 + dy) c2
  onXMerge _ _ = pure ()
  onYMerge _ _ = pure ()

runRender :: XY Int -> (forall s. RenderM s a) -> (a, Image PixelRGBA8)
runRender (XY width height) f = runST $ do
  image <- newMutableImage width height
  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x ->
      writePixel image x y (PixelRGBA8 255 255 255 255)
  r <- runRenderM f image
  forM_ [0 .. (height - 1) `div` 2] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      c1 <- readPixel image x y
      c2 <- readPixel image x (height - 1 - y)
      writePixel image x y c2
      writePixel image x (height - 1 - y) c1
  (r,) <$> unsafeFreezeImage image
