module ICFPC.Render where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import Control.Monad.Reader
import Data.Array.MArray as A
import Data.Array.ST as A
import Data.Array.Base as A
import Data.Word

import ICFPC.Tracer
import ICFPC.Pairs

newtype RenderM s a = RenderM { runRenderM :: (Int, Int, A.STUArray s Int Word32) -> ST s a }
  deriving (Functor, Applicative, Monad)
    via (ReaderT (Int, Int, A.STUArray s Int Word32) (ST s))

instance MonadReader (XY Int) (RenderM s) where
  ask = RenderM $ \(width, height, _) -> pure (XY width height)
  local = error "local"

instance MonadCommand (RenderM s) where
  onXCut _ _ = pure ()
  onYCut _ _ = pure ()
  onPCut _ = pure ()
  onColor (XY (MinMax xmin xmax) (MinMax ymin ymax)) rgba = do
    forM_ [ymin .. ymax - 1] $ \y -> RenderM $ \(width, _, arr) -> do
      let off = y * width
      forM_ [xmin .. xmax - 1] $ \x -> do
        A.unsafeWrite arr (off + x) rgba
  onSwap (XY (MinMax xmin1 xmax1) (MinMax ymin1 ymax1)) (XY (MinMax xmin2 _) (MinMax ymin2 _)) = do
    forM_ [0 .. ymax1 - ymin1 - 1] $ \dy -> RenderM $ \(width, _, arr) -> do
      let
        off1 = (ymin1 + dy) * width + xmin1
        off2 = (ymin2 + dy) * width + xmin2
      forM_ [0 .. xmax1 - xmin1 - 1] $ \dx -> do
        r1 <- A.unsafeRead arr (off1 + dx)
        r2 <- A.unsafeRead arr (off2 + dx)
        A.unsafeWrite arr (off2 + dx) r1
        A.unsafeWrite arr (off1 + dx) r2
  onXMerge _ _ = pure ()
  onYMerge _ _ = pure ()

vflipImage :: MutableImage s PixelRGBA8 -> ST s ()
vflipImage image = do
  forM_ [0 .. (height - 1) `div` 2] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      c1 <- readPixel image x y
      c2 <- readPixel image x (height - 1 - y)
      writePixel image x y c2
      writePixel image x (height - 1 - y) c1
  where
    width = mutableImageWidth image
    height = mutableImageHeight image

vflippedImage :: Image PixelRGBA8 -> Image PixelRGBA8
vflippedImage image = runST $ do
  image' <- thawImage image
  vflipImage image'
  unsafeFreezeImage image'

runRender :: XY Int -> (forall s. RenderM s a) -> (a, Image PixelRGBA8)
runRender (XY width height) f = runST $ do
  arr <- A.newArray (0, width * height - 1) $ packPixel $ PixelRGBA8 255 255 255 255
  r <- runRenderM f (width, height, arr)
  image <- newMutableImage width height
  forM_ [0 .. height - 1] $ \y -> do
    let
      off1 = (height - y - 1) * width
      off2 = mutablePixelBaseIndex image 0 y
    forM_ [0 .. width - 1] $ \x ->
      A.unsafeRead arr (off1 + x) >>= writePackedPixelAt image (off2 + x * componentCount (undefined :: PixelRGBA8)) . unpackPixel
  img <- unsafeFreezeImage image
  pure (r, img)

readImageRGBA8 :: String -> IO (Image PixelRGBA8)
readImageRGBA8 path = readImage path >>= \case
  Right (ImageRGBA8 image) -> pure image
  Right dyn -> pure $ convertRGBA8 dyn
  Left err -> error err
