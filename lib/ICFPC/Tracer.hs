module ICFPC.Tracer where

import Codec.Picture.Types
import Data.Functor.Identity
import Data.Word

import ICFPC.Pairs

type RGBA = Word32

packRGBA :: (Word8, Word8, Word8, Word8) -> RGBA
packRGBA (r, g, b, a) = packPixel $ PixelRGBA8 r g b a

unpackRGBA :: RGBA -> (Word8, Word8, Word8, Word8)
unpackRGBA rgba = case unpackPixel rgba of
  PixelRGBA8 r g b a -> (r, g, b, a)

class Monad m => MonadCommand m where
  onXCut :: X (MinMedMax Int) -> Y (MinMax Int) -> m ()
  onYCut :: X (MinMax Int) -> Y (MinMedMax Int) -> m ()
  onPCut :: XY (MinMedMax Int) -> m ()
  onColor :: XY (MinMax Int) -> RGBA -> m ()
  onSwap :: XY (MinMax Int) -> XY (MinMax Int) -> m ()
  onXMerge :: X (MinMedMax Int) -> Y (MinMax Int) -> m ()
  onYMerge :: X (MinMax Int) -> Y (MinMedMax Int) -> m ()

instance MonadCommand Identity where
  onXCut _ _ = pure ()
  onYCut _ _ = pure ()
  onPCut _ = pure ()
  onColor _ _ = pure ()
  onSwap _ _ = pure ()
  onXMerge _ _ = pure ()
  onYMerge _ _ = pure ()

instance MonadCommand ((->) r) where
  onXCut _ _ = pure ()
  onYCut _ _ = pure ()
  onPCut _ = pure ()
  onColor _ _ = pure ()
  onSwap _ _ = pure ()
  onXMerge _ _ = pure ()
  onYMerge _ _ = pure ()
