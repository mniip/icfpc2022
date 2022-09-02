module ICFPC.Tracer where

import Data.Functor.Identity

import ICFPC.Pairs

type Block = XY (MinMax Int)

class Monad m => MonadCommand m where
  onXCut :: X (MinMedMax Int) -> Y (MinMax Int) -> m ()
  onYCut :: X (MinMax Int) -> Y (MinMedMax Int) -> m ()
  onPCut :: XY (MinMedMax Int) -> m ()
  onColor :: XY (MinMax Int) -> (Int, Int, Int, Int) -> m ()
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
