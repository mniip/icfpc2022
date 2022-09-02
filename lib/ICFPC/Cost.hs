module ICFPC.Cost where

import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.Reader

import ICFPC.Tracer

newtype CostT m a = CostT { runCostT :: m (a, Sum Int) }
  deriving (Functor, Applicative, Monad, MonadWriter (Sum Int), MonadReader r)
    via (WriterT (Sum Int) m)
  deriving (MonadTrans)
    via (WriterT (Sum Int))

addCost :: MonadReader (XY Int) m => (Int -> Int) -> CostT m ()
addCost f = ask >>= \(XY width height) -> tell $ Sum $ f $ width * height

len :: MinMax Int -> Int
len (MinMax a b) = b - a

area :: XY (MinMax Int) -> Int
area (XY xs ys) = len xs * len ys

roundDiv :: Int -> Int -> Int
roundDiv x y = (x + (y `div` 2)) `div` y

instance (MonadReader (XY Int) m, MonadCommand m) => MonadCommand (CostT m) where
  onXCut xs ys = do
    addCost $ roundDiv $ 7 * area (XY (outer xs) ys)
    lift $ onXCut xs ys
  onYCut xs ys = do
    addCost $ roundDiv $ 7 * area (XY xs (outer ys))
    lift $ onYCut xs ys
  onPCut b@(XY xs ys) = do
    addCost $ roundDiv $ 10 * area (XY (outer xs) (outer ys))
    lift $ onPCut b
  onColor b rgba = do
    addCost $ roundDiv $ 5 * area b
    lift $ onColor b rgba
  onSwap b1 b2 = do
    addCost $ roundDiv $ 3 * area b1
    lift $ onSwap b1 b2
  onXMerge xs ys = do
    addCost $ roundDiv $ 1 * area (XY (outer xs) ys)
    lift $ onXMerge xs ys
  onYMerge xs ys = do
    addCost $ roundDiv $ 1 * area (XY xs (outer ys))
    lift $ onYMerge xs ys
