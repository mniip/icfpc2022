module ICFPC.Pairs where

import Control.Monad.Reader.Class

type X a = a

type Y a = a

data XY a = XY !a !a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative XY where
  pure a = XY a a
  XY f g <*> XY x y = XY (f x) (g y)

instance Monad XY where
  return = pure
  XY x y >>= f = XY (case f x of XY x' _ -> x') (case f y of XY _ y' -> y')

data Orientation
  = X
  | Y
  deriving (Eq, Ord, Show)

instance MonadReader Orientation XY where
  ask = XY X Y
  local f (XY x y) = XY (case f X of X -> x; Y -> y) (case f Y of X -> x; Y -> y)

data MinMax a = MinMax !a !a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative MinMax where
  pure a = MinMax a a
  MinMax f g <*> MinMax x y = MinMax (f x) (g y)

instance Monad MinMax where
  return = pure
  MinMax x y >>= f = MinMax (case f x of MinMax x' _ -> x') (case f y of MinMax _ y' -> y')

data MinMedMax a = MinMedMax !a !a !a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative MinMedMax where
  pure a = MinMedMax a a a
  MinMedMax f g h <*> MinMedMax x y z = MinMedMax (f x) (g y) (h z)

instance Monad MinMedMax where
  return = pure
  MinMedMax x y z >>= f = MinMedMax
    (case f x of MinMedMax x' _ _ -> x')
    (case f y of MinMedMax _ y' _ -> y')
    (case f z of MinMedMax _ _ z' -> z')

lower, upper, outer :: MinMedMax a -> MinMax a
lower (MinMedMax a b _) = MinMax a b
upper (MinMedMax _ a b) = MinMax a b
outer (MinMedMax a _ b) = MinMax a b

mmLength :: Num a => MinMax a -> a
mmLength (MinMax a b) = b - a

xyArea :: Num a => XY a -> a
xyArea (XY a b) = a * b

between :: Ord a => a -> MinMax a -> Bool
between a (MinMax b c) = b <= a && a < c
