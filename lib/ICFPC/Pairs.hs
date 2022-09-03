module ICFPC.Pairs where

import GHC.TypeNats
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

data family Wide (n :: Nat) (a :: *) :: *

data instance Wide 0 a = Wide0
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative (Wide 0) where
  pure _ = Wide0
  _ <*> _ = Wide0

instance Monad (Wide 0) where
  return = pure
  _ >>= _ = Wide0

newtype instance Wide 1 a = Wide1 a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative (Wide 1) where
  pure a = Wide1 a
  Wide1 f <*> Wide1 x = Wide1 (f x)

instance Monad (Wide 1) where
  return = pure
  Wide1 x >>= f = Wide1 (case f x of Wide1 x' -> x')

data instance Wide 2 a = Wide2 !a !a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative (Wide 2) where
  pure a = Wide2 a a
  Wide2 f g <*> Wide2 x y = Wide2 (f x) (g y)

instance Monad (Wide 2) where
  return = pure
  Wide2 x y >>= f = Wide2 (case f x of Wide2 x' _ -> x') (case f y of Wide2 _ y' -> y')

data instance Wide 4 a = Wide4 !a !a !a !a
  deriving (Eq, Ord, Show, Functor, Foldable)

instance Applicative (Wide 4) where
  pure a = Wide4 a a a a
  Wide4 f g h k <*> Wide4 x y z w = Wide4 (f x) (g y) (h z) (k w)

instance Monad (Wide 4) where
  return = pure
  Wide4 x y z w >>= f = Wide4
    (case f x of Wide4 x' _ _ _ -> x')
    (case f y of Wide4 _ y' _ _ -> y')
    (case f z of Wide4 _ _ z' _ -> z')
    (case f w of Wide4 _ _ _ w' -> w')
