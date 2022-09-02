module ICFPC.Tracer where

import Control.Lens
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans.State hiding (get, modify, state)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer hiding (tell)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import ICFPC.ISL

type X a = a
type Y a = a
data XY a = XY !a !a
  deriving (Eq, Ord, Show)
data MinMax a = MinMax !a !a
  deriving (Eq, Ord, Show)
data MinMedMax a = MinMedMax !a !a !a
  deriving (Eq, Ord, Show)

lower, upper, outer :: MinMedMax a -> MinMax a
lower (MinMedMax a b _) = MinMax a b
upper (MinMedMax _ a b) = MinMax a b
outer (MinMedMax a _ b) = MinMax a b

type Block = XY (MinMax Int)

data BState = BState !Int !Int !(M.Map BlockId Block)

type Blocks = M.Map BlockId Block

data InvalidCommand
  = BlockNotFound !BlockId
  | TooThinToCut !BlockId !Orientation !(MinMax Int)
  | CutLineNotInsideBlock !BlockId !Orientation !Int !(MinMax Int)
  | NotMergeable !BlockId !Block !BlockId !Block
  deriving (Eq, Ord, Show)

affixBlock :: Int -> BlockId -> BlockId
affixBlock x (BlockId xs) = BlockId $ xs <> NE.singleton x

class Monad m => MonadCommand m where
  onXCut :: X (MinMedMax Int) -> Y (MinMax Int) -> m ()
  onYCut :: X (MinMax Int) -> Y (MinMedMax Int) -> m ()
  onPCut :: XY (MinMedMax Int) -> m ()
  onColor :: XY (MinMax Int) -> (Int, Int, Int, Int) -> m ()
  onSwap :: XY (MinMax Int) -> XY (MinMax Int) -> m ()
  onXMerge :: X (MinMedMax Int) -> Y (MinMax Int) -> m ()
  onYMerge :: X (MinMax Int) -> Y (MinMedMax Int) -> m ()

initState :: XY Int -> BState
initState (XY x y) = BState 0 1 $ M.singleton (BlockId $ 0 NE.:| []) (XY (MinMax 0 x) (MinMax 0 y))

traceProgram :: MonadCommand m => Program -> ExceptT InvalidCommand (StateT BState m) ()
traceProgram (Program ps) = forM_ ps $ \pl -> do
  case pl of
    Command cmd -> traceCommand cmd
    _ -> pure ()
  modify $ \(BState line fresh bs) -> BState (line + 1) fresh bs

traceCommand :: MonadCommand m => Command -> ExceptT InvalidCommand (StateT BState m) ()
traceCommand = \case
  LCut blk X x -> do
    XY xs ys <- block blk
    xs' <- split x xs blk X
    modifyMap
      $ M.insert (affixBlock 0 blk) (XY (lower xs') ys)
      . M.insert (affixBlock 1 blk) (XY (upper xs') ys)
      . M.delete blk
    lift . lift $ onXCut xs' ys
  LCut blk Y y -> do
    XY xs ys <- block blk
    ys' <- split y ys blk Y
    modifyMap
      $ M.insert (affixBlock 0 blk) (XY xs (lower ys'))
      . M.insert (affixBlock 1 blk) (XY xs (upper ys'))
      . M.delete blk
    lift . lift $ onYCut xs ys'
  PCut blk x y -> do
    XY xs ys <- block blk
    xs' <- split x xs blk X
    ys' <- split y ys blk Y
    modifyMap
      $ M.insert (affixBlock 0 blk) (XY (lower xs') (lower ys'))
      . M.insert (affixBlock 1 blk) (XY (upper xs') (lower ys'))
      . M.insert (affixBlock 2 blk) (XY (upper xs') (upper ys'))
      . M.insert (affixBlock 3 blk) (XY (lower xs') (upper ys'))
      . M.delete blk
    lift . lift $ onPCut (XY xs' ys')
  Color blk r g b a -> do
    b' <- block blk
    lift . lift $ onColor b' (r, g, b, a)
  Swap blk1 blk2 -> do
    b1 <- block blk1
    b2 <- block blk2
    modifyMap
      $ M.insert blk1 b2
      . M.insert blk2 b1
    lift . lift $ onSwap b1 b2
  Merge blk1 blk2 -> do
    b1@(XY xs1 ys1) <- block blk1
    b2@(XY xs2 ys2) <- block blk2
    fresh <- newFresh
    if
      | Just xs' <- merge ys1 ys2 xs1 xs2 -> do
        modifyMap
          $ M.insert fresh (XY (outer xs') ys1)
          . M.delete blk1
          . M.delete blk2
        lift . lift $ onXMerge xs' ys1
      | Just ys' <- merge xs1 xs2 ys1 ys2 -> do
        modifyMap
          $ M.insert fresh (XY xs1 (outer ys'))
          . M.delete blk1
          . M.delete blk2
        lift . lift $ onYMerge xs1 ys'
      | otherwise -> throwError $ NotMergeable blk1 b1 blk2 b2
  where
    block blk = get >>= \(BState _ _ bs) -> case M.lookup blk bs of
      Nothing -> throwError $ BlockNotFound blk
      Just b -> pure b
    modifyMap f = modify $ \(BState line fresh bs) -> BState line fresh (f bs)
    newFresh = state $ \(BState line fresh bs) -> (BlockId $ fresh NE.:| [], BState line (fresh + 1) bs)
    split p ps@(MinMax pmin pmax) blk orient
      | pmin + 1 >= pmax = throwError $ TooThinToCut blk orient ps
      | pmin < p && p < pmax = pure $ MinMedMax pmin p pmax
      | otherwise = throwError $ CutLineNotInsideBlock blk orient p ps
    merge p1 p2 (MinMax qmin1 qmax1) (MinMax qmin2 qmax2)
      | p1 /= p2 = Nothing
      | qmax1 == qmin2 = Just $ MinMedMax qmin1 qmax1 qmax2
      | qmax2 == qmin1 = Just $ MinMedMax qmin2 qmax2 qmax1
      | otherwise = Nothing

instance MonadCommand Identity where
  onXCut _ _ = pure ()
  onYCut _ _ = pure ()
  onPCut _ = pure ()
  onColor _ _ = pure ()
  onSwap _ _ = pure ()
  onXMerge _ _ = pure ()
  onYMerge _ _ = pure ()

validate :: Program -> XY Int -> Maybe (InvalidCommand, Int)
validate prog size = case runStateT (runExceptT (traceProgram prog)) (initState size) of
  Identity (Left err, BState line _ _) -> Just (err, line)
  Identity (Right _, _) -> Nothing

newtype CostM a = CostM { runCostM :: Int -> (a, Int) }
  deriving (Functor, Applicative, Monad, MonadReader Int, MonadWriter (Sum Int))
    via (ReaderT Int (Writer (Sum Int)))

addCost :: (Int -> Int) -> CostM ()
addCost f = ask >>= \ar -> tell $ Sum $ f ar

len :: MinMax Int -> Int
len (MinMax a b) = b - a

area :: XY (MinMax Int) -> Int
area (XY xs ys) = len xs * len ys

roundDiv :: Int -> Int -> Int
roundDiv x y = (x + (y `div` 2)) `div` y

instance MonadCommand CostM where
  onXCut xs ys = addCost $ roundDiv $ 7 * area (XY (outer xs) ys)
  onYCut xs ys = addCost $ roundDiv $ 7 * area (XY xs (outer ys))
  onPCut (XY xs ys) = addCost $ roundDiv $ 10 * area (XY (outer xs) (outer ys))
  onColor b _ = addCost $ roundDiv $ 5 * area b
  onSwap b _ = addCost $ roundDiv $ 3 * area b
  onXMerge xs ys = addCost $ roundDiv $ 1 * area (XY (outer xs) ys)
  onYMerge xs ys = addCost $ roundDiv $ 1 * area (XY xs (outer ys))

cost :: Program -> XY Int -> Either (InvalidCommand, Int) Int
cost prog size@(XY width height)
  = case runCostM (runStateT (runExceptT (traceProgram prog)) (initState size)) (width * height) of
    ((Left err, BState line _ _), _) -> Left (err, line)
    ((Right _, _), cost) -> Right cost
