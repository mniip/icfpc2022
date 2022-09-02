module ICFPC.Graph where

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Word

import ICFPC.ISL qualified as I
import ICFPC.Pairs
import ICFPC.Tracer hiding (InvalidCommand(..))

type Id = Int

data Node
  = XCut !Id !(X Int) {- -} !Id !Id
  | YCut !Id !(Y Int) {- -} !Id !Id
  | PCut !Id !(XY Int) {- -} !Id !Id !Id !Id
  | Color !Id !Word8 !Word8 !Word8 !Word8 {- -} !Id
  | Swap !Id !Id {- -} !Id !Id
  | Merge !Id !Id {- -} !Id
  deriving (Eq, Ord, Show)

-- Invariants:
-- gLinked, gDanglingDown, gDanglingUp are disjoint
-- keys of gDown are exactly gLinked + gDanglingUp
-- keys of gUp are exactly gLinked + gDanglingDown
data Graph = Graph
  { gLinked :: !(S.Set Id)
  , gDanglingDown :: !(S.Set Id)
  , gDanglingUp :: !(S.Set Id)
  , gDown :: !(M.Map Id Node)
  , gUp :: !(M.Map Id Node)
  }
  deriving (Show)

nodeId :: Node -> Id
nodeId = \case
  XCut i _ _ _ -> i
  YCut i _ _ _ -> i
  PCut i _ _ _ _ _ -> i
  Color i _ _ _ _ _ -> i
  Swap i _ _ _ -> i
  Merge i _ _ -> i

nodeSources :: Node -> S.Set Id
nodeSources = \case
  XCut i1 _ _ _ -> S.singleton i1
  YCut i1 _ _ _ -> S.singleton i1
  PCut i1 _ _ _ _ _ -> S.singleton i1
  Color i1 _ _ _ _ _ -> S.singleton i1
  Swap i1 i2 _ _ -> S.fromList [i1, i2]
  Merge i1 i2 _ -> S.fromList [i1, i2]

nodeTargets :: Node -> S.Set Id
nodeTargets = \case
  XCut _ _ i1 i2 -> S.fromList [i1, i2]
  YCut _ _ i1 i2 -> S.fromList [i1, i2]
  PCut _ _ i1 i2 i3 i4 -> S.fromList [i1, i2, i3, i4]
  Color _ _ _ _ _ i1 -> S.singleton i1
  Swap _ _ i1 i2 -> S.fromList [i1, i2]
  Merge _ _ i1 -> S.singleton i1

emptyGraph :: Graph
emptyGraph = Graph
  { gLinked = S.empty
  , gDanglingDown = S.empty
  , gDanglingUp = S.empty
  , gDown = M.empty
  , gUp = M.empty
  }

freshId :: Graph -> Id
freshId Graph{..} = 1 + (fromMaybe 0 (S.lookupMax gLinked)
  `max` fromMaybe 0 (S.lookupMax gDanglingDown)
  `max` fromMaybe 0 (S.lookupMax gDanglingUp))

addNode :: Node -> Graph -> Graph
addNode node Graph{..}
  | not (edges `S.disjoint` gLinked)
  = error $ "Nodes already linked: " <> show (S.toList $ edges `S.intersection` gLinked)
  | not (sources `S.disjoint` gDanglingUp)
  = error $ "Connecting sources to sources: " <> show (S.toList $ sources `S.intersection` gDanglingUp)
  | not (targets `S.disjoint` gDanglingDown)
  = error $ "Connecting targets to targets: " <> show (S.toList $ targets `S.intersection` gDanglingDown)
  | otherwise
  = Graph
    { gLinked = gLinked `S.union` linkedUp `S.union` linkedDown
    , gDanglingDown = (gDanglingDown `S.difference` linkedUp) `S.union` notLinkedDown
    , gDanglingUp = (gDanglingUp `S.difference` linkedDown) `S.union` notLinkedUp
    , gDown = gDown `M.union` M.fromSet (const node) sources
    , gUp = gUp `M.union` M.fromSet (const node) targets
    }
  where
    !sources = nodeSources node
    !targets = nodeTargets node
    !edges = sources `S.union` targets
    !linkedUp = sources `S.intersection` gDanglingDown
    !notLinkedUp = sources `S.difference` gDanglingDown
    !linkedDown = targets `S.intersection` gDanglingUp
    !notLinkedDown = targets `S.difference` gDanglingUp

type ReversedId = NE.NonEmpty Int

toReverse :: I.BlockId -> ReversedId
toReverse (I.BlockId blk) = NE.reverse blk

fromReverse :: ReversedId -> I.BlockId
fromReverse blk = I.BlockId $ NE.reverse blk

data UnfoldState = UnfoldState
  { usFreshGraph :: !Int
  , usFreshISL :: !Int
  , usRename :: M.Map ReversedId Id
  , usGraph :: Graph
  }
  deriving (Show)

fromISL :: I.Program -> Graph
fromISL (I.Program prog) = usGraph $ foldl' goLine initState prog
  where
    initState = UnfoldState
      { usFreshGraph = 1
      , usFreshISL = 1
      , usRename = M.singleton (NE.singleton 0) 0
      , usGraph = emptyGraph
      }
    goLine us (I.Comment _) = us
    goLine us I.Blank = us
    goLine us (I.Command cmd) = go us cmd
    go us@UnfoldState{..} = \case
      I.LCut (toReverse -> blk) orient l
        | p <- usRename M.! blk
        , f <- usFreshGraph -> us
        { usFreshGraph = f + 2
        , usRename = usRename & M.insert (0 NE.<| blk) f & M.insert (1 NE.<| blk) (f + 1)
        , usGraph = usGraph & addNode ((case orient of I.X -> XCut; I.Y -> YCut) p l f (f + 1))
        }
      I.PCut (toReverse -> blk) x y
        | p <- usRename M.! blk
        , f <- usFreshGraph -> us
        { usFreshGraph = f + 4
        , usRename = usRename & M.insert (0 NE.<| blk) f & M.insert (1 NE.<| blk) (f + 1) & M.insert (2 NE.<| blk) (f + 2) & M.insert (3 NE.<| blk) (f + 3)
        , usGraph = usGraph & addNode (PCut p (XY x y) f (f + 1) (f + 2) (f + 3))
        }
      I.Color (toReverse -> blk) r g b a
        | p <- usRename M.! blk
        , f <- usFreshGraph -> us
        { usFreshGraph = f + 1
        , usRename = usRename & M.insert blk f
        , usGraph = usGraph & addNode (Color p (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a) f)
        }
      I.Swap (toReverse -> blk1) (toReverse -> blk2)
        | p1 <- usRename M.! blk1
        , p2 <- usRename M.! blk2
        , f <- usFreshGraph -> us
        { usFreshGraph = f + 2
        , usRename = usRename & M.insert blk1 (f + 1) & M.insert blk2 f
        , usGraph = usGraph & addNode (Swap p1 p2 f (f + 1))
        }
      I.Merge (toReverse -> blk1) (toReverse -> blk2)
        | p1 <- usRename M.! blk1
        , p2 <- usRename M.! blk2
        , f <- usFreshGraph -> us
        { usFreshGraph = f + 1
        , usFreshISL = usFreshISL + 1
        , usRename = usRename & M.delete blk1 & M.delete blk2 & M.insert (NE.singleton usFreshISL) f
        , usGraph = usGraph & addNode (Merge p1 p2 f)
        }

topoSort :: Graph -> [Node]
topoSort Graph{..} = (`appEndo` []) . execWriter $ evalStateT (forM_ gDanglingDown go) S.empty
  where
    go tgt = case M.lookup tgt gUp of
      Nothing -> pure ()
      Just node
        | i <- nodeId node -> get >>= \s -> if i `S.member` s
          then pure ()
          else do
            put (S.insert i s)
            forM_ (nodeSources node) go
            tell $ Endo (node :)

data FoldState = FoldState
  { fsFresh :: !Int
  , fsRename :: !(M.Map Id ReversedId)
  }

toISL :: Graph -> I.Program
toISL graph = case S.minView $ gDanglingUp graph of
  Nothing -> error "No nodes dangling up"
  Just (source, rest)
    | not (S.null rest) -> error $ "Multiple nodes dangling up, which one to start with? "
      <> show (S.toList $ gDanglingUp graph)
    | otherwise -> I.Program . fmap I.Command . NE.fromList
      $ evalState (forM (topoSort graph) go) (initState source)
  where
    initState source = FoldState
      { fsFresh = 1
      , fsRename = M.singleton source $ NE.singleton 0
      }
    useName t = state $ \fs -> (fsRename fs M.! t, fs { fsRename = M.delete t $ fsRename fs })
    nextFresh = state $ \fs -> (fsFresh fs, fs { fsFresh = fsFresh fs + 1 })
    rename t blk = modify $ \fs -> fs { fsRename = M.insert t blk $ fsRename fs }
    go = \case
      XCut s x t1 t2 -> do
        blk <- useName s
        rename t1 (0 NE.<| blk)
        rename t2 (1 NE.<| blk)
        pure $ I.LCut (fromReverse blk) I.X x
      YCut s y t1 t2 -> do
        blk <- useName s
        rename t1 (0 NE.<| blk)
        rename t2 (1 NE.<| blk)
        pure $ I.LCut (fromReverse blk) I.Y y
      PCut s (XY x y) t1 t2 t3 t4 -> do
        blk <- useName s
        rename t1 (0 NE.<| blk)
        rename t2 (1 NE.<| blk)
        rename t3 (2 NE.<| blk)
        rename t4 (3 NE.<| blk)
        pure $ I.PCut (fromReverse blk) x y
      Color s r g b a t -> do
        blk <- useName s
        rename t blk
        pure $ I.Color (fromReverse blk) (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
      Swap s1 s2 t1 t2 -> do
        blk1 <- useName s1
        blk2 <- useName s2
        rename t1 blk2
        rename t2 blk1
        pure $ I.Swap (fromReverse blk1) (fromReverse blk2)
      Merge s1 s2 t -> do
        blk1 <- useName s1
        blk2 <- useName s2
        blk <- nextFresh
        rename t (NE.singleton blk)
        pure $ I.Merge (fromReverse blk1) (fromReverse blk2)

type NState = M.Map Id Block

data InvalidNode
  = NodeNotFound !Id
  | TooThinToCut !Id !I.Orientation !(MinMax Int)
  | CutLineNotInsideBlock !Id !I.Orientation !Int !(MinMax Int)
  | NotMergeable !Id !Block !Id !Block
  deriving (Eq, Ord, Show)

traceNode :: MonadCommand m => Node -> ExceptT InvalidNode (StateT NState m) ()
traceNode = \case
  XCut s x t1 t2 -> do
    XY xs ys <- block s
    xs' <- split x xs s I.X
    modify
      $ M.insert t1 (XY (lower xs') ys)
      . M.insert t2 (XY (upper xs') ys)
    lift . lift $ onXCut xs' ys
  YCut s y t1 t2 -> do
    XY xs ys <- block s
    ys' <- split y ys s I.Y
    modify
      $ M.insert t1 (XY xs (lower ys'))
      . M.insert t2 (XY xs (upper ys'))
    lift . lift $ onYCut xs ys'
  PCut s (XY x y) t1 t2 t3 t4 -> do
    XY xs ys <- block s
    xs' <- split x xs s I.X
    ys' <- split y ys s I.Y
    modify
      $ M.insert t1 (XY (lower xs') (lower ys'))
      . M.insert t2 (XY (upper xs') (lower ys'))
      . M.insert t3 (XY (upper xs') (upper ys'))
      . M.insert t4 (XY (lower xs') (upper ys'))
    lift . lift $ onPCut (XY xs' ys')
  Color s r g b a t -> do
    b' <- block s
    modify $ M.insert t b'
    lift . lift $ onColor b' (fromIntegral r, fromIntegral g, fromIntegral b, fromIntegral a)
  Swap s1 s2 t1 t2 -> do
    b1 <- block s1
    b2 <- block s2
    modify
      $ M.insert t1 b2
      . M.insert t2 b1
    lift . lift $ onSwap b1 b2
  Merge s1 s2 t -> do
    b1@(XY xs1 ys1) <- block s1
    b2@(XY xs2 ys2) <- block s2
    if
      | Just xs' <- merge ys1 ys2 xs1 xs2 -> do
        modify $ M.insert t (XY (outer xs') ys1)
        lift . lift $ onXMerge xs' ys1
      | Just ys' <- merge xs1 xs2 ys1 ys2 -> do
        modify $ M.insert t (XY xs1 (outer ys'))
        lift . lift $ onYMerge xs1 ys'
      | otherwise -> throwError $ NotMergeable s1 b1 s2 b2
  where
    block i = get >>= \m -> case M.lookup i m of
      Nothing -> throwError $ NodeNotFound i
      Just b -> pure b
    split p ps@(MinMax pmin pmax) blk orient
      | pmin + 1 >= pmax = throwError $ TooThinToCut blk orient ps
      | pmin < p && p < pmax = pure $ MinMedMax pmin p pmax
      | otherwise = throwError $ CutLineNotInsideBlock blk orient p ps
    merge p1 p2 (MinMax qmin1 qmax1) (MinMax qmin2 qmax2)
      | p1 /= p2 = Nothing
      | qmax1 == qmin2 = Just $ MinMedMax qmin1 qmax1 qmax2
      | qmax2 == qmin1 = Just $ MinMedMax qmin2 qmax2 qmax1
      | otherwise = Nothing
