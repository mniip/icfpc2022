module ICFPC.Graph where

import Codec.Picture
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad.Except
import Data.Foldable
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.TypeNats
import Text.Read (readEither)

import ICFPC.Cost
import ICFPC.ISL qualified as I
import ICFPC.Pairs
import ICFPC.Render
import ICFPC.Tracer

import Debug.Trace

type NodeId = Int
type EndId = Int

data Node (n :: Nat) (m :: Nat) where
  Start :: Node 0 1
  XCut :: !(X Int) -> Node 1 2
  YCut :: !(Y Int) -> Node 1 2
  PCut :: !(X Int) -> !(Y Int) -> Node 1 4
  Color :: !RGBA -> Node 1 1
  Swap :: Node 2 2
  Merge :: Node 2 1

deriving instance Eq (Node n m)
deriving instance Ord (Node n m)
deriving instance Show (Node n m)

newtype End (up :: Bool) = End EndId
  deriving (Eq, Ord, Show)

data NodeData where
  NodeData :: !(Node n m) -> !(Wide n (End 'True)) -> !(Wide m (End 'False)) -> NodeData

{-# INLINE instantiateNodeData #-}
instantiateNodeData :: Node n m -> ((Foldable (Wide n), Applicative (Wide n), Foldable (Wide m), Applicative (Wide m)) => r) -> r
instantiateNodeData node k = case node of
  Start -> k
  XCut _ -> k
  YCut _ -> k
  PCut _ _ -> k
  Color _ -> k
  Swap -> k
  Merge -> k

data EdgeData = EdgeData
  { _edSource :: !NodeId
  , _edTarget :: !NodeId
  } deriving (Eq, Ord, Show)

makeLenses ''EdgeData

data Graph = Graph
  { _gFreshNodeId :: !NodeId
  , _gFreshEndId :: !EndId
  , _gNodes :: !(M.Map NodeId NodeData)
  , _gEnds :: !(M.Map EndId NodeId)
  , _gUp :: !(M.Map (End 'True) (End 'False))
  , _gDown :: !(M.Map (End 'False) (End 'True))
  }

makeLenses ''Graph

emptyGraph :: Graph
emptyGraph = Graph
  { _gFreshNodeId = 0
  , _gFreshEndId = 0
  , _gNodes = M.empty
  , _gEnds = M.empty
  , _gUp = M.empty
  , _gDown = M.empty
  }

insertNode :: Node n m -> Graph -> (NodeId, Wide n (End 'True), Wide m (End 'False), Graph)
insertNode node graph = case mkData node of
  (df, ends, ups, downs) ->
    ( i
    , ups
    , downs
    , graph'
      & gNodes . at i .~ Just (NodeData node ups downs)
      & gFreshEndId .~ f + df
      & gEnds %~ M.union (M.fromList $ (,i) <$> ends)
    )
  where
    i = graph ^. gFreshNodeId
    f = graph ^. gFreshEndId
    graph' = graph & gFreshNodeId .~ i + 1

    mkData :: Node n m -> (Int, [EndId], Wide n (End 'True), Wide m (End 'False))
    mkData = \case
      Start -> go Wide0 (Wide1 (End f))
      XCut _ -> go (Wide1 (End f)) (Wide2 (End $ f + 1) (End $ f + 2))
      YCut _ -> go (Wide1 (End f)) (Wide2 (End $ f + 1) (End $ f + 2))
      PCut _ _ -> go (Wide1 (End f)) (Wide4 (End $ f + 1) (End $ f + 2) (End $ f + 3) (End $ f + 4))
      Color _ -> go (Wide1 (End f)) (Wide1 (End $ f + 1))
      Swap -> go (Wide2 (End f) (End $ f + 1)) (Wide2 (End $ f + 2) (End $ f + 3))
      Merge -> go (Wide2 (End f) (End $ f + 1)) (Wide1 (End $ f + 2))
      where
        go ups downs =
          ( length ups + length downs
          , (toList ups <&> \(End e) -> e) ++ (toList downs <&> \(End e) -> e)
          , ups
          , downs )

modifyNode :: (Applicative t, Traversable t) => NodeId -> (forall n m. Node n m -> t (Node n m)) -> Graph -> t Graph
modifyNode i f graph
  = graph & gNodes . at i . _Just %%~ \(NodeData node ups downs) -> f node <&> \node' -> NodeData node' ups downs

getGraphNode :: Graph -> NodeId -> NodeData
getGraphNode graph i = (graph ^. gNodes) M.! i

getGraphEdgeUp :: Graph -> End 'True -> Maybe (End 'False)
getGraphEdgeUp graph u = M.lookup u (graph ^. gUp)

getGraphEdgeDown :: Graph -> End 'False -> Maybe (End 'True)
getGraphEdgeDown graph d = M.lookup d (graph ^. gDown)

endToNode :: Graph -> End b -> NodeId
endToNode graph (End e) = (graph ^. gEnds) M.! e

data DeletedNodeData where
  DeletedNodeData :: !(Node n m) -> !(Wide n (Maybe (End 'False))) -> !(Wide m (Maybe (End 'True))) -> DeletedNodeData

deleteEdgeUp :: End 'True -> Graph -> Maybe (End 'False, Graph)
deleteEdgeUp u graph = case graph ^. gUp . at u of
  Nothing -> Nothing
  Just d -> Just (d, graph & gUp . at u .~ Nothing & gDown . at d .~ Nothing)

deleteEdgeDown :: End 'False -> Graph -> Maybe (End 'True, Graph)
deleteEdgeDown d graph = case graph ^. gDown . at d of
  Nothing -> Nothing
  Just u -> Just (u, graph & gDown . at d .~ Nothing & gUp . at u .~ Nothing)

deleteNode :: NodeId -> Graph -> (DeletedNodeData, Graph)
deleteNode i graph = case mkData $ (graph ^. gNodes) M.! i of
  (downs, ups, dd) -> (dd, foldl' pruneUp (foldl' pruneDown graph downs) ups)
  where
    mkData (NodeData node ups downs) = instantiateNodeData node $
      ( toList downs
      , toList ups
      , DeletedNodeData node (getGraphEdgeUp graph <$> ups) (getGraphEdgeDown graph <$> downs)
      ) -- traverse?
    pruneUp graph u = pruneEnd u $ case graph ^. gUp . at u of
      Nothing -> graph
      Just d -> graph & gUp . at u .~ Nothing & gDown . at d .~ Nothing
    pruneDown graph d = pruneEnd d $ case graph ^. gDown . at d of
      Nothing -> graph
      Just u -> graph & gDown . at d .~ Nothing & gUp . at u .~ Nothing
    pruneEnd (End e) graph = graph & gEnds . at e .~ Nothing

insertEdge :: End 'False -> End 'True -> Graph -> Graph
insertEdge d u graph = graph & gUp . at u .~ Just d & gDown . at d .~ Just u

insertDownwards :: Wide n (Maybe (End 'False)) -> Node n m -> Graph -> (Wide m (End 'False), Graph)
insertDownwards downs node graph = case insertNode node graph of
  (_, ups, downs', graph') -> instantiateNodeData node $
    (downs', foldl' connect graph' $ toList $ (,) <$> downs <*> ups)
  where
    connect g (Just d, u) = insertEdge d u g
    connect g (Nothing, _) = g

insertUpwards :: Wide m (Maybe (End 'True)) -> Node n m -> Graph -> (Wide n (End 'True), Graph)
insertUpwards ups node graph = case insertNode node graph of
  (_, ups', downs, graph') -> instantiateNodeData node $
    (ups', foldl' connect graph' $ toList $ (,) <$> downs <*> ups)
  where
    connect g (d, Just u) = insertEdge d u g
    connect g (_, Nothing) = g

instance Show Graph where
  show graph = "{-\n" <> intercalate "\n" (showND <$> M.toAscList (graph ^. gNodes)) <> " -}"
    where
      showND (i, NodeData node ups downs) = instantiateNodeData node $
        intercalate " " (showE . getGraphEdgeUp graph <$> toList ups)
          <> " -> " <> show i <> ":" <> show node <> " -> "
          <> intercalate " " (showE . getGraphEdgeDown graph <$> toList downs)
      showE :: Maybe (End b) -> String
      showE = maybe "?" show . fmap (endToNode graph)

type ReversedId = NE.NonEmpty Int

toReverse :: I.BlockId -> ReversedId
toReverse (I.BlockId blk) = NE.reverse blk

fromReverse :: ReversedId -> I.BlockId
fromReverse blk = I.BlockId $ NE.reverse blk

data UnfoldState = UnfoldState
  { _usFreshISL :: !Int
  , _usRename :: M.Map ReversedId (End 'False)
  , _usGraph :: Graph
  }
  deriving (Show)

makeLenses ''UnfoldState

fromISL :: I.Program -> Graph
fromISL (I.Program prog) = view usGraph $ execState (mapM_ goLine prog) initState
  where
    initState = case insertDownwards Wide0 Start emptyGraph of
      (Wide1 d, graph) -> UnfoldState
        { _usFreshISL = 0
        , _usRename = M.singleton (NE.singleton 0) d
        , _usGraph = graph
        }
    useName blk = use (usRename . at blk) >>= \case
      Just r -> do
        usRename . at blk .= Nothing
        pure r
      Nothing -> error $ "Unknown block: " <> show blk
    nextFreshISL = NE.singleton <$> (usFreshISL <+= 1)
    goLine (I.Comment _) = pure ()
    goLine I.Blank = pure ()
    goLine (I.Command cmd) = go cmd
    go = \case
      I.LCut (toReverse -> blk) orient l -> do
        p <- useName blk
        Wide2 d1 d2 <- usGraph %%= insertDownwards (Wide1 (Just p)) ((case orient of X -> XCut; Y -> YCut) l)
        usRename . at (0 NE.<| blk) .= Just d1
        usRename . at (1 NE.<| blk) .= Just d2
      I.PCut (toReverse -> blk) x y -> do
        p <- useName blk
        Wide4 d1 d2 d3 d4 <- usGraph %%= insertDownwards (Wide1 (Just p)) (PCut x y)
        usRename . at (0 NE.<| blk) .= Just d1
        usRename . at (1 NE.<| blk) .= Just d2
        usRename . at (2 NE.<| blk) .= Just d3
        usRename . at (3 NE.<| blk) .= Just d4
      I.Color (toReverse -> blk) rgba -> do
        p <- useName blk
        Wide1 d <- usGraph %%= insertDownwards (Wide1 (Just p)) (Color rgba)
        usRename . at blk .= Just d
      I.Swap (toReverse -> blk1) (toReverse -> blk2) -> do
        p1 <- useName blk1
        p2 <- useName blk2
        Wide2 d1 d2 <- usGraph %%= insertDownwards (Wide2 (Just p1) (Just p2)) Swap
        usRename . at blk1 .= Just d2
        usRename . at blk2 .= Just d1
      I.Merge (toReverse -> blk1) (toReverse -> blk2) -> do
        p1 <- useName blk1
        p2 <- useName blk2
        Wide1 d <- usGraph %%= insertDownwards (Wide2 (Just p1) (Just p2)) Merge
        w <- nextFreshISL
        usRename . at w .= Just d

reachableFrom :: NodeId -> Graph -> S.Set NodeId
reachableFrom initial graph = execState (go initial) S.empty
  where
    go i = get >>= \s -> if i `S.member` s
      then pure ()
      else do
        put (S.insert i s)
        mapM_ go $ getDowns $ (graph ^. gNodes) M.! i
    getDowns (NodeData node _ downs) = instantiateNodeData node $
      map (endToNode graph) . mapMaybe (getGraphEdgeDown graph) $ toList downs

topoSort :: Graph -> [NodeId]
topoSort graph = (`appEndo` []) . execWriter $ evalStateT (mapM_ go $ M.keys $ graph ^. gNodes) S.empty
  where
    go i = get >>= \s -> if i `S.member` s
      then pure ()
      else do
        put (S.insert i s)
        mapM_ go $ getUps $ (graph ^. gNodes) M.! i
        tell $ Endo (i:)
    getUps (NodeData node ups _) = instantiateNodeData node $
      map (endToNode graph) . mapMaybe (getGraphEdgeUp graph) $ toList ups

topoRank :: Graph -> M.Map NodeId Int
topoRank graph = execState (forM_ (topoSort graph) go) M.empty
  where
    go i = case getGraphNode graph i of
      NodeData node ups _ -> instantiateNodeData node $ modify $ \m ->
        let hs = mapMaybe (`M.lookup` m) . map (endToNode graph) . mapMaybe (getGraphEdgeUp graph) $ toList ups
        in M.insert i (maximum $ 0 : map (1 +) hs) m

data FoldState = FoldState
  { _fsFresh :: !Int
  , _fsRename :: !(M.Map (End 'False) ReversedId)
  }

makeLenses ''FoldState

toISL :: Graph -> I.Program
toISL graph = I.Program . fmap I.Command . NE.fromList . catMaybes
  $ evalState (forM (filter (`S.member` reachable) $ topoSort graph) go) initState
  where
    reachable = reachableFrom 0 graph
    initState :: FoldState
    initState = case getGraphNode graph 0 of
      NodeData Start _ (Wide1 down) -> FoldState
        { _fsFresh = 0
        , _fsRename = M.singleton down $ NE.singleton 0
        }
      NodeData node _ _ -> error $ "Expected Start at 0, got " <> show node
    useName :: End 'True -> State FoldState ReversedId
    useName u = case getGraphEdgeUp graph u of
      Nothing -> error $ "No edge up from " <> show (endToNode graph u)
      Just d -> fsRename . at d %%= \case
        Nothing -> error $ "End of " <> show (endToNode graph d) <> " not marked"
        Just i -> (i, Nothing)
    nextFresh :: State FoldState ReversedId
    nextFresh = NE.singleton <$> (fsFresh <+= 1)
    rename i name = fsRename . at i .= Just name
    go i = case getGraphNode graph i of
      NodeData Start _ _ -> pure Nothing
      NodeData (XCut x) (Wide1 u) (Wide2 d1 d2) -> do
        blk <- useName u
        rename d1 (0 NE.<| blk)
        rename d2 (1 NE.<| blk)
        pure $ Just $ I.LCut (fromReverse blk) X x
      NodeData (YCut y) (Wide1 u) (Wide2 d1 d2) -> do
        blk <- useName u
        rename d1 (0 NE.<| blk)
        rename d2 (1 NE.<| blk)
        pure $ Just $ I.LCut (fromReverse blk) Y y
      NodeData (PCut x y) (Wide1 u) (Wide4 d1 d2 d3 d4) -> do
        blk <- useName u
        rename d1 (0 NE.<| blk)
        rename d2 (1 NE.<| blk)
        rename d3 (2 NE.<| blk)
        rename d4 (3 NE.<| blk)
        pure $ Just $ I.PCut (fromReverse blk) x y
      NodeData (Color rgba) (Wide1 u) (Wide1 d) -> do
        blk <- useName u
        rename d blk
        pure $ Just $ I.Color (fromReverse blk) rgba
      NodeData Swap (Wide2 u1 u2) (Wide2 d1 d2) -> do
        blk1 <- useName u1
        blk2 <- useName u2
        rename d1 blk2
        rename d2 blk1
        pure $ Just $ I.Swap (fromReverse blk1) (fromReverse blk2)
      NodeData Merge (Wide2 u1 u2) (Wide1 d) -> do
        blk1 <- useName u1
        blk2 <- useName u2
        blk <- nextFresh
        rename d blk
        pure $ Just $ I.Merge (fromReverse blk1) (fromReverse blk2)

type NState = M.Map (End 'False) (XY (MinMax Int))

data InvalidNode
  = NodeNotConnected !(End 'True)
  | NodeNotFound !(End 'False)
  | TooThinToCut !(End 'True) !Orientation !(MinMax Int)
  | CutLineNotInsideBlock !(End 'True) !Orientation !Int !(MinMax Int)
  | NotMergeable !(End 'True) !(XY (MinMax Int)) !(End 'True) !(XY (MinMax Int))
  | StartNotFound
  deriving (Eq, Ord, Show)

traceNode :: MonadCommand m => Graph -> NodeData -> ExceptT InvalidNode (StateT NState m) ()
traceNode graph = \case
  NodeData Start _ _ -> pure ()
  NodeData (XCut x) (Wide1 u) (Wide2 d1 d2) -> do
    XY xs ys <- useBlock u
    xs' <- split x xs u X
    storeBlock d1 $ XY (lower xs') ys
    storeBlock d2 $ XY (upper xs') ys
    lift . lift $ onXCut xs' ys
  NodeData (YCut y) (Wide1 u) (Wide2 d1 d2) -> do
    XY xs ys <- useBlock u
    ys' <- split y ys u Y
    storeBlock d1 $ XY xs (lower ys')
    storeBlock d2 $ XY xs (upper ys')
    lift . lift $ onYCut xs ys'
  NodeData (PCut x y) (Wide1 u) (Wide4 d1 d2 d3 d4) -> do
    XY xs ys <- useBlock u
    xs' <- split x xs u X
    ys' <- split y ys u Y
    storeBlock d1 $ XY (lower xs') (lower ys')
    storeBlock d2 $ XY (upper xs') (lower ys')
    storeBlock d3 $ XY (upper xs') (upper ys')
    storeBlock d4 $ XY (lower xs') (upper ys')
    lift . lift $ onPCut (XY xs' ys')
  NodeData (Color rgba) (Wide1 u) (Wide1 d) -> do
    b <- useBlock u
    storeBlock d b
    lift . lift $ onColor b rgba
  NodeData Swap (Wide2 u1 u2) (Wide2 d1 d2) -> do
    b1 <- useBlock u1
    b2 <- useBlock u2
    storeBlock d1 b2
    storeBlock d2 b1
    lift . lift $ onSwap b1 b2
  NodeData Merge (Wide2 u1 u2) (Wide1 d) -> do
    b1@(XY xs1 ys1) <- useBlock u1
    b2@(XY xs2 ys2) <- useBlock u2
    if
      | Just xs' <- merge ys1 ys2 xs1 xs2 -> do
        storeBlock d $ XY (outer xs') ys1
        lift . lift $ onXMerge xs' ys1
      | Just ys' <- merge xs1 xs2 ys1 ys2 -> do
        storeBlock d $ XY xs1 (outer ys')
        lift . lift $ onYMerge xs1 ys'
      | otherwise -> throwError $ NotMergeable u1 b1 u2 b2
  where
    useBlock :: Monad m => End 'True -> ExceptT InvalidNode (StateT NState m) (XY (MinMax Int))
    useBlock u = case getGraphEdgeUp graph u of
      Nothing -> throwError $ NodeNotConnected u
      Just d -> use (at d) >>= \case
        Nothing -> throwError $ NodeNotFound d
        Just b -> do
          at d .= Nothing
          pure b
    storeBlock d b = at d .= Just b
    split p ps@(MinMax pmin pmax) u orient
      | pmin + 1 >= pmax = throwError $ TooThinToCut u orient ps
      | pmin < p && p < pmax = pure $ MinMedMax pmin p pmax
      | otherwise = throwError $ CutLineNotInsideBlock u orient p ps
    merge p1 p2 (MinMax qmin1 qmax1) (MinMax qmin2 qmax2)
      | p1 /= p2 = Nothing
      | qmax1 == qmin2 = Just $ MinMedMax qmin1 qmax1 qmax2
      | qmax2 == qmin1 = Just $ MinMedMax qmin2 qmax2 qmax1
      | otherwise = Nothing

traceGraph :: MonadCommand m => XY Int -> Graph -> m (M.Map NodeId InvalidNode, NState)
traceGraph size graph = case getGraphNode graph 0 of
  NodeData Start _ (Wide1 d) -> runStateT (execWriterT $ mapM_ go $ topoSort graph) $ M.singleton d (MinMax 0 <$> size)
  _ -> pure (M.singleton 0 StartNotFound, M.empty)
  where
    go :: MonadCommand m => NodeId -> WriterT (M.Map NodeId InvalidNode) (StateT NState m) ()
    go i = lift (runExceptT $ traceNode graph $ getGraphNode graph i) >>= \case
      Left err -> tell $ M.singleton i err
      Right _ -> pure ()

graphErrors :: XY Int -> Graph -> [(NodeId, InvalidNode)]
graphErrors size graph = M.toList $ fst $ runIdentity $ traceGraph size graph

tryGraphRunCost :: XY Int -> Graph -> Either (M.Map NodeId InvalidNode) Int
tryGraphRunCost size graph = case runCostT (traceGraph size graph) size of
  ((errs, _), Sum cost)
    | M.null errs -> Right cost
    | otherwise -> Left errs

graphRunCost :: XY Int -> Graph -> Int
graphRunCost size graph = case tryGraphRunCost size graph of
  Right cost -> cost
  Left errs -> error $ intercalate "\n" $ map (\(i, err) -> "Node " <> show i <> ": " <> show err) $ M.toList errs

renderErrors :: XY Int -> Graph -> (Image PixelRGBA8, M.Map NodeId InvalidNode)
renderErrors size graph = case runRender size $ traceGraph size graph of
  ((errs, _), image) -> (image, errs)

renderGraph :: XY Int -> Graph -> Image PixelRGBA8
renderGraph size graph = case renderErrors size graph of
  (image, errs)
    | M.null errs -> image
    | otherwise -> error $ intercalate "\n" $ map (\(i, err) -> "Node " <> show i <> ": " <> show err) $ M.toList errs

tryRenderWithCost :: XY Int -> Graph -> (Image PixelRGBA8, Either (M.Map NodeId InvalidNode) Int)
tryRenderWithCost size graph = case runRender size $ runCostT (traceGraph size graph) of
  (((errs, _), Sum cost), image)
    | M.null errs -> (image, Right cost)
    | otherwise -> (image, Left errs)

renderWithCost :: XY Int -> Graph -> (Image PixelRGBA8, Int)
renderWithCost size graph = case tryRenderWithCost size graph of
  (_, Left errs) -> error $ intercalate "\n" $ map (\(i, err) -> "Node " <> show i <> ": " <> show err) $ M.toList errs
  (image, Right cost) -> (image, cost)

tryScoreGraph :: Image PixelRGBA8 -> Graph -> Either (M.Map NodeId InvalidNode) Int
tryScoreGraph image graph = case tryRenderWithCost (XY (imageWidth image) (imageHeight image)) graph of
  (_, Left err) -> Left err
  (image', Right cost) -> Right $ cost + compareImages image image'

scoreGraph :: Image PixelRGBA8 -> Graph -> Int
scoreGraph image graph = case tryScoreGraph image graph of
  Left errs -> error $ intercalate "\n" $ map (\(i, err) -> "Node " <> show i <> ": " <> show err) $ M.toList errs
  Right score -> score
