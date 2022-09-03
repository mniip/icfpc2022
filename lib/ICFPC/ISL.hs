module ICFPC.ISL where

import Codec.Picture
import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Monoid
import Data.Semigroup.Foldable
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Text.Read qualified as TR
import Data.Word

import ICFPC.Cost
import ICFPC.Pairs
import ICFPC.Render
import ICFPC.Tracer

newtype BlockId = BlockId (NE.NonEmpty Int)
  deriving (Eq, Ord, Show)

newtype Program = Program (NE.NonEmpty ProgramLine)
  deriving (Eq, Ord)

instance Show Program where
  show = show . TB.toLazyText . buildProgram

data ProgramLine
  = Comment !T.Text
  | Blank
  | Command !Command
  deriving (Eq, Ord, Show)

data Command
  = LCut !BlockId !Orientation !Int
  | PCut !BlockId !Int !Int
  | Color !BlockId !RGBA
  | Swap !BlockId !BlockId
  | Merge !BlockId !BlockId
  deriving (Eq, Ord)

instance Show Command where
  show = show . TB.toLazyText . buildCommand

buildParens :: NE.NonEmpty TB.Builder -> TB.Builder
buildParens xs = TB.singleton '[' <> intercalate1 (TB.singleton ',') xs <> TB.singleton ']'

buildOrientation :: Orientation -> TB.Builder
buildOrientation = buildParens . NE.singleton . \case
  X -> TB.singleton 'X'
  Y -> TB.singleton 'Y'

buildBlockId :: BlockId -> TB.Builder
buildBlockId (BlockId ids) = buildParens $ intercalateMap1 (TB.singleton '.') TB.decimal ids NE.:| []

buildInt :: Int -> TB.Builder
buildInt = buildParens . NE.singleton . TB.decimal

buildInt2 :: Int -> Int -> TB.Builder
buildInt2 x y = buildParens $ TB.decimal x NE.:| [TB.decimal y]

buildInt4 :: Word8 -> Word8 -> Word8 -> Word8 -> TB.Builder
buildInt4 x y z w = buildParens $ TB.decimal x NE.:| [TB.decimal y, TB.decimal z, TB.decimal w]

buildCommand :: Command -> TB.Builder
buildCommand = \case
  LCut block orient line -> TB.fromString "cut" <> buildBlockId block <> buildOrientation orient <> buildInt line
  PCut block x y -> TB.fromString "cut" <> buildBlockId block <> buildInt2 x y
  Color block rgba
    | (r, g, b, a) <- unpackRGBA rgba -> TB.fromString "color" <> buildBlockId block <> buildInt4 r g b a
  Swap block1 block2 -> TB.fromString "swap" <> buildBlockId block1 <> buildBlockId block2
  Merge block1 block2 -> TB.fromString "merge" <> buildBlockId block1 <> buildBlockId block2

buildProgramLine :: ProgramLine -> TB.Builder
buildProgramLine = \case
  Comment text -> TB.singleton '#' <> TB.fromText text
  Blank -> mempty
  Command cmd -> buildCommand cmd

buildProgram :: Program -> TB.Builder
buildProgram (Program xs) = intercalateMap1 (TB.singleton '\n') buildProgramLine xs

printProgram :: Program -> T.Text
printProgram = TL.toStrict . TB.toLazyText . buildProgram

newtype R a = R { runR :: TR.Reader a }
  deriving (Functor, Applicative, Alternative, Monad, MonadState T.Text, MonadError String)
    via (StateT T.Text (Except String))

pStr :: String -> R ()
pStr str = R $ \t -> case T.stripPrefix txt t of
  Nothing -> Left $ "Expected " <> str <> "\n"
  Just t' -> Right ((), t')
  where txt = T.pack str

pWs :: R a -> R a
pWs p = skipWs *> p <* skipWs
  where
    skipWs = R $ \t -> Right ((), T.dropWhile (\x -> x /= '\n' && isSpace x) t)

pComment :: R T.Text
pComment = R $ \t -> Right $ T.span (/= '\n') t

pParens :: R a -> R a
pParens p = pStr "[" *> pWs p <* pStr "]"

pBlockId :: R BlockId
pBlockId = pParens $ BlockId <$> (liftA2 (NE.:|) (R TR.decimal) $ many $ pStr "." *> R TR.decimal)

pInt :: R Int
pInt = pParens $ pWs (R TR.decimal)

pInt2 :: R (Int, Int)
pInt2 = pParens $ (,)
  <$> pWs (R TR.decimal)
  <*> (pStr "," *> pWs (R TR.decimal))

pInt4 :: R (Word8, Word8, Word8, Word8)
pInt4 = pParens $ (,,,)
  <$> pWs (R TR.decimal)
  <*> (pStr "," *> pWs (R TR.decimal))
  <*> (pStr "," *> pWs (R TR.decimal))
  <*> (pStr "," *> pWs (R TR.decimal))

pOrient :: R Orientation
pOrient = pParens $ (X <$ (pStr "X" <|> pStr "x")) <|> (Y <$ (pStr "Y" <|> pStr "y"))

pCommand :: R Command
pCommand = pWs $
  ((LCut <$ pStr "cut") <*> pWs pBlockId <*> pWs pOrient <*> pWs pInt)
  <|> ((mkPCut <$ pStr "cut") <*> pWs pBlockId <*> pWs pInt2)
  <|> ((Color <$ pStr "color") <*> pWs pBlockId <*> (packRGBA <$> pWs pInt4))
  <|> ((Swap <$ pStr "swap") <*> pWs pBlockId <*> pWs pBlockId)
  <|> ((Merge <$ pStr "merge") <*> pWs pBlockId <*> pWs pBlockId)
  where
    mkPCut blk (x, y) = PCut blk x y

pProgramLine :: R ProgramLine
pProgramLine = pWs $
  ((Comment <$ pStr "#") <*> pComment)
  <|> (Command <$> pCommand)
  <|> pure Blank

pProgram :: R Program
pProgram = Program <$> (liftA2 (NE.:|) pProgramLine $ many $ pStr "\n" *> pProgramLine)

tryParseProgram :: T.Text -> Either String Program
tryParseProgram t = case runR pProgram t of
  Left err -> Left err
  Right (r, t')
    | T.null t' -> Right r
    | otherwise -> Left $ "Garbage at end of input: " <> show t'

parseProgram :: T.Text -> Program
parseProgram t = case tryParseProgram t of
  Right r -> r
  Left err -> error $ "parseProgram: " <> err

data BState = BState
  { _bsFresh :: !Int
  , _bsBlockStates :: !(M.Map BlockId (XY (MinMax Int)))
  }

makeLenses ''BState

data InvalidCommand
  = BlockNotFound !BlockId
  | TooThinToCut !BlockId !Orientation !(MinMax Int)
  | CutLineNotInsideBlock !BlockId !Orientation !Int !(MinMax Int)
  | NotMergeable !BlockId !(XY (MinMax Int)) !BlockId !(XY (MinMax Int))
  | InvalidSwap !BlockId !(XY (MinMax Int)) !BlockId !(XY (MinMax Int))
  deriving (Eq, Ord, Show)

traceProgram :: MonadCommand m => XY Int -> Program -> m (Maybe (Int, InvalidCommand), BState)
traceProgram size (Program prog) = runStateT (go 0 $ NE.toList prog) initState
  where
    initState = BState
      { _bsFresh = 1
      , _bsBlockStates = M.singleton (BlockId $ 0 NE.:| []) (MinMax 0 <$> size)
      }
    go !_ [] = pure Nothing
    go lineNo (Comment _:ps) = go (lineNo + 1) ps
    go lineNo (Blank:ps) = go (lineNo + 1) ps
    go lineNo (Command cmd:ps) = runExceptT (traceCommand cmd) >>= \case
      Right _ -> go (lineNo + 1) ps
      Left err -> pure $ Just (lineNo, err)

traceCommand :: MonadCommand m => Command -> ExceptT InvalidCommand (StateT BState m) ()
traceCommand = \case
  LCut blk X x -> do
    XY xs ys <- getBlock blk
    xs' <- split x xs blk X
    zoom bsBlockStates $ do
      at (subBlock 0 blk) .= Just (XY (lower xs') ys)
      at (subBlock 1 blk) .= Just (XY (upper xs') ys)
      at blk .= Nothing
    lift . lift $ onXCut xs' ys
  LCut blk Y y -> do
    XY xs ys <- getBlock blk
    ys' <- split y ys blk Y
    zoom bsBlockStates $ do
      at (subBlock 0 blk) .= Just (XY xs (lower ys'))
      at (subBlock 1 blk) .= Just (XY xs (upper ys'))
      at blk .= Nothing
    lift . lift $ onYCut xs ys'
  PCut blk x y -> do
    XY xs ys <- getBlock blk
    xs' <- split x xs blk X
    ys' <- split y ys blk Y
    zoom bsBlockStates $ do
      at (subBlock 0 blk) .= Just (XY (lower xs') (lower ys'))
      at (subBlock 1 blk) .= Just (XY (upper xs') (lower ys'))
      at (subBlock 2 blk) .= Just (XY (upper xs') (upper ys'))
      at (subBlock 3 blk) .= Just (XY (lower xs') (upper ys'))
      at blk .= Nothing
    lift . lift $ onPCut (XY xs' ys')
  Color blk rgba -> do
    b <- getBlock blk
    lift . lift $ onColor b rgba
  Swap blk1 blk2 -> do
    b1 <- getBlock blk1
    b2 <- getBlock blk2
    if fmap mmLength b1 == fmap mmLength b2
    then do
      zoom bsBlockStates $ do
        at blk1 .= Just b2
        at blk2 .= Just b1
      lift . lift $ onSwap b1 b2
    else throwError $ InvalidSwap blk1 b1 blk2 b2
  Merge blk1 blk2 -> do
    b1@(XY xs1 ys1) <- getBlock blk1
    b2@(XY xs2 ys2) <- getBlock blk2
    fresh <- newFresh
    if
      | Just xs' <- merge ys1 ys2 xs1 xs2 -> do
        zoom bsBlockStates $ do
          at fresh .= Just (XY (outer xs') ys1)
          at blk1 .= Nothing
          at blk2 .= Nothing
        lift . lift $ onXMerge xs' ys1
      | Just ys' <- merge xs1 xs2 ys1 ys2 -> do
        zoom bsBlockStates $ do
          at fresh .= Just (XY xs1 (outer ys'))
          at blk1 .= Nothing
          at blk2 .= Nothing
        lift . lift $ onYMerge xs1 ys'
      | otherwise -> throwError $ NotMergeable blk1 b1 blk2 b2
  where
    getBlock blk = use (bsBlockStates . at blk) >>= \case
      Nothing -> throwError $ BlockNotFound blk
      Just b -> pure b
    newFresh = BlockId . (NE.:| []) <$> (bsFresh <+= 1)
    split p ps@(MinMax pmin pmax) blk orient
      | pmin + 1 >= pmax = throwError $ TooThinToCut blk orient ps
      | pmin < p && p < pmax = pure $ MinMedMax pmin p pmax
      | otherwise = throwError $ CutLineNotInsideBlock blk orient p ps
    merge p1 p2 (MinMax qmin1 qmax1) (MinMax qmin2 qmax2)
      | p1 /= p2 = Nothing
      | qmax1 == qmin2 = Just $ MinMedMax qmin1 qmax1 qmax2
      | qmax2 == qmin1 = Just $ MinMedMax qmin2 qmax2 qmax1
      | otherwise = Nothing
    subBlock x (BlockId xs) = BlockId $ xs <> NE.singleton x

programErrors :: XY Int -> Program -> Maybe (Int, InvalidCommand)
programErrors size prog = fst $ runIdentity $ traceProgram size prog

tryProgramRunCost :: XY Int -> Program -> Either (Int, InvalidCommand) Int
tryProgramRunCost size prog = case runCostT (traceProgram size prog) size of
  ((Just err, _), _) -> Left err
  ((Nothing, _), Sum cost) -> Right cost

programRunCost :: XY Int -> Program -> Int
programRunCost size prog = case tryProgramRunCost size prog of
  Right cost -> cost
  Left (line, err) -> error $ "On line " <> show line <> ": " <> show err

renderErrors :: XY Int -> Program -> (Image PixelRGBA8, Maybe (Int, InvalidCommand))
renderErrors size prog = case runRender size $ traceProgram size prog of
  ((mErr, _), image) -> (image, mErr)

renderProgram :: XY Int -> Program -> Image PixelRGBA8
renderProgram size prog = case renderErrors size prog of
  (_, Just (line, err)) -> error $ "On line " <> show line <> ": " <> show err
  (image, Nothing) -> image

tryRenderWithCost :: XY Int -> Program -> (Image PixelRGBA8, Either (Int, InvalidCommand) Int)
tryRenderWithCost size prog = case runRender size $ runCostT (traceProgram size prog) of
  (((Just err, _), _), image) -> (image, Left err)
  (((Nothing, _), Sum cost), image) -> (image, Right cost)

renderWithCost :: XY Int -> Program -> (Image PixelRGBA8, Int)
renderWithCost size prog = case tryRenderWithCost size prog of
  (_, Left (line, err)) -> error $ "On line " <> show line <> ": " <> show err
  (image, Right cost) -> (image, cost)

tryScoreProgram :: Image PixelRGBA8 -> Program -> Either (Int, InvalidCommand) Int
tryScoreProgram image prog = case tryRenderWithCost (XY (imageWidth image) (imageHeight image)) prog of
  (_, Left err) -> Left err
  (image', Right cost) -> Right $ cost + compareImages image image'

scoreProgram :: Image PixelRGBA8 -> Program -> Int
scoreProgram image prog = case tryScoreProgram image prog of
  Left (line, err) -> error $ "On line " <> show line <> ": " <> show err
  Right score -> score
