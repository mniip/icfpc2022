module ICFPC.ISL where

import Control.Lens
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.List.NonEmpty qualified as NE
import Data.Semigroup.Foldable
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TB
import Data.Text.Read qualified as TR
import Data.Map.Strict qualified as M

import ICFPC.Tracer
import ICFPC.Pairs

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
  | Color !BlockId !Int !Int !Int !Int
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

buildInt4 :: Int -> Int -> Int -> Int -> TB.Builder
buildInt4 x y z w = buildParens $ TB.decimal x NE.:| [TB.decimal y, TB.decimal z, TB.decimal w]

buildCommand :: Command -> TB.Builder
buildCommand = \case
  LCut block orient line -> TB.fromString "cut" <> buildBlockId block <> buildOrientation orient <> buildInt line
  PCut block x y -> TB.fromString "cut" <> buildBlockId block <> buildInt2 x y
  Color block r g b a -> TB.fromString "color" <> buildBlockId block <> buildInt4 r g b a
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

pInt4 :: R (Int, Int, Int, Int)
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
  <|> ((mkColor <$ pStr "color") <*> pWs pBlockId <*> pWs pInt4)
  <|> ((Swap <$ pStr "swap") <*> pWs pBlockId <*> pWs pBlockId)
  <|> ((Merge <$ pStr "merge") <*> pWs pBlockId <*> pWs pBlockId)
  where
    mkPCut blk (x, y) = PCut blk x y
    mkColor blk (r, g, b, a) = Color blk r g b a

pProgramLine :: R ProgramLine
pProgramLine = pWs $
  ((Comment <$ pStr "#") <*> pComment)
  <|> (Command <$> pCommand)
  <|> pure Blank

pProgram :: R Program
pProgram = Program <$> (liftA2 (NE.:|) pProgramLine $ many $ pStr "\n" *> pProgramLine)

parseProgram :: T.Text -> Either String Program
parseProgram t = case runR pProgram t of
  Left err -> Left err
  Right (r, t')
    | T.null t' -> Right r
    | otherwise -> Left $ "Garbage at end of input: " <> show t'

data BState = BState !Int !Int !(M.Map BlockId Block)

data InvalidCommand
  = BlockNotFound !BlockId
  | TooThinToCut !BlockId !Orientation !(MinMax Int)
  | CutLineNotInsideBlock !BlockId !Orientation !Int !(MinMax Int)
  | NotMergeable !BlockId !Block !BlockId !Block
  | InvalidSwap !BlockId !Block !BlockId !Block
  deriving (Eq, Ord, Show)

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
    if fmap mmLength b1 == fmap mmLength b2
    then do
      modifyMap
        $ M.insert blk1 b2
        . M.insert blk2 b1
      lift . lift $ onSwap b1 b2
    else throwError $ InvalidSwap blk1 b1 blk2 b2
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
    affixBlock :: Int -> BlockId -> BlockId
    affixBlock x (BlockId xs) = BlockId $ xs <> NE.singleton x

runTrace :: MonadCommand m => Program -> XY Int -> m (Maybe InvalidCommand, BState)
runTrace prog size = runStateT (runExceptT (traceProgram prog)) (initState size)
  <&> \case
    (Left err, st) -> (Just err, st)
    (Right _, st) -> (Nothing, st)
  where
    initState (XY x y) = BState 0 1 $ M.singleton (BlockId $ 0 NE.:| []) (XY (MinMax 0 x) (MinMax 0 y))
