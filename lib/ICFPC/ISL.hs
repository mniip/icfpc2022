module ICFPC.ISL where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Char
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import Data.Semigroup.Foldable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Read as TR

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

data Orientation
  = X
  | Y
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
