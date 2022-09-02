module ICFPC.ISL where

import qualified Data.List.NonEmpty as NE
import Data.Semigroup.Foldable
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB

newtype BlockId = BlockId (NE.NonEmpty Int)
  deriving (Eq, Ord, Show)

newtype Program = Program (NE.NonEmpty ProgramLine)

data ProgramLine
  = Comment !T.Text
  | Blank
  | Command !Command

data Orientation
  = X
  | Y

data Command
  = LCut !BlockId !Orientation !Int
  | PCut !BlockId !Int !Int
  | Color !BlockId !Int !Int !Int !Int
  | Swap !BlockId !BlockId
  | Merge !BlockId !BlockId

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
