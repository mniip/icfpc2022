import Codec.Picture
import Data.Text.IO qualified as T
import System.Environment

import ICFPC.Cost
import ICFPC.ISL
import ICFPC.Pairs
import ICFPC.Render

main :: IO ()
main = getArgs >>= \case
  [input] -> do
    image <- readImageRGBA8 input
    prog <- parseProgram <$> T.getContents
    case renderWithCost (XY (imageWidth image) (imageHeight image)) prog of
      (image', cost) -> do
        let diff = compareImages image image'
        putStrLn $ show (cost + diff) <> " (" <> show cost <> " + " <> show diff <> ")"
  _ -> error "Usage: score <input.png>"
