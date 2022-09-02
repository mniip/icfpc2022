import Codec.Picture
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text.IO qualified as T
import System.Environment

import ICFPC.Cost
import ICFPC.ISL
import ICFPC.Pairs
import ICFPC.Render

main :: IO ()
main = do
  getArgs >>= \case
    [input] -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image) -> parseProgram <$> T.getContents >>= \case
        Left err -> error err
        Right prog -> do
          let
            width = imageWidth image
            height = imageHeight image
          case runRender (XY width height) $ runCostT (runTrace prog (XY width height)) of
            (((Nothing, _), Sum cost), image') -> do
              let diff = compareImages image image'
              putStrLn $ "Cost: " <> show cost
              putStrLn $ "Difference: " <> show diff
              putStrLn $ "Total: " <> show (cost + diff)
            (((Just err, BState line _ _), _), _) -> error $ show err <> "\nOn line " <> show (line + 1)
              <> "\n" <> case prog of Program ls -> show $ ls NE.!! line
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: score <input.png>"
