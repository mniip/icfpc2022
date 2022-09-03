import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.ISL
import ICFPC.Pairs

main :: IO ()
main = getArgs >>= \case
  [sX, sY]
    | Just x <- readMaybe sX
    , Just y <- readMaybe sY -> do
      prog@(Program ls) <- parseProgram <$> T.getContents
      case programErrors (XY x y) prog of
        Just (line, err) -> error $ show err <> "\nOn line " <> show (line + 1) <> "\n" <> show (ls NE.!! line)
        Nothing -> pure ()
  _ -> error "Usage: ./validate <width> <height>"
