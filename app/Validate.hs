import qualified Data.List.NonEmpty as NE
import qualified Data.Text.IO as T
import System.Environment
import Text.Read

import ICFPC.ISL
import ICFPC.Tracer

main :: IO ()
main = map readMaybe <$> getArgs >>= \case
  [Just x, Just y] -> do
    t <- T.getContents
    case parseProgram t of
      Left err -> error err
      Right prog -> case cost prog (XY x y) of
        Right cost -> putStrLn $ "Program cost was: " <> show cost
        Left (err, line) -> error $ show err <> "\nOn line " <> show (line + 1)
          <> "\n" <> case prog of Program ls -> show $ ls NE.!! line
  _ -> error "Usage: ./validate <width> <height>"
