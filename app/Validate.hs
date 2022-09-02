import Data.Functor.Identity
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.Cost
import ICFPC.ISL
import ICFPC.Pairs
import ICFPC.Tracer

main :: IO ()
main = map readMaybe <$> getArgs >>= \case
  [Just x, Just y] -> do
    t <- T.getContents
    case parseProgram t of
      Left err -> error err
      Right prog -> case runIdentity $ runTrace prog (XY x y) of
        (Nothing, _) -> pure ()
        (Just err, BState line _ _) -> error $ show err <> "\nOn line " <> show (line + 1)
          <> "\n" <> case prog of Program ls -> show $ ls NE.!! line
  _ -> error "Usage: ./validate <width> <height>"
