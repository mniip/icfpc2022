import Data.Functor.Identity
import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.ISL
import ICFPC.Pairs

scaleCom :: Int -> Command -> Command
scaleCom scale (LCut i o l) = LCut i o (scale*l)
scaleCom scale (PCut i x y) = PCut i (scale*x) (scale*y)
scaleCom _ other = other

scaleProgLine :: Int -> ProgramLine -> ProgramLine
scaleProgLine scale (Command cmd) = Command (scaleCom scale cmd)
scaleProgLine _ other = other

scaleProg :: Int -> Program -> Program
scaleProg scale (Program pls) = Program (NE.map (scaleProgLine scale) pls)

main :: IO ()
main = map readMaybe <$> getArgs >>= \case
  [Just scale] -> do
    t <- T.getContents
    case parseProgram t of
      Left err -> error err
      Right prog -> T.putStrLn $ printProgram (scaleProg scale prog)
  _ -> error "Usage: ./scale <scaling factor>"
