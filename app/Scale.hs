import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.ISL

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
main = getArgs >>= \case
  [sScale]
    | Just scale <- readMaybe sScale -> do
      T.putStrLn . printProgram . scaleProg scale . parseProgram =<< T.getContents
  _ -> error "Usage: ./scale <scaling factor>"
