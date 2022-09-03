import Codec.Picture
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.ISL
import ICFPC.Pairs

main :: IO ()
main = getArgs >>= \case
  [sWidth, sHeight, output]
    | Just width <- readMaybe sWidth
    , Just height <- readMaybe sHeight -> do
      prog <- parseProgram <$> T.getContents
      savePngImage output $ ImageRGBA8 $ renderProgram (XY width height) prog
  _ -> error "Usage: render <width> <height> <output.png>"
