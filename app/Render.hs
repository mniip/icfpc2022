import Codec.Picture
import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as T
import System.Environment
import Text.Read

import ICFPC.ISL
import ICFPC.Pairs
import ICFPC.Render

main :: IO ()
main = do
  getArgs >>= \case
    [sWidth, sHeight, output]
      | Just width <- readMaybe sWidth
      , Just height <- readMaybe sHeight
      -> parseProgram <$> T.getContents >>= \case
        Left err -> error err
        Right prog -> case runRender (XY width height) (runTrace prog (XY width height)) of
          ((Nothing, _), image) -> savePngImage output $ ImageRGBA8 image
          ((Just err, BState line _ _), _) -> error $ show err <> "\nOn line " <> show (line + 1)
            <> "\n" <> case prog of Program ls -> show $ ls NE.!! line
    _ -> error "Usage: render <width> <height> <output.png>"
