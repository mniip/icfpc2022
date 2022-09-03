import Codec.Picture
import Control.Lens
import Control.Monad.State
import Data.List
import Data.Monoid
import Data.Ord
import Data.Maybe
import Data.Text.IO qualified as T
import System.Environment
import System.IO
import Text.Read

import ICFPC.Graph
import ICFPC.ISL qualified as I
import ICFPC.Pairs
import ICFPC.Cost
import ICFPC.Render

main :: IO ()
main = do
  getArgs >>= \case
    [input, output] -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image') -> do
        let
          image = vflippedImage image'
          width = imageWidth image
          height = imageHeight image
 
          go graph = do
            let
              Just (_, cost) = tryCost graph
              graphs = mapMaybe tryCost $ concat $ shrinkNode graph <$> topoSort graph
              tryCost g = case runRender (XY width height) $ runCostT (runTrace g (XY width height)) of
                (((Nothing, _), Sum cost), image'') -> Just $ (g, cost + compareImages image' image'')
                (((Just e, _), _), _) -> Nothing

            case graphs of
              [] -> do
                hPutStrLn stderr "No shrinks"
              (minimumBy (comparing snd) -> (g, cost'))
                | cost' < cost -> do
                  hPutStrLn stderr $ "Improvement: " <> show cost'
                  T.writeFile output $ I.printProgram $ toISL g
                  go g
                | otherwise -> do
                  hPutStrLn stderr "No improvement"

        I.parseProgram <$> T.readFile output >>= \case
          Left err -> error err
          Right prog -> go $ fromISL prog
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: local <input.png> <inout.isl>"

shrinkNode :: Graph -> Node -> [Graph]
shrinkNode graph = \case
  n@(XCut s x t1 t2) ->
    [ updateNode n (XCut s (x - 1) t1 t2) graph
    , updateNode n (XCut s (x + 1) t1 t2) graph
    ]
  n@(YCut s y t1 t2) ->
    [ updateNode n (YCut s (y - 1) t1 t2) graph
    , updateNode n (YCut s (y + 1) t1 t2) graph
    ]
  n@(PCut s x y t1 t2 t3 t4) ->
    [ updateNode n (PCut s (x - 1) y t1 t2 t3 t4) graph
    , updateNode n (PCut s (x + 1) y t1 t2 t3 t4) graph
    , updateNode n (PCut s x (y - 1) t1 t2 t3 t4) graph
    , updateNode n (PCut s x (y + 1) t1 t2 t3 t4) graph
    ]
  n@(Color s r g b a t) -> concat
    [ [updateNode n (Color s (r - 1) g b a t) graph | r > 0]
    , [updateNode n (Color s (r + 1) g b a t) graph | r < 255]
    , [updateNode n (Color s r (g - 1) b a t) graph | g > 0]
    , [updateNode n (Color s r (g + 1) b a t) graph | g < 255]
    , [updateNode n (Color s r g (b - 1) a t) graph | b > 0]
    , [updateNode n (Color s r g (b + 1) a t) graph | b < 255]
    , [updateNode n (Color s r g b (a - 1) t) graph | a > 0]
    , [updateNode n (Color s r g b (a + 1) t) graph | a < 255]
    ]
  _ -> []

