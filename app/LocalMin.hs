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
          tryCost g = case runRender (XY width height) $ runCostT (runTrace g (XY width height)) of
            (((Nothing, _), Sum cost), image'') -> Just $ (g, cost + compareImages image' image'')
            (((Just e, _), _), _) -> Nothing
 
          -- Optimize only color nodes
          goColor cost graph = do
            let
              graphs = mapMaybe tryCost $ concat $ shrinkColorNode graph <$> topoSort graph

            case graphs of
              [] -> (graph, cost)
              (minimumBy (comparing snd) -> (g, cost'))
                | cost' < cost -> goColor cost' g
                | otherwise -> (graph, cost)

          go cost0 step graph0 = do
            let
              (graph, cost) = goColor cost0 graph0
              graphs = mapMaybe tryCost $ concat $ shrinkNode step graph <$> topoSort graph

            case graphs of
              [] -> if step > 1 then go cost (step-1) graph else hPutStrLn stderr "No shrinks"
              (minimumBy (comparing snd) -> (g, cost'))
                | cost' < cost -> do
                  hPutStrLn stderr $ "Improvement: " <> show cost' ++ ", step: " ++ show step
                  T.writeFile output $ I.printProgram $ toISL g
                  go cost' (step + 1) g -- Accelerate
                | step > 1 -> do
                    let step' = (step + 1) `div` 2
                    hPutStrLn stderr $ "Decelerate to " ++ show step'
                    go cost step' graph -- Decelrate
                | otherwise -> do
                  hPutStrLn stderr "No improvement"

        I.parseProgram <$> T.readFile output >>= \case
          Left err -> error err
          Right prog -> let graph = fromISL prog
                            Just (_, cost) = tryCost graph
                        in go cost 40 graph
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: local <input.png> <inout.isl>"

shrinkColorNode :: Graph -> Node -> [Graph]
shrinkColorNode graph = \case
  n@(Color s r g b a t) -> let step' = 1 in concat
    [ [updateNode n (Color s (r - step') g b a t) graph | r >= step']
    , [updateNode n (Color s (r + step') g b a t) graph | r <= 255 - step']
    , [updateNode n (Color s r (g - step') b a t) graph | g >= step']
    , [updateNode n (Color s r (g + step') b a t) graph | g <= 255 - step']
    , [updateNode n (Color s r g (b - step') a t) graph | b >= step']
    , [updateNode n (Color s r g (b + step') a t) graph | b <= 255 - step']
    , [updateNode n (Color s r g b (a - step') t) graph | a >= step']
    , [updateNode n (Color s r g b (a + step') t) graph | a <= 255 - step']
    ]
  _ -> []

shrinkNode :: Int -> Graph -> Node -> [Graph]
shrinkNode step graph = \case
  n@(XCut s x t1 t2) ->
    [ updateNode n (XCut s (x - step) t1 t2) graph
    , updateNode n (XCut s (x + step) t1 t2) graph
    ]
  n@(YCut s y t1 t2) ->
    [ updateNode n (YCut s (y - step) t1 t2) graph
    , updateNode n (YCut s (y + step) t1 t2) graph
    ]
  n@(PCut s x y t1 t2 t3 t4) ->
    [ updateNode n (PCut s (x - step) y t1 t2 t3 t4) graph
    , updateNode n (PCut s (x + step) y t1 t2 t3 t4) graph
    , updateNode n (PCut s x (y - step) t1 t2 t3 t4) graph
    , updateNode n (PCut s x (y + step) t1 t2 t3 t4) graph
    ] 
  _ -> []
