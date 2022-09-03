import Control.Lens
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.Text.IO qualified as T
import System.Environment
import System.IO

import ICFPC.Graph
import ICFPC.ISL qualified as I
import ICFPC.Render
import ICFPC.Tracer

main :: IO ()
main = getArgs >>= \case
  [input, output] -> do
    image <- readImageRGBA8 input
    let
      tryCost graph = case tryScoreGraph image graph of
        Left _ -> Nothing
        Right cost -> Just (graph, cost)

      -- Optimize only color nodes
      goColor cost graph = do
        let graphs = mapMaybe tryCost $ shrinkGraphWith shrinkColorNode graph

        case graphs of
          [] -> (graph, cost)
          (minimumBy (comparing snd) -> (g, cost'))
            | cost' < cost -> goColor cost' g
            | otherwise -> (graph, cost)

      go cost0 step graph0 = do
        let
          (graph, cost) = (graph0, cost0) -- goColor cost0 graph0
          graphs = mapMaybe tryCost $ shrinkGraphWith (shrinkNode step) graph

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

    initGraph <- fromISL . I.parseProgram <$> T.getContents
    let Just (_, initCost) = tryCost initGraph
    go initCost 40 initGraph
  _ -> error "Usage: local <input.png> <inout.isl>"

shrinkGraphWith :: (forall n m. Node n m -> [Node n m]) -> Graph -> [Graph]
shrinkGraphWith shr graph = topoSort graph >>= \i -> modifyNode i shr graph

shrinkColorNode :: Node n m -> [Node n m]
shrinkColorNode = \case
  Color (unpackRGBA -> (r, g, b, a)) -> let step' = 1 in concat
    [ [Color $ packRGBA (r - step', g, b, a) | r >= step']
    , [Color $ packRGBA (r + step', g, b, a) | r <= 255 - step']
    , [Color $ packRGBA (r, g - step', b, a) | g >= step']
    , [Color $ packRGBA (r, g + step', b, a) | g <= 255 - step']
    , [Color $ packRGBA (r, g, b - step', a) | b >= step']
    , [Color $ packRGBA (r, g, b + step', a) | b <= 255 - step']
    , [Color $ packRGBA (r, g, b, a - step') | a >= step']
    , [Color $ packRGBA (r, g, b, a + step') | a <= 255 - step']
    ]
  _ -> []

shrinkNode :: Int -> Node n m -> [Node n m]
shrinkNode step = \case
  XCut x ->
    [ XCut (x - step)
    , XCut (x + step)
    ]
  YCut y ->
    [ YCut (y - step)
    , YCut (y + step)
    ]
  PCut x y ->
    [ PCut (x - step) y
    , PCut (x + step) y
    , PCut x (y - step)
    , PCut x (y + step)
    ]
  Color (unpackRGBA -> (r, g, b, a)) -> let step' = fromIntegral step in concat
    [ [Color $ packRGBA (r - step', g, b, a) | r >= step']
    , [Color $ packRGBA (r + step', g, b, a) | r <= 255 - step']
    , [Color $ packRGBA (r, g - step', b, a) | g >= step']
    , [Color $ packRGBA (r, g + step', b, a) | g <= 255 - step']
    , [Color $ packRGBA (r, g, b - step', a) | b >= step']
    , [Color $ packRGBA (r, g, b + step', a) | b <= 255 - step']
    , [Color $ packRGBA (r, g, b, a - step') | a >= step']
    , [Color $ packRGBA (r, g, b, a + step') | a <= 255 - step']
    ]
  _ -> []
