import Codec.Picture
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Text.IO qualified as T
import System.Environment

import ICFPC.Cost
import ICFPC.ISL
import ICFPC.Pairs
import ICFPC.Render

commands :: Program -> [Command]
commands (Program pls) = concatMap (\p -> case p of Command c -> [c]; _ -> []) $ NE.toList pls

toProg :: [Command] -> Program
toProg cmd = Program (NE.fromList (map Command cmd))

-- Try removing color commands one by one
removeColors :: [Command] -> [[Command]]
removeColors [] = [[]]
removeColors ls@(x@(Color _ _ _ _ _):xs) = xs : (map (x:) $ removeColors xs)
removeColors (x:xs) = (map (x:) $ removeColors xs)

-- Remove commands which don't affect the image at the end
removeTail :: [Command] -> [Command]
removeTail xs = reverse . dropWhile (\c -> case c of Merge _ _ -> True; PCut _ _ _ -> True; LCut _ _ _ -> True; _ -> False) $ reverse xs

main :: IO ()
main = do
  getArgs >>= \case
    [input] -> readImage input >>= \case
      Left err -> error err
      Right (ImageRGBA8 image) -> parseProgram <$> T.getContents >>= \case
        Left err -> error err
        Right prog -> do
          let
            width = imageWidth image
            height = imageHeight image
            score prg = 
              case runRender (XY width height) $ runCostT (runTrace prg (XY width height)) of
                (((Nothing, _), Sum cost), image') -> Just (cost + compareImages image image', prg)
                (((Just err, BState line _ _), _), _) -> Nothing
            tries prg = mapMaybe score (map (toProg . removeTail) . removeColors $ commands prg)
            improve prg = snd . minimumBy (compare `on` fst) $ tries prg
            go prg = let prg' = improve prg in if prg' == prg then prg else go prg'
          T.putStrLn . printProgram $ go prog
      Right _ -> error "Invalid pixel format"
    _ -> error "Usage: clean <input.png>"
