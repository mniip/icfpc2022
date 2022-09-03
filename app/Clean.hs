import Data.Maybe
import Data.Function
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Text.IO qualified as T
import System.Environment

import ICFPC.ISL
import ICFPC.Render

commands :: Program -> [Command]
commands (Program pls) = concatMap (\p -> case p of Command c -> [c]; _ -> []) $ NE.toList pls

toProg :: [Command] -> Program
toProg cmd = Program (NE.fromList (map Command cmd))

-- Try removing color commands one by one
removeColors :: [Command] -> [[Command]]
removeColors [] = [[]]
removeColors (x@(Color _ _):xs) = xs : (map (x:) $ removeColors xs)
removeColors (x:xs) = (map (x:) $ removeColors xs)

-- Remove commands which don't affect the image at the end
removeTail :: [Command] -> [Command]
removeTail xs = reverse . dropWhile (\c -> case c of Merge _ _ -> True; PCut _ _ _ -> True; LCut _ _ _ -> True; _ -> False) $ reverse xs

main :: IO ()
main = getArgs >>= \case
  [input] -> do
    image <- readImageRGBA8 input
    prog <- parseProgram <$> T.getContents
    let
      score pr = case tryScoreProgram image pr of
        Left _ -> Nothing
        Right sc -> Just (sc, pr)
      tries prg = mapMaybe score (map (toProg . removeTail) . removeColors $ commands prg)
      improve prg = let Just (sc, _) = score prg
                        prgs = filter (\(s, _) -> s < sc) $ tries prg
                    in if null prgs then prg else snd $ head prgs
      go prg = let prg' = improve prg in if prg' == prg then prg else go prg'
    T.putStrLn . printProgram $ go prog
  _ -> error "Usage: clean <input.png>"
