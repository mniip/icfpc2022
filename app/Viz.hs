import Codec.Picture
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import Data.Foldable
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text.IO qualified as T
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Interact
import System.Environment
import Text.Read hiding (lift)

import ICFPC.Gloss
import ICFPC.Graph hiding (Color)
import ICFPC.ISL qualified as I
import ICFPC.Pairs
import ICFPC.Render
import ICFPC.Tracer

data ScreenData = ScreenData
  { _screenSize :: !(Float, Float)
  , _uiViewPort :: !ViewPort
  , _sceneViewPort :: !ViewPort
  }

makeLenses ''ScreenData

mkScreenData :: (Float, Float) -> ScreenData
mkScreenData (width, height) = ScreenData
  { _screenSize = (width, height)
  , _uiViewPort = ViewPort (-width / 2, -height / 2) 0 1
  , _sceneViewPort = ViewPort (0, 0) 0 (min width height / 2)
  }

data MouseData = MouseData
  { _uiCoords :: !(Float, Float)
  , _rounded :: !(XY Int)
  , _floored :: !(XY Int)
  }

makeLenses ''MouseData

mkMouseData :: XY Int -> ScreenData -> (Float, Float) -> MouseData
mkMouseData sz sdata ms = MouseData
  { _uiCoords = ms ^. to (invertViewPort $ sdata ^. uiViewPort)
  , _floored = ms ^. to (invertViewPort $ sdata ^. sceneViewPort) . to (uncurry XY) . to (fmap $ floor . fromGlossCoords sz)
  , _rounded = ms ^. to (invertViewPort $ sdata ^. sceneViewPort) . to (uncurry XY) . to (fmap $ round . fromGlossCoords sz)
  }

data GraphData = GraphData
  { _graph :: !Graph
  , _allBlockMap :: !(M.Map EdgeId Block)
  , _blockMap :: !(M.Map EdgeId Block)
  , _resultImage :: !(Image PixelRGBA8)
  , _resultBitmap :: !Picture
  }

makeLenses ''GraphData

mkGraphData :: XY Int -> Graph -> GraphData
mkGraphData sz gr = GraphData
  { _graph = gr
  , _allBlockMap = abm
  , _blockMap = M.restrictKeys abm (gDanglingDown gr)
  , _resultImage = image
  , _resultBitmap = imageToGloss image
  }
  where ((_, abm), image) = runRender sz $ runTrace gr sz

data World = World
  { _outputImage :: !(Maybe (Image PixelRGBA8))
  , _outputBitmap :: !(Maybe Picture)
  , _inputFilename :: !(Maybe FilePath)
  , _size :: !(XY Int)
  , _graphData :: !GraphData
  , _screenData :: !ScreenData
  , _mouseData :: !MouseData
  , _hovering :: !(Maybe EdgeId)
  }

makeLenses ''World

drawUI :: World -> WriterT Picture IO ()
drawUI world = pure ()

drawScene :: World -> WriterT Picture IO ()
drawScene world = do
  --forM_ (world ^. outputBitmap) tell
  tell $ world ^. graphData . resultBitmap
  forM_ (world ^. graphData . blockMap . to M.toList) $ \(i, blk) -> do
    censor (Color (if Just i == world ^. hovering then red else green)) $ do
      tell $ wireframe $ fmap (fmap $ toGlossCoords (world ^. size)) blk

draw :: World -> WriterT Picture IO ()
draw world = do
  censor (applyViewPortToPicture (world ^. screenData . uiViewPort)) $ drawUI world
  censor (applyViewPortToPicture (world ^. screenData . sceneViewPort)) $ drawScene world

updateHover :: StateT World IO ()
updateHover = do
  bm <- use $ graphData . blockMap
  fl <- use $ mouseData . floored
  hovering .= fmap fst (find (inBlock fl . snd) $ M.toList bm)
  where inBlock xy b = and $ liftA2 between xy b

handleEvent :: Event -> StateT World IO ()
handleEvent = \case
  EventMotion ms -> do
    sz <- use size
    sdata <- use screenData
    mouseData .= mkMouseData sz sdata ms
    updateHover
  EventResize (w, h) -> do
    screenData .= mkScreenData (fromIntegral w, fromIntegral h)
  _ -> pure ()

main :: IO ()
main = getArgs >>= \case
  [sWidth, sHeight, input]
    | Just width <- readMaybe sWidth
    , Just height <- readMaybe sHeight
    -> do
      let sz = XY width height
      txt <- T.readFile input
      start Nothing (Just input) sz (parseInput txt)
  [sWidth, sHeight]
    | Just width <- readMaybe sWidth
    , Just height <- readMaybe sHeight
    -> do
      let sz = XY width height
      start Nothing Nothing sz emptyGraph
  [output, input]
    -> readImage output >>= \case
      Left err -> error err
      Right (ImageRGBA8 image) -> do
        let sz = XY (imageWidth image) (imageHeight image)
        txt <- T.readFile input
        start (Just image) (Just input) sz (parseInput txt)
      Right _ -> error "Invalid pixel format"
  [output]
    -> readImage output >>= \case
      Left err -> error err
      Right (ImageRGBA8 image) -> do
        let sz = XY (imageWidth image) (imageHeight image)
        start (Just image) Nothing sz emptyGraph
      Right _ -> error "Invalid pixel format"

  _ -> error "Usage: viz ( <width> <height> [input.isl | input.graph] | [output.png] [input.isl | input.graph])"
  where
    parseInput txt = case I.parseProgram txt of
      Right prog -> fromISL prog
      Left err -> case parseGraph txt of
        Right gr -> gr
        Left err' -> error $ err <> "\n" <> err'
    start mImage mInput sz gr = do
      screen <- getScreenSize
      let
        width = fromIntegral $ fst screen
        height = fromIntegral $ snd screen
        sdata = mkScreenData (width, height)
        initWorld = World
          { _outputImage = mImage
          , _outputBitmap = imageToGloss <$> mImage
          , _inputFilename = mInput
          , _size = sz
          , _graphData = mkGraphData sz gr
          , _screenData = sdata
          , _mouseData = mkMouseData sz sdata (0, 0)
          , _hovering = Nothing
          }
      interactIO FullScreen black initWorld (execWriterT . draw) (execStateT . handleEvent) (const $ pure ())
