module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
import System.IO.MMap (mmapFileByteStringLazy)
import System.IO (hFileSize, withFile, IOMode(..))
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as BC


tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState


data TuiState = TuiState { lines :: V.Vector BC.ByteString
                         , topLine :: Int
                         } deriving (Show, Eq)
data Name = Viewport1 deriving (Show, Eq, Ord)

tuiApp :: App TuiState e Name
tuiApp = App { appDraw = drawTui
             , appChooseCursor = showFirstCursor   
             , appHandleEvent = handleTuiEvent
             , appStartEvent = pure
             , appAttrMap = const $ attrMap mempty []
             }

buildInitialState :: IO TuiState
buildInitialState = do
    (file : args) <- getArgs
    bs <- mmapFileByteStringLazy file Nothing
    pure $ (TuiState (V.fromList (BC.lines bs)) 0)

drawTui :: TuiState -> [Widget Name]
drawTui (TuiState lines topLine) =
    [viewport Viewport1 Vertical $ str $ BC.unpack $ BC.unlines (V.toList lines)]

handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent s@(TuiState lines topLine) e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 'j') [] -> do
                    let vp = viewportScroll Viewport1
                    vScrollBy vp 1
                    continue s
                EvKey (KChar 'k') [] -> do
                    let vp = viewportScroll Viewport1
                    vScrollBy vp (-1)
                    continue s
                _ -> continue s
        _ -> continue s

main = tui
