module Main where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import qualified Data.Vector as V
import Graphics.Vty.Input.Events
import System.IO.MMap (mmapFileByteStringLazy)
import System.IO (hFileSize, withFile, IOMode(..))
import System.Environment (getArgs, getEnv)
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
    pure (TuiState (V.fromList (BC.lines bs)) 0)

myStr :: V.Vector BC.ByteString -> Widget Name
myStr lines = Widget Greedy Greedy $ do
    ctx <- getContext
    let height = availHeight ctx
    render $ viewport Viewport1 Vertical $ str $ BC.unpack $ BC.unlines $ V.toList $ V.take height lines

drawTui :: TuiState -> [Widget Name]
drawTui (TuiState lines topLine) =
    [myStr $ V.drop topLine lines]

handleTuiEvent :: TuiState -> BrickEvent Name e -> EventM Name (Next TuiState)
handleTuiEvent s@(TuiState lines topLine) e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey (KChar 'j') [] -> continue $ TuiState lines (min (length lines - 1) (topLine + 1))
                EvKey (KChar 'k') [] -> continue $ TuiState lines (max 0 (topLine - 1))
                EvKey (KChar 'e') [] -> continue $ TuiState lines (length lines - 1)
                EvKey (KChar 'g') [] -> do
                    v <- lookupViewport Viewport1
                    case v of
                        Just vp ->
                            let height = snd (_vpSize vp) in
                            continue $ TuiState lines (min (length lines - 1) (topLine + height))
                        Nothing -> continue s
                _ -> continue s
        _ -> continue s

main = tui
