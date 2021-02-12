{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Client.Ui where

import qualified Brick as A
import Brick.Widgets.Core ((<=>))
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Data.Text as T
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Brick.BChan as C
import qualified Graphics.Vty
import Client.Core (UiClientCtx(getInputChan, getEventChan), ClientEnv)
import Control.Monad.Reader (asks)
import Control.Monad (void)

type Event = T.Text

data Names = 
    MessagesViewport
    | InputEditor
    deriving (Eq, Ord, Show)

data AppState = AppState {
    messages :: [T.Text],
    input :: E.Editor T.Text Names
}

initialInput :: E.Editor T.Text Names
initialInput = E.editorText InputEditor (Just 1) ""

initialState :: AppState
initialState = AppState {
    messages = [],
    input = initialInput
}

getInput :: E.Editor T.Text n -> T.Text
getInput = T.strip . T.unlines . E.getEditContents

appDraw :: AppState -> [A.Widget Names]
appDraw AppState { input, messages } =  [
        A.padAll 1 (
             messagesWidget <=>
             B.border inputWidget
        )
    ]
    where
        messagesWidget = A.viewport MessagesViewport A.Vertical $ A.vBox (map A.txtWrap messages)
        inputWidget = E.renderEditor (A.txt . T.unlines) True input

appHandleEvent :: C.BChan T.Text -> AppState -> A.BrickEvent n Event -> A.EventM Names (A.Next AppState)
appHandleEvent inputChan st@AppState{ input } (A.VtyEvent (V.EvKey V.KEnter [])) = 
    case E.getEditContents input of
        [":quit"] -> M.halt st
        [":clear"] -> 
            M.continue $ st {
                input = initialInput,
                messages = []
            }
        _ -> do
            void $ liftIO $ C.writeBChanNonBlocking inputChan $ getInput input
            M.continue $ st {
                input = initialInput
            }

appHandleEvent _ st@AppState { input } (A.VtyEvent e) = do
    editor <- E.handleEditorEvent e input
    M.continue st { input = editor }

appHandleEvent _ st@AppState { messages } (A.AppEvent m) =
    M.continue st { messages = messages <> [m] }

appHandleEvent _ st _ = M.continue st

appAttrMap :: b -> A.AttrMap
appAttrMap = const $ A.attrMap V.defAttr []

appChooseCursor :: s -> [A.CursorLocation n] -> Maybe (A.CursorLocation n)
appChooseCursor = M.showFirstCursor

app :: C.BChan T.Text -> M.App AppState Event Names
app inputChan = M.App {
    A.appDraw,
    A.appChooseCursor,
    A.appHandleEvent = appHandleEvent inputChan,
    A.appAttrMap,
    A.appStartEvent = return
}

run :: ClientEnv ()
run  = do
    eventChan <- asks getEventChan
    inputChan <- asks getInputChan
    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    initialVty <- liftIO buildVty
    void $ liftIO $ M.customMain initialVty buildVty (Just eventChan) (app inputChan) initialState
