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
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V
import qualified Brick.Focus as F
import Brick (on)

type Event = T.Text

data Names = 
    Messages
    | InputEditor
    deriving (Eq, Ord, Show)

data AppState = AppState {
    messages :: L.List Names T.Text,
    input :: E.Editor T.Text Names,
    focusRing :: F.FocusRing Names
}

initialInput :: E.Editor T.Text Names
initialInput = E.editorText InputEditor (Just 1) ""

initialState :: AppState
initialState = AppState {
    messages = L.list Messages V.empty 1,
    input = initialInput,
    focusRing = F.focusRing [InputEditor, Messages]
}

getInput :: E.Editor T.Text n -> T.Text
getInput = T.strip . T.unlines . E.getEditContents

appDraw :: AppState -> [A.Widget Names]
appDraw AppState { input, messages, focusRing } =  [
        A.padAll 1 (
             messagesWidget <=>
             B.border inputWidget
        )
    ]
    where
        messagesWidget = F.withFocusRing focusRing (L.renderList (\_ t -> A.txt t)) messages
        inputWidget = F.withFocusRing focusRing (E.renderEditor (A.txt . T.unlines)) input

appHandleEvent :: C.BChan T.Text -> AppState -> A.BrickEvent n Event -> A.EventM Names (A.Next AppState)
appHandleEvent inputChan st@AppState{ input, messages } (A.VtyEvent (V.EvKey V.KEnter [])) = 
    case E.getEditContents input of
        [":quit"] -> M.halt st
        [":clear"] -> 
            M.continue $ st {
                input = initialInput,
                messages = L.listClear messages
            }
        _ -> do
            void $ liftIO $ C.writeBChanNonBlocking inputChan $ getInput input
            M.continue $ st {
                input = initialInput
            }

appHandleEvent _ st@AppState { focusRing, messages } (A.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    M.continue $ st { focusRing = F.focusNext focusRing, messages = L.listMoveTo (length messages) messages }

appHandleEvent _ st@AppState { input, messages, focusRing } (A.VtyEvent e) = do
    case F.focusGetCurrent focusRing of
        Just InputEditor -> do
            editor <- E.handleEditorEvent e input
            M.continue st { input = editor }
        Just Messages -> do
            list <- L.handleListEventVi L.handleListEvent e messages
            M.continue st { messages = list }
        Nothing -> M.continue st


appHandleEvent _ st@AppState { messages, focusRing } (A.AppEvent m) =
    case F.focusGetCurrent focusRing of
        Just InputEditor -> do
            M.continue st { messages = L.listMoveTo len $ L.listInsert len m messages }
        _ -> do
            M.continue st { messages = L.listInsert len m messages }
        where len = length messages

appHandleEvent _ st _ = M.continue st

appAttrMap :: b -> A.AttrMap
appAttrMap = const $ A.attrMap V.defAttr [(L.listSelectedFocusedAttr, V.white `on` V.blue)]

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
