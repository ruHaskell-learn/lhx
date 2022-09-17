{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import qualified Graphics.Vty as Vty
import Control.Monad.State hiding (State)
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty.Attributes qualified as VA
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Lens.Micro
import Lens.Micro.Extras (view)
import Lens.Micro.TH

import Lhx qualified

data Name
  = Input
  | Template
  | Output
  deriving (Show, Eq, Ord, Enum, Bounded)

data State = State
  { _sInput          :: [Text]
  , _sTemplateEditor :: Editor Text Name
  , _sTemplate       :: Either [Lhx.Error] Lhx.Template
  , _sOutput         :: [Text]
  , _sFocused        :: Name
  }

$(makeLenses 'State)

main :: IO ()
main = void $ defaultMain @Name App
  { appDraw = draw
  , appChooseCursor = \s -> showCursorNamed $ s ^. sFocused
  , appHandleEvent = handle
  , appStartEvent = initMouse
  , appAttrMap = \_ ->
    attrMap VA.defAttr
    [ (editAttr, VA.defAttr)
    , (parsingError, VA.withForeColor VA.defAttr VA.red)
    ]
  } State
  { _sInput =
      [ "Bob Smith, 1980"
      , "Ann Thompson, 1970"
      , "John Doe"
      , "Jane Air, 1920, 2000"
      , "foobar"
      , "foobar"
      , "foobar"
      , "foobar"
      , "foobar"
      , "foobar"
      ]
  , _sTemplateEditor = editorText Template Nothing ""
  , _sTemplate = Right []
  , _sOutput = []
  , _sFocused = Template
  }

draw :: State -> [Widget Name]
draw s = [layout]
  where
    f = s ^. sFocused
    layout = vBox [inp, tpl, out]
    inp = clickable Input
      . vLimitPercent 30
      . textArea (f == Input) Input 
      $ s ^. sInput
    tpl = clickable Template
      . vLimit (min 5 $ length . getEditContents $ s ^. sTemplateEditor)
      . withVScrollBars OnRight
      . renderEditor re True
      $ s ^. sTemplateEditor
    out = clickable Output . textArea (f == Output) Output $ s ^. sOutput
    re = withAttr edAttr . txt . T.unlines
    edAttr
      | s ^. sTemplate . to isLeft = parsingError
      | otherwise = editAttr

initMouse :: EventM n s ()
initMouse = do
  vty <- Brick.getVtyHandle
  let output = Vty.outputIface vty
  when (Vty.supportsMode output Vty.Mouse) $
    liftIO $ Vty.setMode output Vty.Mouse True

handle :: BrickEvent Name () -> EventM Name State ()
handle evt =
  case evt of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt
    VtyEvent (EvKey KBackTab []) ->
      modify $ over sFocused \case
        n | n == minBound -> maxBound
          | otherwise -> pred n
    VtyEvent (EvKey (KChar '\t') []) ->
      modify $ over sFocused \case
        n | n == maxBound -> minBound
          | otherwise -> succ n
    MouseDown clickedArea Vty.BLeft _ _ -> do
      modify (sFocused .~ clickedArea)
    _ ->
      gets (view sFocused) >>= \case
        Input ->
          handleTextAreaEvents Input evt
        Output ->
          handleTextAreaEvents Output evt
        Template -> do
          zoom sTemplateEditor $ handleEditorEvent evt
          ed <- gets $ view sTemplateEditor
          modify \s ->
            s & sTemplate .~ case getEditContents ed of
              (t:_) -> Lhx.makeTemplate t
              _     -> Right []
          updateOutput

updateOutput :: MonadState State m => m ()
updateOutput = do
  tpl <- gets $ view sTemplate
  case tpl of
    Left _  -> pure ()
    Right t -> do
      is <- gets $ view sInput
      let os = is ^.. traversed
               . to (Lhx.makeInput (Lhx.Separator ","))
               . to (Lhx.apply t)
               . _Right
      modify $ \s -> s & sOutput .~ os

textArea :: (Show n, Ord n) => Bool -> n -> [Text] -> Widget n
textArea focused name =
  withBorderStyle bs
  . border
  . withVScrollBars OnRight
  . viewport name Both
  . txt
  . T.unlines
  where
    bs | focused = unicodeBold
       | otherwise = defaultBorderStyle

parsingError :: AttrName
parsingError = attrName "parsingError"

handleTextAreaEvents :: n -> BrickEvent n e -> EventM n s ()
handleTextAreaEvents name = \case
  VtyEvent (EvKey KHome []) -> vScrollToBeginning vps
  VtyEvent (EvKey KEnd [])  -> vScrollToEnd vps
  MouseDown _ Vty.BScrollUp _ _ -> vScrollBy vps (-1)
  VtyEvent (EvKey KUp [])   -> vScrollBy vps (-1)
  MouseDown _ Vty.BScrollDown _ _ -> vScrollBy vps 1
  VtyEvent (EvKey KDown []) -> vScrollBy vps 1
  _ -> pure ()
  where
    vps = viewportScroll name
