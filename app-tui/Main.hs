module Main where

import Control.Monad (void)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
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
  , appStartEvent = pure ()
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
    inp =
      vLimitPercent 30
      $ textArea (f == Input) Input $ s ^. sInput
    tpl =
      vLimit 3
      $ renderEditor re True
      $ s ^. sTemplateEditor
    out = textArea (f == Output) Output $ s ^. sOutput
    re = withAttr edAttr . txt . T.unlines
    edAttr
      | s ^. sTemplate . to isLeft = parsingError
      | otherwise = editAttr

handle :: BrickEvent Name e -> EventM Name State ()
handle evt =
  case evt of
    VtyEvent (EvKey KEsc []) -> halt
    VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt
    VtyEvent (EvKey KDown [MCtrl]) ->
      modify \s -> s & sFocused %~ \case
        n | n == maxBound -> minBound
          | otherwise -> succ n
    _ -> do
      zoom sTemplateEditor $ handleEditorEvent evt
      ed <- gets $ view sTemplateEditor
      modify \s ->
        s & sTemplate .~ case getEditContents ed of
          (t:_) -> Lhx.makeTemplate t
          _     -> Right []
      tpl <- gets $ view sTemplate
      case tpl of
        Left _ -> pure ()
        Right t -> do
          is <- gets $ view sInput
          let os = is ^.. traversed
                   . to (Lhx.makeInput (Lhx.Separator ","))
                   . to (Lhx.apply t)
                   . _Right
          modify $ \s -> s & sOutput .~ os

textArea :: Bool -> Name -> [Text] -> Widget Name
textArea focused n =
  withBorderStyle bs
  . border
  . withVScrollBars OnRight
  . withHScrollBars OnBottom
  . viewport n Both
  . txt
  . T.unlines
  where
    bs | focused = unicodeBold
       | otherwise = defaultBorderStyle

parsingError :: AttrName
parsingError = attrName "parsingError"
