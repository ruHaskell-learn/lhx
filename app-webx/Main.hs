{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Control.Applicative
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Web.Scotty hiding (raw)
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Data.Aeson
import Data.Either (rights)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Blaze.Html.Renderer.Text qualified as R
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze (toMarkup, toValue)
import XStatic

import Lhx qualified
import Lhx.Assets qualified
import Lhx.Browser

data WsState = WsState
  { wsInput    :: [Lhx.Input]
  , wsTemplate :: Lhx.Template
  }

data WsMessage
  = NewInput Text
  | NewTemplate Text

type WsMonad m =
  ( MonadReader Connection m
  , MonadState WsState m
  , MonadIO m
  )

instance FromJSON WsMessage where
  parseJSON = withObject "WsMessage" \v ->
    (NewInput <$> v .: "input")
    <|>
    (NewTemplate <$> v .: "template")

main :: IO ()
main = withBrowserOnFreePort (`scotty` app)

app :: ScottyM ()
app = do
  middleware withWS
  middleware $ xstaticMiddleware
    [ svgIconFile
    , htmxJsFile
    ]
  get "/" view

view :: ActionM ()
view = page $ wsWrapper do
  H.div "Input:"
  H.form
    ! H.customAttribute "hx-ws" "send"
    ! H.customAttribute "hx-trigger"
    "keyup from:[name='input'] changed delay:1s"
    $ do
      H.textarea
        ! A.name "input"
        ! A.autocomplete "off"
        $ ""
  H.div "Template:"
  H.form
    ! H.customAttribute "hx-ws" "send"
    ! H.customAttribute "hx-trigger"
    "keyup from:[name='template'] changed delay:1s"
    $ do
      H.input
        ! A.name "template"
        ! A.autocomplete "off"
      H.span
        ! A.id "template-errors"
        $ ""
  H.div "Output:"
  output ""
  where
    wsWrapper = H.div
      ! H.customAttribute "hx-ws" "connect:/"

output :: Text -> Html
output content =
  H.textarea
    ! A.id "output"
    ! A.readonly (toValue True)
    $ toMarkup content

page :: Html -> ActionM ()
page inner = html $ R.renderHtml do
  H.docTypeHtml do
    H.head do
      H.title "LHX"
      H.link
        ! A.rel "shortcut icon"
        ! A.href (toValue Lhx.Assets.ico)
      H.link
        ! A.rel "icon"
        ! A.sizes "any"
        ! A.href "/xstatic/lhx.svg"
      H.script
        ! A.src "/xstatic/htmx.min.js"
        $ ""
    H.body do
      inner

withWS :: Middleware
withWS = websocketsOr defaultConnectionOptions wsApp

wsApp :: ServerApp
wsApp pendingConn = do
  conn <- acceptRequest pendingConn
  info "Connected"
  void $ handler `State.execStateT` initialState `Reader.runReaderT` conn
  where
    handler = forever do
      getMessage >>= \case
        Left msg -> info $ "Unhandled: " <> show msg
        Right (NewInput inp) -> handleNewInput inp
        Right (NewTemplate rawTpl) -> handleNewTemplate rawTpl
    info :: MonadIO m => String -> m ()
    info = liftIO . putStrLn . ("WS: " <>)

getMessage :: (MonadReader Connection m, MonadIO m) => m (Either DataMessage WsMessage)
getMessage = do
  conn <- Reader.ask
  msg <- liftIO $ receiveDataMessage conn
  case msg of
    Text raw Nothing ->
      case decode raw of
        Nothing -> pure $ Left msg
        Just x  -> pure $ Right x
    _ -> pure $ Left msg

handleNewInput :: WsMonad m => Text -> m ()
handleNewInput inp = do
  State.modify \s ->
    s { wsInput =
           map (Lhx.makeInput (Lhx.Separator ","))
           (T.lines inp)
      }
  updateOutput

handleNewTemplate :: WsMonad m => Text -> m ()
handleNewTemplate rawTpl = do
  case Lhx.makeTemplate rawTpl of
    Left es ->
      sendHtml
        $ H.span
        ! A.id "template-errors"
        ! A.title (toValue $ Lhx.errorText es)
        ! A.style "color: red;"
        $ "⚠"
    Right tpl -> do
      sendHtml
        $ H.span
        ! A.id "template-errors"
        $ ""
      State.modify \s -> s { wsTemplate = tpl }
      updateOutput

updateOutput :: WsMonad m => m ()
updateOutput = do
  WsState inp tpl <- State.get
  let ls = rights $ map (Lhx.apply tpl) inp
  sendHtml $ output $ T.unlines ls

sendHtml :: (MonadReader Connection m, MonadIO m) => Html -> m ()
sendHtml el = do
  conn <- Reader.ask
  liftIO $ sendTextData conn . R.renderHtml $ el

initialState :: WsState
initialState = WsState
  { wsInput = []
  , wsTemplate = []
  }

svgIconFile :: XStaticFile
svgIconFile = XStaticFile
  { xfPath = "/lhx.svg"
  , xfContent = Lhx.Assets.svg
  , xfETag = ""
  , xfType = "image/svg+xml"
  }

htmxJsFile :: XStaticFile
htmxJsFile = XStaticFile
  { xfPath = "/htmx.min.js"
  , xfContent = $(embedFile "data/htmx.min.js.gz")
  , xfETag = ""
  , xfType = "application/javascript"
  }
