{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.IORef
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

data State = State
  { sInput         :: [Lhx.Input]
  , sTemplate      :: Lhx.Template
  }

data WsMessage
  = NewInput Text
  | NewTemplate Text

instance FromJSON WsMessage where
  parseJSON = withObject "WsMessage" \v ->
    (NewInput <$> v .: "input")
    <|>
    (NewTemplate <$> v .: "template")

main :: IO ()
main = scotty 8000 do
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
  liftIO $ putStrLn "WS connected"
  ref <- newIORef initialState
  forever do
    msg <- receiveDataMessage conn
    let mbMsg = do
          Text raw Nothing <- pure msg
          decode raw
    case mbMsg of
      Nothing -> liftIO $ putStrLn $ "Unhandled: " <> show msg
      Just (NewInput inp) -> do
        liftIO $ atomicModifyIORef' ref \s ->
              (s { sInput =
                      map (Lhx.makeInput (Lhx.Separator ","))
                      (T.lines inp)
                 }, ())
        updateOutput ref conn
      Just (NewTemplate rawTpl) ->
        case Lhx.makeTemplate rawTpl of
          Left es ->
            sendTextData conn . R.renderHtml $
              H.span
                ! A.id "template-errors"
                ! A.title (toValue $ T.unlines $ map Lhx.getError es)
                ! A.style "color: red;"
                $ "⚠"
          Right tpl -> do
            sendTextData conn . R.renderHtml $
              H.span ! A.id "template-errors" $ ""
            liftIO $ atomicModifyIORef' ref \s ->
              (s { sTemplate = tpl }, ())
            updateOutput ref conn
  where
    updateOutput ref conn = do
      State inp tpl <- readIORef ref
      let ls = rights $ map (Lhx.apply tpl) inp
      sendTextData conn . R.renderHtml $ output $ T.unlines ls

initialState :: State
initialState = State
  { sInput = []
  , sTemplate = []
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
