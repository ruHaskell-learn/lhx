{-# OPTIONS -Wno-unused-do-bind #-}

module Main where

import Data.IORef
import Data.Either (rights)
import Data.Text qualified as T
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Events qualified as E
import Graphics.UI.Threepenny.Core

import Lhx (Separator(..))
import Lhx qualified
import Lhx.Assets qualified
import Lhx.Browser
import Lhx.Runner
import Lhx.Streaming qualified as S

data State = State
  { template   :: Either [Lhx.Error] Lhx.Template
  , input      :: [Lhx.Input]
  , inputEl    :: Element
  , templateEl :: Element
  , outputEl   :: Element
  , btnOkEl    :: Element
  }

main :: IO ()
main =
  S.interact (S.defaultApp @IO)
    { S.aPrepare = pure . Lhx.makeInput (Lhx.Separator ",")
    , S.aMakeTransformer = \preview -> do
         mbTpl <- interactWith \stop ->
           withBrowserOnFreePort \port ->
           startGUI defaultConfig
             { jsPort = Just port
             , jsLog = const $ pure ()
             } $ gui preview stop
         pure $ safeApply <$> mbTpl
    }
  where
    safeApply tpl t = case Lhx.apply tpl t of
      Left _ -> Nothing
      Right x -> Just x

gui :: [Lhx.Input] -> (Maybe Lhx.Template -> IO ()) -> Window -> UI ()
gui preview stop win = do
  -- view
  pure win # set UI.title "LHX"
  getHead win #+
    [ UI.link
      # set UI.rel "shortcut icon"
      # set UI.href (T.unpack Lhx.Assets.ico)
    ]
  inp <- UI.textarea
    -- # set UI.enabled False
    # set UI.style [("height", "150px")]
  tpl <- UI.input
  out <- UI.textarea
    # set UI.enabled False
    # set UI.style [("height", "150px")]
  btnOk <- UI.button
    # set UI.text "Ok"
  btnCancel <- UI.button
    # set UI.text "Cancel"
  getBody win
    # set UI.style
      [ ("display", "flex")
      , ("flex-direction", "column")
      ]
    #+
    [ pure inp
    , pure tpl
    , pure out
    , UI.div #+ [ pure btnOk, pure btnCancel]
    ]
  UI.setFocus tpl
  -- state
  ref <- liftIO $ newIORef State
    { template   = Right []
    , input      = []
    , inputEl    = inp
    , templateEl = tpl
    , outputEl   = out
    , btnOkEl    = btnOk
    }
  -- initial input
  let i = unlines $ map (T.unpack . Lhx.iRaw) preview
  set UI.value i $ pure inp
  changeInput ref i
  -- events
  on E.valueChange inp $ changeInput ref
  on E.valueChange tpl $ changeTemplate ref
  on E.click btnOk $ submit ref
  on E.click btnCancel \_ -> liftIO (stop Nothing)
  pure ()
  where
    submit ref _ = do
      State{template = tpl} <- liftIO $ readIORef ref
      case tpl of
        Left _  -> pure ()
        Right t -> liftIO . stop $ Just t

applyState :: IORef State -> UI ()
applyState ref = do
  State{template = et, input = is, outputEl = out} <-
    liftIO $ readIORef ref
  case et of
    Left _  -> pure ()
    Right t -> do
      let ls = rights $ map (Lhx.apply t) is
      pure out # set UI.value (T.unpack $ T.unlines ls)
      pure ()

changeInput :: IORef State -> String -> UI ()
changeInput ref val = do
  pureModifyState ref \s -> s
    { input = map (Lhx.makeInput (Separator ","))
      $ T.lines $ T.pack val }
  applyState ref

changeTemplate :: IORef State -> String -> UI ()
changeTemplate ref val = do
  State{templateEl = tpl, btnOkEl = btnOk, template = t} <-
    pureModifyState ref \s -> s
      { template = Lhx.makeTemplate $ T.pack val }
  case t of
    Left es  ->
      let ttl = unlines [T.unpack e | Lhx.Error e <- es]
      in do
         pure tpl
           # set UI.style [("color", "red")]
           # set UI.title__ ttl
         pure btnOk
           # set UI.enabled False
    Right _ -> do
      pure tpl
        # set UI.style [("color", "initial")]
        # set UI.title__ ""
      pure btnOk
        # set UI.enabled True
  applyState ref

pureModifyState :: IORef State -> (State -> State) -> UI State
pureModifyState ref f = liftIO $ atomicModifyIORef' ref \s ->
  let s' = f s
  in (s', s')
