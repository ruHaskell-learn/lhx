{-# OPTIONS -Wno-unused-do-bind #-}

module Main where

import Data.IORef
import Data.Either (rights)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.UI.Threepenny qualified as UI
import Graphics.UI.Threepenny.Events qualified as E
import Graphics.UI.Threepenny.Core

import Lhx (Separator(..))
import Lhx qualified

fakeInput :: [Text]
fakeInput =
  [ "Bob Smith, 42"
  , "Ann Thomas, 90"
  ]

data State = State
  { template   :: Either [Lhx.Error] Lhx.Template
  , input      :: [Lhx.Input]
  , inputEl    :: Element
  , templateEl :: Element
  , outputEl   :: Element
  }

main :: IO ()
main = startGUI defaultConfig { jsPort = Just 8000 } gui

gui :: Window -> UI ()
gui win = do
  -- view
  pure win # set UI.title "LHX"
  inp <- UI.textarea
    -- # set UI.enabled False
    # set UI.style [("height", "150px")]
  tpl <- UI.input
  out <- UI.textarea
    # set UI.enabled False
    # set UI.style [("height", "150px")]
  getBody win
    # set UI.style
      [ ("display", "flex")
      , ("flex-direction", "column")
      ]
    #+
    [ pure inp
    , pure tpl
    , pure out
    ]
  UI.setFocus tpl
  -- state
  ref <- liftIO $ newIORef State
    { template   = Right []
    , input      = []
    , inputEl    = inp
    , templateEl = tpl
    , outputEl   = out
    }
  let i = unlines $ map T.unpack fakeInput
  set UI.value i $ pure inp
  changeInput ref i
  -- events
  on E.valueChange inp $ changeInput ref
  on E.valueChange tpl $ changeTemplate ref
  pure ()

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
    { input = map (Lhx.makeInput (Separator " "))
      $ T.lines $ T.pack val }
  applyState ref

changeTemplate :: IORef State -> String -> UI ()
changeTemplate ref val = do
  State{templateEl = tpl, template = t} <-
    pureModifyState ref \s -> s
      { template = Lhx.makeTemplate $ T.pack val }
  case t of
    Left es  ->
      let ttl = unlines [T.unpack e | Lhx.Error e <- es]
      in pure tpl
        # set UI.style [("color", "red")]
        # set UI.title__ ttl
    Right _ ->
      pure tpl
      # set UI.style [("color", "initial")]
      # set UI.title__ ""
  applyState ref

pureModifyState :: IORef State -> (State -> State) -> UI State
pureModifyState ref f = liftIO $ atomicModifyIORef' ref \s ->
  let s' = f s
  in (s', s')
