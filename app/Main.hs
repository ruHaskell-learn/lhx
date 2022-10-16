module Main where

import System.IO
import System.Exit
import Control.Monad
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative

import Lhx

data Options = Options
  { expression     :: String
  , quiet          :: Bool
  , skipErrors     :: Bool
  , fieldSeparator :: String
  } deriving Show

main :: IO ()
main = do
  opts <- execParser cli
  tpl <- case makeTemplate (T.pack $ expression opts) of
    Right t -> pure t
    Left es -> do
      unless (quiet opts) $
        dumpErrors es
      exitWith (ExitFailure 1)
  ls <- map T.pack . lines <$> getContents
  let prepare = makeInput . Separator . T.pack $ fieldSeparator opts
  forM_ ls $ \l ->
    case apply tpl (prepare l) of
      Right v -> TIO.putStrLn v
      Left es ->
        unless (skipErrors opts) $ dumpErrors es

dumpErrors :: Error -> IO ()
dumpErrors = TIO.hPutStrLn stderr . Lhx.errorText

cli :: ParserInfo Options
cli = info (options <**> helper)
  ( fullDesc
    <> progDesc "expands some lines"
    <> header "Line Hyper Expander" )

options :: Parser Options
options = Options
  <$> strOption
    ( long "expression"
    <> short 'e'
    <> metavar "EXPR"
    <> help "Expression to expand" )
  <*> switch
    ( long "quiet"
    <> short 'q'
    <> help "Don't print any errors" )
  <*> switch
    ( long "skip-errors"
    <> help "Skip lines with errors" )
  <*> option nonEmptyStr
    ( long "field-separator"
    <> short 'f'
    <> metavar "STRING"
    <> value " "
    <> help "Sequence that separates input's fields" )

nonEmptyStr :: ReadM String
nonEmptyStr =
  eitherReader $ \s ->
    if null s
    then Left "Field separator can't be an empty string"
    else Right s
