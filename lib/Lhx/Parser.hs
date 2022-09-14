module Lhx.Parser
  ( FName(..)
  , Chunk(..)
  , parse
  ) where

import Data.Bifunctor
import Data.Void
import Data.Text as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (signed, decimal)

newtype FName = FName { unFName :: Text } deriving (Show, Eq)

data Chunk
  = Raw Text
  | Apply Int [FName]
  deriving (Show, Eq)

type Parser a = Parsec Void Text a

parse :: Text -> Either Text [Chunk]
parse =
  first (T.pack . errorBundlePretty)
  . MP.parse templateP ""

templateP :: Parser [Chunk]
templateP = Prelude.concat <$> many tokenP

tokenP :: Parser [Chunk]
tokenP =
  try escapeP
  <|> applyP
  <|> (:[]) <$> rawP

rawP :: Parser Chunk
rawP = Raw . T.pack <$> some (satisfy (/= '$'))

applyP :: Parser [Chunk]
applyP =
  char '$' *> do
    app <- applyP'
    r <- optional rawP
    case r of
      Just (Raw t)
        | T.take 1 t == ":" -> fail "Expected ;"
        | otherwise -> pure [app, Raw t]
      _ -> pure [app]
  where
    applyP' =
      (Apply
        <$> signed (pure ()) decimal
        <*> (try (char ':' *> namesP) <|> pure []))
      <|>
      (Apply 0 <$> namesP)

namesP :: Parser [FName]
namesP = (fmap FName identP `sepBy1` char ':') <* char ';'

identP :: Parser Text
identP =
  cons
  <$> lowerChar
  <*> (T.pack <$> many (lowerChar <|> digitChar))

escapeP :: Parser [Chunk]
escapeP = char '$' *> char '$' *> pure [Raw "$"]
