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
import Text.Megaparsec.Char.Lexer (decimal)

newtype FName = FName { unFName :: Text } deriving (Show, Eq)

data Chunk
  = Raw Text
  | Apply Int (Maybe FName)
  deriving Show

type Parser a = Parsec Void Text a

parse :: Text -> Either Text [Chunk]
parse =
  first (T.pack . errorBundlePretty)
  . MP.parse templateP ""

templateP :: Parser [Chunk]
templateP = many tokenP

tokenP :: Parser Chunk
tokenP =
  applyP
  <|> Raw . T.pack <$> some (satisfy (/= '$'))

applyP :: Parser Chunk
applyP =
  char '$' *> (
    -- $0:foo
    try (Apply <$> (decimal <* char ':') <*> (Just . FName <$> identP))
    <|>
    -- $0
    (Apply <$> decimal <*> pure Nothing)
    <|>
    -- $foo
    (Apply 0 . Just . FName <$> identP)
  )

identP :: Parser Text
identP =
  cons
  <$> lowerChar
  <*> (T.pack <$> many (lowerChar <|> digitChar))
