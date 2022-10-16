{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lhx
  ( Error(), errorText
  , Input(..), Separator(..), makeInput
  , Template, makeTemplate, apply, functions
  ) where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Validation
import Data.Bifunctor
import Data.List (scanl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

import Lhx.Parser hiding (Raw, Apply)
import Lhx.Parser qualified as Parser

newtype Error = Error [Text] deriving (Show, Eq, Semigroup, Monoid)

newtype Separator = Separator { unSeparator :: Text } deriving (Show)

type Template = [Op]

data Op
  = Put Text
  | Apply Int [(Key, Transformation)]

type Transformation = Text -> Either Error Text

newtype Key = Key { unKey :: [Text] } deriving (Eq, Ord, Show)

data Input = Input
  { iRaw    :: !Text
  , iFields :: [Text]
  } deriving (Show, Eq)

type Cache = Map Int (Map Key Text)

apply :: Template -> Input -> Either Error Text
apply tpl i =
  T.concat <$>
  runValidationTEither (traverse (applyOpTo i) tpl)
  `evalState` Map.empty

applyOpTo :: Input -> Op -> ValidationT Error (State Cache) Text
applyOpTo _ (Put t) = pure t
applyOpTo inp (Apply idx steps) = do
  cache <- lift (gets $ Map.lookup idx) >>= \case
    Nothing ->
      let new = Map.empty
      in new <$ lift (modify $ Map.insert idx new)
    Just x -> pure x
  liftEitherAsWarning "" $ runExceptT do
    t <- liftEither $ at idx inp
    (res, cache') <- runStateT (go t steps) cache
    res <$ lift (modify $ Map.insert idx cache')
  where
    go t [] = pure t
    go t ((k, f) : fs) = do
      cache <- get
      case Map.lookup k cache of
        Just r -> pure r
        Nothing -> do
          t' <- go t fs
          r <- liftEither $ f t'
          modify $ Map.insert k r
          pure r

liftEitherAsWarning :: (Monad m, Monoid e) => a -> m (Either e a) -> ValidationT e m a
liftEitherAsWarning def body = lift body >>= \case
  Left e -> def <$ vWarning e
  Right x -> pure x

makeInput :: Separator -> Text -> Input
makeInput (Separator sep) s = Input s (T.splitOn sep s)

functions :: [(FName, Transformation)]
functions =
  [ (FName "rev", Right . T.reverse)
  , (FName "strip", Right . T.strip)
  , (FName "lstrip", Right . T.stripStart)
  , (FName "rstrip", Right . T.stripEnd)
  ]

lookupFunction :: Monad m => FName -> ValidationT Error m (Maybe Transformation)
lookupFunction n = do
  let f = lookup n functions
  when (isNothing f) $
    vWarning $ Error ["Unknown function: " <> unFName n]
  pure f

buildTemplate :: [Chunk] -> Either Error Template
buildTemplate =
  fmap catMaybes . runIdentity . runValidationTEither . traverse useChunk
  where
    useChunk (Parser.Raw t) = pure . Just $ Put t
    useChunk (Parser.Apply idx ns) =
      fmap (Apply idx . addKeys) . sequence <$> traverse lookupFunction ns
      where
        addKeys = zip (buildPrefixesReversed ns)

-- | Builds a list of prefixes (reversed) from a list of function names
--
-- >>> buildPrefixesReversed [FName "f", FName "g", FName "h"]
-- [Key {unKey = ["f"]},Key {unKey = ["g","f"]},Key {unKey = ["h","g","f"]}]
buildPrefixesReversed :: [FName] -> [Key]
buildPrefixesReversed = tail . scanl' step (Key [])
  where
    step (Key p) (FName n) = Key (n : p)

makeTemplate :: Text -> Either Error Template
makeTemplate = buildTemplate <=< first wrap . parse
  where
    wrap err = Error [err]

at :: Int -> Input -> Either Error Text
at 0 Input{iRaw = raw} = Right raw
at ix Input{iFields = fs} =
  case drop (abs ix - 1) (prepare fs) of
    (x:_) -> Right x
    _     -> Left $ Error ["Index is out of range: " <> T.pack (show ix)]
  where
    prepare
      | ix < 0    = reverse
      | otherwise = id

errorText :: Error -> Text
errorText (Error xs) = T.unlines xs
