module Lhx where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either
import Data.Bifunctor
import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Lhx.Parser hiding (Raw, Apply)
import Lhx.Parser qualified as Parser

newtype Error = Error { getError :: Text } deriving (Show, Eq)

newtype Separator = Separator { unSeparator :: Text } deriving (Show)

type Template = [Op]

data Op
  = Put Text
  | Apply Int [(Key, Text -> Either Error Text)]

newtype Key = Key { unKey :: [Text] } deriving (Eq, Ord, Show)

data Input = Input
  { iRaw    :: !Text
  , iFields :: [Text]
  } deriving (Show, Eq)

type Cache = Map Int (Map Key Text)

apply :: Template -> Input -> Either [Error] Text
apply tpl i =
  T.concat <$> traverseAllM (runExceptT . applyOpTo i) tpl `evalState` Map.empty

applyOpTo
  :: (MonadState Cache m, MonadError Error m)
  => Input -> Op -> m Text
applyOpTo _ (Put t) = pure t
applyOpTo inp (Apply idx steps) = do
  cache <- gets (Map.lookup idx) >>= \case
    Nothing ->
      let new = Map.empty
      in new <$ modify (Map.insert idx new)
    Just x -> pure x
  t <- liftEither $ at idx inp
  (res, cache') <- runStateT (go t steps) cache
  res <$ modify (Map.insert idx cache')
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

makeInput :: Separator -> Text -> Input
makeInput (Separator sep) s = Input s (T.splitOn sep s)

functions :: [(FName, Text -> Either Error Text)]
functions =
  [ (FName "rev", Right . T.reverse)
  , (FName "strip", Right . T.strip)
  , (FName "lstrip", Right . T.stripStart)
  , (FName "rstrip", Right . T.stripEnd)
  ]

lookupFunction :: FName -> Either Error (Text -> Either Error Text)
lookupFunction n =
  maybe (Left $ Error $ "Unknown function: " <> unFName n) Right
  $ lookup n functions

buildTemplate :: [Chunk] -> Either [Error] Template
buildTemplate = bimap concat concat . traverseAll (fmap (:[]) . useChunk)
  where
    useChunk :: Parser.Chunk -> Either [Error] Op
    useChunk (Parser.Raw t) = Right (Put t)
    useChunk (Parser.Apply idx ns) = do
      fs <- traverseAll lookupFunction ns
      pure $ Apply idx $ snd $ foldl' step ([], []) $ zip ns fs
    step (p, rs) (FName n, f) =
      let p' = n : p
      in (p', (Key p', f) : rs)

traverseAll :: (a -> Either b c) -> [a] -> Either [b] [c]
traverseAll f = runIdentity . traverseAllM (pure . f)

traverseAllM :: Monad m => (a -> m (Either b c)) -> [a] -> m (Either [b] [c])
traverseAllM f xs = do
  ys <- mapM f xs
  pure $ case lefts ys of
    [] -> Right $ rights ys
    es -> Left es

makeTemplate :: Text -> Either [Error] Template
makeTemplate = buildTemplate <=< first wrap . parse
  where
    wrap err = [Error err]

at :: Int -> Input -> Either Error Text
at 0 Input{iRaw = raw} = Right raw
at ix Input{iFields = fs} =
  case drop (abs ix - 1) (prepare fs) of
    (x:_) -> Right x
    _     -> Left . Error $ "Index is out of range: " <> T.pack (show ix)
  where
    prepare
      | ix < 0    = reverse
      | otherwise = id

errorsToText :: [Error] -> Text
errorsToText = T.unlines . map getError
