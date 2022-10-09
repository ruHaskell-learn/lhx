{-# LANGUAGE RecordWildCards #-}

module Lhx.Streaming
  ( App(..)
  , defaultApp
  , run
  , stdin, stdout
  ) where

import Data.Bifunctor
import Data.Text.Encoding qualified as Encoding
import Data.Text (Text)
import Data.Text qualified as T
import Streaming hiding (run)
import Streaming.Prelude qualified as S
import Streaming.ByteString.Char8 qualified as SB

data App m a = App
  { aPreviewSize     :: Int
  , aPrepare         :: Text -> m a
  , aMakeTransformer :: [a] -> m (Maybe (a -> Maybe Text))
  }

defaultApp :: Monad m => App m Text
defaultApp = App
  { aPreviewSize = 10
  , aPrepare = pure
  , aMakeTransformer = const . pure $ Just Just
  }

run
  :: Monad m
  => App m a
  -> Stream (Of Text) m () -> m (Stream (Of Text) m ())
run App{..} inp = do
  xs :> rest <- toList $ S.splitAt aPreviewSize inp
  ys <- traverse aPrepare xs
  aMakeTransformer ys >>= \case
    Nothing -> pure never
    Just stepper -> do
      let
        rs = S.mapMaybe stepper $ S.each ys
        ss = S.mapMaybeM (step stepper) rest
      pure $ concats $ yields rs >> yields ss
  where
    step stepper = fmap stepper . aPrepare

stdin :: MonadIO m => Stream (Of Text) m ()
stdin =
  maps (first Encoding.decodeUtf8)
  . mapped SB.toStrict
  $ SB.lines SB.stdin

stdout :: MonadIO m => Stream (Of Text) m () -> m ()
stdout = S.stdoutLn . maps (first T.unpack)  -- TODO: rewrite with SB

toList :: Monad m => Stream (Of a) m r -> m (Of [a] r)
toList = S.fold (flip (:)) [] reverse
