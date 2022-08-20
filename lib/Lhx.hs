module Lhx where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.Text as T

import Lhx.Parser

newtype Error = Error Text deriving Show

type Template = [Op]
type Op = (Input -> Either Error Text)

data Input = Input
  { iRaw    :: !Text
  , iFields :: [Text]
  }

apply :: Template -> Input -> Either [Error] Text
apply tpl = repack . foldMap wrap . sequenceA tpl
  where
    repack (t, []) = Right t
    repack (_, es) = Left es
    wrap (Right t) = (t, [])
    wrap (Left  e) = ("", [e])

fromText :: Text -> Input
fromText s = Input s (splitOn " " s)

functions :: [(FName, (Text -> Either Error Text))]
functions =
  [ (FName "rev", Right . T.reverse)
  ]

makeTemplate :: [Chunk] -> Either [Error] Template
makeTemplate = repack . foldMap wrap
  where
    repack (cs, []) = Right cs
    repack (_,  es) = Left es
    wrap (Raw t) = ok (Right . pure t)
    wrap (Apply ix Nothing) = ok (at ix)
    wrap (Apply ix (Just n)) =
      case lookup n functions of
        Nothing -> oops $ Error $ "Unknown function: " <> unFName n
        Just f  -> ok (f <=< at ix)
    ok   x = ([x], [])
    oops x = ([], [x])

at :: Int -> Input -> Either Error Text
at 0 Input{iRaw = raw} = Right raw
at ix Input{iFields = fs}
  | ix < Prelude.length fs = Right $ fs !! (ix - 1)
  | otherwise = Left . Error $ "Index is out of range: " <> T.pack (show ix)
