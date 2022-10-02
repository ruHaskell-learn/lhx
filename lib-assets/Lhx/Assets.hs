{-# LANGUAGE TemplateHaskell #-}

module Lhx.Assets where

import Data.ByteString
import Data.FileEmbed
import Data.Text

ico :: Text
ico = "data:image/x-icon;base64,\
  \AAABAAEAEBAQAAEABAAoAQAAFgAAACgAAAAQAAAAIAAAAAEABAAAAAAAgAAAAC4BAAAuAQAAEAAA\
  \ABAAAAAAEwEAAC8AAAB4BAAGEwYAAnYCAEcvdQAAAP8AAIAAAACKUgAAn58AAOTkAP8AAABNNE0A\
  \ewN7ADFPMQCMAHQAz+/f7O7+/v715fX1XF9cX+REXzVTPzRO03cQmqkVd83d0nIpoScu3d3uR3dy\
  \JO7d3UEREAABBN3dQAlZhZAE3d1BCYmYkATd3UERAFARFN3d7kcid3Tt3d3id4qody7d3nggCqUC\
  \dz3ENTM1UzNETMbG5vW/vLy/7+/s787+/v4AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\
  \AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"

svg :: ByteString
svg = $(embedFile "data/lhx.svg.gz")
