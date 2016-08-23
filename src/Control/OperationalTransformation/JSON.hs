{-# LANGUAGE OverloadedStrings #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where

import Control.OperationalTransformation
import Data.Aeson
import qualified Data.Aeson as A
import Data.Text

type Property = Text
type Position = Int
data PathSegment = Prop Property | Pos Position
                 deriving Show
type Path = [PathSegment]


instance FromJSON PathSegment where
  parseJSON (String a) = Prop <$> parseJSON (String a)
  parseJSON (Number a) = Pos  <$> parseJSON (Number a)
  parseJSON _ = fail "Invalid Pathsegment"


invertOperation = undefined

-- https://github.com/josephg/ShareJS/blob/master/lib/types/json-api.js

-- {a: [1, 0, {b: {}}]}

-- insert(["a", 0], 'qasdlkfasdfkljasdflkja') -> {a: ['q', 1, 0, {b: {}}]}

-- set(["a", 2, "bkasdflkasdlkfjasdflkjasdflkj"], {})

newtype Length = Length Int
newtype Index = Index Int


data JSONOperation =
  Get Path
  | Set Path
  | Remove Path Int
  | Insert Path A.Value
  | Move Path Path Path -- ???
  | Add Path Int
  | DeleteText Path Length Index


instance OTOperation JSONOperation where
  transform (Set path1) (Set path2) = undefined


-- Misc:
-- get(path)
-- getLength(path) -- just does this.get(path).length
-- hmm, why isn't there insertText?
--
--
--
