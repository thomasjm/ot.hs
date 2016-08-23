{-# LANGUAGE OverloadedStrings #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where

import Data.Aeson
import Data.Text

type Property = Text
type Position = Int
data PathSegment = Prop Property | Pos Position
                 deriving Show
type Path = [PathSegment]

-- This is some random code I found on the internet
-- data JSONNumberOperation = Add Path Int
--                          deriving Show

-- data JSONStringOperation = StringInsert Path Position Text
--                          | StringDelete Path Position Text
--                          deriving Show

-- data JSONArrayOperation = ArrayInsert Path Position Value
--                         | ArrayDelete Path Position Value
--                         | ArrayReplace Path Position Value Value
--                         | ArrayMove Path Position Int
--                         deriving Show

-- data JSONObjectOperation = Insert Path Property Value
--                          | Delete Path Property
--                          | Replace Path Property Value Value
--                          deriving Show

instance FromJSON PathSegment where
  parseJSON (String a) = Prop <$> parseJSON (String a)
  parseJSON (Number a) = Pos  <$> parseJSON (Number a)
  parseJSON _ = fail "Invalid Pathsegment"

-- instance FromJSON JSONNumberOperation where
--   parseJSON (Object v) = Add <$> (v .: "p") <*> (v .: "na")
--   parseJSON _          = fail "Not an Object"


data JSONOperation = JSONOperation

invertOperation = undefined


-- https://github.com/josephg/ShareJS/blob/master/lib/types/json-api.js

-- get(path)
-- set(path, value, cb)
-- remove(path, len, cb)
-- insert(path, value, cb)
-- move(path, from, to, cb)
-- push(path, value, cb)
-- add(path, amount, cb)
-- getLength(path) -- just does this.get(path).length
-- deleteText(path, length, pos, cb)
-- hmm, why isn't there insertText?
--
--
--
