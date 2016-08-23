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

{a: [1, 0, {b: {}}]}

insert(["a", 0], 'qasdlkfasdfkljasdflkja') -> {a: ['q', 1, 0, {b: {}}]}

set(["a", 2, "bkasdflkasdlkfjasdflkjasdflkj"], {})

data JSONOperation = Get Path
  | Set Path
  | ...


instance OTOperation JSONOperation where
  transform (Set path) (Set path) = undefined

--
-- set(path, value)
-- remove(path, len)
-- insert(path, value)
-- move(path, from, to)
-- push(path, value)
-- add(path, amount)
-- deleteText(path, length, pos)

-- Misc:
-- get(path)
-- getLength(path) -- just does this.get(path).length
-- hmm, why isn't there insertText?
--
--
--
