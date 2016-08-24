{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Control.OperationalTransformation.JSON.Types
( JSONOperation(..)
  , Path
  , PathSegment(..)
) where

import Control.OperationalTransformation.Text (TextOperation)
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
-- import Debug.Trace

-- * PathSegment and Path

data PathSegment
  = Prop T.Text
  | Pos Int
  deriving (Show, Eq)

type Path = [PathSegment]

instance FromJSON PathSegment where
  parseJSON (String x) = return $ Prop x
  parseJSON (Number x) = return $ Pos $ round x
  parseJSON _ = fail "Invalid Pathsegment"

-- * JSONOperation

-- Based on the "Summary of operations" at https://github.com/ottypes/json0
-- See also https://github.com/josephg/ShareJS/blob/master/lib/types/json-api.js
data JSONOperation
  -- * Numbers

  -- adds the number to the number at [path]
  = Add Path Int

  -- * Lists

  -- inserts the object obj before the item at idx in the list at [path]
  | ListInsert Path Int A.Value
  -- deletes the object obj from the index idx in the list at [path]
  | ListDelete Path Int A.Value
  -- replaces the object before at the index in the list at [path] with the object after
  | ListReplace Path Int A.Value A.Value
  -- moves the object at idx1 such that the object will be at index idx2 in the list at [path]
  | ListMove Path Int Int

  -- * Objects

  -- inserts the object obj into the object at [path] with key key
  | ObjectInsert Path T.Text A.Value
  -- deletes the object obj with key key from the object at [path]
  | ObjectDelete Path T.Text A.Value
  -- replaces the object before with the object after at key key in the object at [path]
  | ObjectReplace Path T.Text A.Value A.Value

  -- * Subtypes

  -- applies the subtype op o of type t to the object at [path]
  | ApplySubtypeOperation Path TextOperation
  -- inserts the string s at offset offset into the string at [path] (uses subtypes internally)
  | InsertString Path Int T.Text
  -- deletes the string s at offset offset from the string at [path] (uses subtypes internally)
  | DeleteString Path Int T.Text

  deriving (Eq, Show)

instance ToJSON JSONOperation where
  toJSON (Add a b) = undefined

instance FromJSON JSONOperation where
  parseJSON (A.Object v) | "na" `elem` (HM.keys v) = Add <$> v .: "p" <*> v .: "na"

  -- Lists
  parseJSON (A.Object v) | "li" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             obj <- v .: "li"
                             return $ ListInsert path index obj
  parseJSON (A.Object v) | "ld" `elem` (HM.keys v) && "li" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             before <- v .: "ld"
                             after <- v .: "li"
                             return $ ListReplace path index before after
  parseJSON (A.Object v) | "ld" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             obj <- v .: "ld"
                             return $ ListDelete path index obj
  parseJSON (A.Object v) | "lm" `elem` (HM.keys v) = do
                             (path, index1) <- parsePathAndIndex v
                             index2 <- v .: "lm"
                             return $ ListMove path index1 index2

  -- Objects
  parseJSON (A.Object v) | "oi" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             obj <- v .: "oi"
                             return $ ObjectInsert path prop obj
  parseJSON (A.Object v) | "od" `elem` (HM.keys v) && "oi" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             before <- v .: "od"
                             after <- v .: "oi"
                             return $ ObjectReplace path prop before after
  parseJSON (A.Object v) | "od" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             obj <- v .: "od"
                             return $ ObjectDelete path prop obj

  -- Subtypes
  parseJSON (A.Object v) | "o" `elem` (HM.keys v) = ApplySubtypeOperation <$> v .: "p" <*> v .: "o"
  parseJSON (A.Object v) | "si" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             str <- v .: "si"
                             return $ InsertString path index str
  parseJSON (A.Object v) | "sd" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             str <- v .: "sd"
                             return $ DeleteString path index str
  parseJSON _ = fail "Failed to parse operation"



parsePathAndIndex :: A.Object -> A.Parser ([PathSegment], Int)
parsePathAndIndex v = do
  pathAndIndex :: [PathSegment]  <- v .: "p"
  let path = init pathAndIndex
  let (Pos index) = last pathAndIndex
  return (path, index)

parsePathAndProp :: A.Object -> A.Parser ([PathSegment], T.Text)
parsePathAndProp v = do
  pathAndProp :: [PathSegment]  <- v .: "p"
  let path = init pathAndProp
  let (Prop prop) = last pathAndProp
  return (path, prop)
