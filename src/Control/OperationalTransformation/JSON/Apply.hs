{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Apply where

import Control.Lens hiding (Identity)
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Transform ()
import Control.OperationalTransformation.JSON.Types
import Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.Vector as V


pathSegmentToTraversal (Pos x) = nth x
pathSegmentToTraversal (Prop k) = key k

pathToTraversal path = foldl (.) id $ [pathSegmentToTraversal x | x <- path]

listAtPath path = (pathToTraversal path) . _Array
objectAtPath path = (pathToTraversal path) . _Object
stringAtPath path = (pathToTraversal path) . _String
numberAtPath path = (pathToTraversal path) . _Number

vectorInsert :: V.Vector A.Value -> Int -> A.Value -> V.Vector A.Value
vectorInsert vec index item = undefined

vectorDelete :: V.Vector A.Value -> Int -> A.Value -> V.Vector A.Value
vectorDelete vec index item = undefined

stringInsert :: T.Text -> Int -> T.Text -> T.Text
stringInsert = undefined

stringDelete :: T.Text -> Int -> T.Text -> T.Text
stringDelete = undefined


apply :: JSONOperation -> A.Value -> Either String A.Value
apply Identity input = Right input

apply (Add path n) input = case input ^? (numberAtPath path) of
  Nothing -> Left "Couldn't find number in Add"
  Just x -> Right $ set (numberAtPath path) ((fromIntegral n) + x) input

apply (ListInsert path pos value) input = case input ^? (listAtPath path) of
  Nothing -> Left "Couldn't find list in ListInsert"
  Just l -> Right $ set (listAtPath path) (vectorInsert l pos value) input

apply (ListDelete path pos value) input = case input ^? (listAtPath path) of
  Nothing -> Left "Couldn't find list in ListDelete"
  Just l -> Right $ set (listAtPath path) (vectorDelete l pos value) input

apply (ListReplace path pos old new) input = case input ^? (listAtPath path) of
  Nothing -> Left "Couldn't find list in ListReplace"
  Just _ -> Right $ set ((pathToTraversal path) . (nth pos)) new input

-- TODO
apply (ListMove path pos1 pos2) input = case input ^? (listAtPath path) of
  Nothing -> Left "Couldn't find list in ListMove"
  Just _ -> error "ListMove apply not implemented"

apply (ObjectInsert path k value) input = case input ^? (objectAtPath path) of
  Nothing -> Left "Couldn't find object in ObjectInsert"
  Just _ -> Right $ set ((pathToTraversal path) . (key k)) value input

apply (ObjectDelete path k value) input = case input ^? (objectAtPath path) of
  Nothing -> Left "Couldn't find object in ObjectInsert"
  Just _ -> Right $ set ((pathToTraversal path)) value input

apply (ObjectReplace path k old new) input = Right $ set ((pathToTraversal path) . (key k)) new input

-- TODO
apply (ApplySubtypeOperation path typ op) input = error "ApplySubtypeOperation apply not implemented"

apply (StringInsert path pos s) input = case input ^? (stringAtPath path) of
  Nothing -> Left "Couldn't find string in StringInsert"
  Just str -> Right $ set (stringAtPath path) (stringInsert str pos s) input

apply (StringDelete path pos s) input = case input ^? (stringAtPath path) of
  Nothing -> Left "Couldn't find string in StringDelete"
  Just str -> Right $ set (stringAtPath path) (stringDelete str pos s) input


-- For playing in ghci
test_obj = [j|{
  key1: "value1",
  key2: 42,
  key3: ["a", "b", "c"]
}|]