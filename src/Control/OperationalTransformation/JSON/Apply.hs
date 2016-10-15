{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, QuasiQuotes, OverloadedLists #-}

module Control.OperationalTransformation.JSON.Apply where

import Control.Lens hiding (Identity)
import Control.OperationalTransformation as OT
import Control.OperationalTransformation.JSON.QuasiQuote (j)
-- import Control.OperationalTransformation.JSON.Transform ()
import Control.OperationalTransformation.JSON.Types
import Data.Aeson as A
import Data.String.Interpolate.IsString
import Data.Aeson.Lens
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V

pathSegmentToTraversal (Pos x) = nth x
pathSegmentToTraversal (Prop k) = key k

pathToTraversal path = foldl (.) id $ map pathSegmentToTraversal path

listAtPath   path = (pathToTraversal path) . _Array
objectAtPath path = (pathToTraversal path) . _Object
stringAtPath path = (pathToTraversal path) . _String
numberAtPath path = (pathToTraversal path) . _Number

vectorInsert :: V.Vector a -> Int -> a -> V.Vector a
vectorInsert vec index item = beginning <> [item] <> rest where
  (beginning, rest) = V.splitAt index vec

vectorDelete :: V.Vector a -> Int -> a -> V.Vector a
vectorDelete vec index item = beginning <> (V.drop 1 rest) where
  (beginning, rest) = V.splitAt index vec

vectorMove :: V.Vector a -> Int -> Int -> V.Vector a
vectorMove vec index1 index2 = V.fromList $ newBeginning ++ (item : newRest) where
  list = V.toList vec
  (beginning, (item:rest)) = splitAt index1 list
  removed = beginning ++ rest
  (newBeginning, newRest) = splitAt (if index1 < index2 then index2 else index2) removed

-- TODO: handle invalid index
stringInsert :: T.Text -> Int -> T.Text -> T.Text
stringInsert haystack pos needle = beginning <> needle <> rest
  where (beginning, rest) = T.splitAt pos haystack

-- TODO: check the value matches
stringDelete :: T.Text -> Int -> T.Text -> T.Text
stringDelete haystack pos needle = beginning <> (T.drop (T.length needle) rest)
  where (beginning, rest) = T.splitAt pos haystack

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
  Nothing -> Left [i|Couldn't find list at path #{show path} in ListMove|]
  Just l -> Right $ set (listAtPath path) (vectorMove l pos1 pos2) input

apply (ObjectInsert path (Just k) value) input = case input ^? (objectAtPath path) of
  Nothing -> Left "Couldn't find object in ObjectInsert"
  Just _ -> Right $ set ((objectAtPath path) . (at k)) (Just value) input
apply (ObjectInsert _ Nothing value) _ = Right value

-- TODO: assert value matches
apply (ObjectDelete path (Just k) _value) input = case input ^? (objectAtPath path) of
  Nothing -> Left "Couldn't find object in ObjectInsert"
  Just _ -> Right $ set ((objectAtPath path) . (at k)) Nothing input
-- TODO: assert value matches
apply (ObjectDelete _ Nothing _) _ = Right A.Null

-- TODO: assert old matches
apply (ObjectReplace path (Just k) _old new) input = Right $ set ((pathToTraversal path) . (key k)) new input
-- TODO: assert old matches
apply (ObjectReplace path Nothing _old new) input = Right new

apply (ApplySubtypeOperation path typ op) input = case input ^? stringAtPath path of
  Nothing -> Left "Couldn't find text in ApplySubtypeOperation"
  Just t -> case OT.apply op t of
    Left err -> Left err
    Right result -> Right $ set (stringAtPath path) result input

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
