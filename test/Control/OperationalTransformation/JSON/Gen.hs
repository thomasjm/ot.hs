{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Control.OperationalTransformation.JSON.Gen
  ( genOperation, genMultiOperation
  ) where

import Control.OperationalTransformation
import Control.OperationalTransformation.JSON
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.Properties (ArbitraryFor (..))
import qualified Data.Aeson as A
import Data.Convertible
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector as V
import Safe
import Test.QuickCheck hiding (Result)

instance ArbitraryFor A.Value JSONOp where
  arbitraryFor = genOp

newtype Identifier = Identifier { unIdentifier :: T.Text }

instance Arbitrary Identifier where
  arbitrary = Identifier <$> identifier

instance Arbitrary A.Value where
  arbitrary = oneof [object, array, string, number, bool, return A.Null]

instance Arbitrary JSONOp where
  arbitrary = arbitrary >>= genOp


genMultiOperation :: A.Value -> Gen JSONOperation
genMultiOperation s = do
  n <- elements [2..10]
  genMultiOperation' n s (JSONOperation [])

genMultiOperation' :: Int -> A.Value -> JSONOperation -> Gen JSONOperation
genMultiOperation' 0 doc ops = return ops
genMultiOperation' n doc (JSONOperation ops) = do
  op@(JSONOperation [singleOp]) <- genOperation doc
  let Right doc' = apply op doc
  genMultiOperation' (n - 1) doc' (JSONOperation (ops ++ [singleOp]))


genOperation :: A.Value -> Gen JSONOperation
genOperation val = do
  op <- genOp val
  return $ JSONOperation [op]

genOp :: A.Value -> Gen JSONOp
genOp = flip genOp' []

genOp' :: A.Value -> Path -> Gen JSONOp
genOp' (A.Object m) path = oneof $ [
  return Identity,

  -- Insert a new key into the object
  do
    key <- suchThat identifier (\x -> (not $ x `HM.member` m))
    ObjectInsert path (Just key) <$> arbitrary
  ]
  -- Operations if the object is non-empty
  ++ (if (not $ HM.null m) then [
         -- Recurse
         (elements $ HM.toList m) >>= \(k, v) -> genOp' v (path ++ [Prop k]),

         -- Delete a key from the object
         (elements $ HM.toList m) >>= \(k, v) -> return $ ObjectDelete path (Just k) v,

         -- Replace a key from the object
         (elements $ HM.toList m) >>= \(k, v) -> ObjectReplace path (Just k) v <$> arbitrary
         ] else [])
genOp' (A.Array v) path = oneof $ [
  return Identity,

  -- Insert an item
  (elements [0 .. (V.length v)]) >>= \i -> ListInsert path i <$> arbitrary
  ]
  -- Operations if the list is non-empty
  ++ (if (not $ V.null v) then [
         -- Recurse
         (elements [0 .. ((V.length v) - 1)]) >>= \i -> genOp' (v V.! i) (path ++ [Pos i]),

         -- Delete an item from the list
         (elements [0 .. ((V.length v) - 1)]) >>= \i -> return $ ListDelete path i (v V.! i),

         -- Replace an item
         (elements [0 .. ((V.length v) - 1)]) >>= \i -> ListReplace path i (v V.! i) <$> arbitrary,

         -- Move an item (note that we allow moving to the same index)
         (elements [0 .. ((V.length v) - 1)]) >>= \i1 ->
           (elements [0 .. ((V.length v) - 1)]) >>= \i2 ->
             return $ ListMove path i1 i2
         ] else [])
genOp' (A.String s) path = oneof [
  -- Insert a string
  (elements [0 .. (T.length s)]) >>= \i -> StringInsert path i <$> randomString,

  -- Delete a string
  (elements [0 .. (T.length s)]) >>= \i1 ->
    (elements [i1 .. (T.length s)]) >>= \i2 ->
      return $ StringDelete path i1 (T.take (i2 - i1) $ T.drop i1 s)
  ]
genOp' (A.Number n) path = Add path <$> arbitrary
-- Booleans get flipped
genOp' (A.Bool b) path@(lastMay -> Just (Pos x)) = return $ ListReplace (init path) x (A.Bool b) (A.Bool $ not b)
genOp' (A.Bool b) path@(lastMay -> Just (Prop x)) = return $ ObjectReplace (init path) (Just x) (A.Bool b) (A.Bool $ not b)
genOp' (A.Bool b) path@(lastMay -> Nothing) = return $ ObjectReplace [] Nothing (A.Bool b) (A.Bool $ not b)
-- Nulls just get left alone
genOp' (A.Null) path = return Identity
--TODO: add tests of subtype operations?



-- | Identifiers are things like foo123 or baz42
identifier :: Gen T.Text
identifier = do
  starter <- elements ["foo", "bar", "baz"]
  num <- elements [0..1000]
  return $ starter <> (convert $ show num)

-- | Random strings are ordered sequences of lowercase letters
randomString :: Gen T.Text
randomString = (elements [0..26]) >>= \x -> return $ T.take x "abcdefghijklmnopqrstuvwxyz"

-- | Objects have random identifier keys and random aeson values
-- | with an exponentially decreasing size parameter
object :: Gen A.Value
object = scale (\n -> quot n 2) $ sized $ \size -> do
  x <- elements [0..size]
  identifiers :: [T.Text] <- (fmap unIdentifier) <$> vector x
  elems <- vector x
  return $ A.Object $ HM.fromList $ zip identifiers elems

-- | Arrays are random aeson values with an exponentially decreasing size parameter
array :: Gen A.Value
array = scale (\n -> quot n 2) $ sized $ \size -> do
  x <- elements [0..size]
  elems <- vector x
  return $ A.Array $ V.fromList elems

-- | Numbers are from 0 to 100
number :: Gen A.Value
number = A.Number <$> (elements $ fmap fromIntegral [0..100])

string :: Gen A.Value
string = A.String <$> randomString

bool :: Gen A.Value
bool = A.Bool <$> arbitrary
