{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}

module Control.OperationalTransformation.JSON.Gen
  ( genOperation
  ) where

import Control.OperationalTransformation.JSON
import Control.OperationalTransformation.Properties (ArbitraryFor (..))
import qualified Data.Aeson as A

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.QuickCheck hiding (Result)

-- |TODO: generate arbitrary JSONOperations
genOperation :: A.Value -> Gen JSONOperation
genOperation = undefined

instance ArbitraryFor A.Value JSONOperation where
  arbitraryFor = genOperation

identifier :: Gen T.Text
identifier = elements ["foo", "bar", "baz"]

instance Arbitrary T.Text where
  arbitrary = identifier

object :: Gen A.Value
object = do
  x <- elements [0..10]
  identifiers :: [T.Text] <- vector x
  elems <- vector x
  return $ A.Object $ HM.fromList $ zip identifiers elems

array :: Gen A.Value
array = do
  x <- elements [0..10]
  elems <- vector x
  return $ A.Array $ V.fromList elems

string :: Gen A.Value
string = do
  x <- elements [0..26]
  return $ A.String $ T.take x "abcdefghijklmnopqrstuvwxyz"

number :: Gen A.Value
number = A.Number <$> (elements $ fmap fromIntegral [0..100])

bool :: Gen A.Value
bool = A.Bool <$> arbitrary


instance Arbitrary A.Value where
  arbitrary = oneof [object, array, string, number, bool]

instance Arbitrary JSONOperation where
  arbitrary = arbitrary >>= genOperation
