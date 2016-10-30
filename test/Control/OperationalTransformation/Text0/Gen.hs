{-# LANGUAGE OverloadedStrings #-}

module Control.OperationalTransformation.Text0.Gen (genOperation) where

import Control.OperationalTransformation.Text0
import qualified Data.Text as T
import Test.QuickCheck hiding (Result)

instance Arbitrary T.Text where
  arbitrary = T.pack <$> listOf (choose ('!', '~'))

genOperation :: T.Text -> Gen Text0Operation
genOperation s = oneof [
  -- Insert a string
  (elements [0 .. (T.length s)]) >>= \i -> do
      randomStr <- randomString
      return $ T0 [TextInsert i randomStr],

  -- Delete a string
  (elements [0 .. (T.length s)]) >>= \i1 ->
    (elements [i1 .. (T.length s)]) >>= \i2 ->
      return $ T0 [TextDelete i1 (T.take (i2 - i1) $ T.drop i1 s)]
  ]


-- | Random strings are ordered sequences of lowercase letters
randomString :: Gen T.Text
randomString = (elements [0..26]) >>= \x -> return $ T.take x "abcdefghijklmnopqrstuvwxyz"
