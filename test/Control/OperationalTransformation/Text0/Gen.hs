{-# LANGUAGE OverloadedStrings #-}

module Control.OperationalTransformation.Text0.Gen (genOperation, genMultiOperation) where

import Control.OperationalTransformation
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

genMultiOperation :: T.Text -> Gen Text0Operation
genMultiOperation s = do
  n <- elements [2..10]
  genMultiOperation' n s (T0 [])

genMultiOperation' :: Int -> T.Text -> Text0Operation -> Gen Text0Operation
genMultiOperation' 0 doc ops = return ops
genMultiOperation' n doc (T0 ops) = do
  op@(T0 [singleOp]) <- genOperation doc
  let Right doc' = apply op doc
  genMultiOperation' (n - 1) doc' (T0 (ops ++ [singleOp]))


-- | Random strings are ordered sequences of lowercase letters
randomString :: Gen T.Text
randomString = (elements [0..26]) >>= \x -> return $ T.take x "abcdefghijklmnopqrstuvwxyz"
