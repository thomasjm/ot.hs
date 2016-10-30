{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.Text0.Specs where

import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.QuasiQuote
import Control.OperationalTransformation.Text0
import Data.Aeson as A
import qualified Data.Text as T
import qualified Test.Hspec as H
import Test.Hspec hiding (shouldBe)
import Test.Tasty
import Test.Tasty.Hspec hiding (shouldBe)

-- These tests are taken directly from
-- https://github.com/ottypes/json0/blob/master/test/text0.coffee

compose :: Text0Operation -> Text0Operation -> Text0Operation
compose op1 op2 = case C.compose op1 op2 of
  Left err -> error err
  Right ret -> ret

normalize = undefined

transformCursor = undefined
transformCursorLeft = undefined
transformCursorRight = undefined

transform :: Text0Operation -> Text0Operation -> (Text0Operation, Text0Operation)
transform op1 op2 = case C.transform op1 op2 of
  Left err -> error err
  Right x -> x

-- for ghci
d :: A.Value -> Text0Operation
d jsonValue = op
  where
    Success op = T0 <$> fromJSON jsonValue

apply :: T.Text -> Text0Operation -> T.Text
apply input op = case C.apply op input of
  Left err -> error err
  Right x -> x

transformLeft :: Text0Operation -> Text0Operation -> Text0Operation
transformLeft a b = a'
  where (a', _) = transform a b

transformRight :: Text0Operation -> Text0Operation -> Text0Operation
transformRight a b = a'
  where (_, a') = transform b a

shouldBe :: (Eq a, Show a) => a -> a -> Expectation
shouldBe = flip H.shouldBe

specs :: SpecWith ()
specs = do
  describe "compose" $ do
    -- Compose is actually pretty easy
    it "is sane" $ do
      shouldBe (compose [x|[]|] [x|[]|]) [x|[]|]
      shouldBe (compose [x|[{"i":"x", "p":0}]|] [x|[]|]) [x|[{"i":"x", "p":0}]|]
      shouldBe (compose [x|[]|] [x|[{"i":"x", "p":0}]|]) [x|[{"i":"x", "p":0}]|]
      shouldBe (compose [x|[{"i":"y", "p":100}]|] [x|[{"i":"x", "p":0}]|]) [x|[{"i":"y", "p":100}, {"i":"x", "p":0}]|]

  describe "transform" $ do
    it "is sane" $ do
      shouldBe [x|[]|] (transformLeft [x|[]|] [x|[]|])
      shouldBe [x|[]|] (transformRight [x|[]|] [x|[]|])

      shouldBe [x|[{"i":"y", "p":100}, {"i":"x", "p":0}]|] (transformLeft [x|[{"i":"y", "p":100}, {"i":"x", "p":0}]|] [x|[]|])
      shouldBe [x|[]|] (transformRight [x|[]|] [x|[{"i":"y", "p":100}, {"i":"x", "p":0}]|])

    it "inserts" $ do
      shouldBe ([x|[{"i":"x", "p":10}]|], [x|[{"i":"a", "p":1}]|]) (transform [x|[{"i":"x", "p":9}]|] [x|[{"i":"a", "p":1}]|])
      shouldBe ([x|[{"i":"x", "p":10}]|], [x|[{"i":"a", "p":11}]|]) (transform [x|[{"i":"x", "p":10}]|] [x|[{"i":"a", "p":10}]|])

      shouldBe ([x|[{"i":"x", "p":10}]|], [x|[{"d":"a", "p":9}]|]) (transform [x|[{"i":"x", "p":11}]|] [x|[{"d":"a", "p":9}]|])
      shouldBe ([x|[{"i":"x", "p":10}]|], [x|[{"d":"a", "p":10}]|]) (transform [x|[{"i":"x", "p":11}]|] [x|[{"d":"a", "p":10}]|])
      shouldBe ([x|[{"i":"x", "p":11}]|], [x|[{"d":"a", "p":12}]|]) (transform [x|[{"i":"x", "p":11}]|] [x|[{"d":"a", "p":11}]|])

      shouldBe [x|[{"i":"x", "p":10}]|] (transformLeft [x|[{"i":"x", "p":10}]|] [x|[{"d":"a", "p":11}]|])

      shouldBe [x|[{"i":"x", "p":10}]|] (transformLeft [x|[{"i":"x", "p":10}]|] [x|[{"d":"a", "p":10}]|])

      shouldBe ([x|[{"p":7,"i":"ab"}]|], [x|[{"p":0,"i":"abcdef"}]|]) (transform [x|[{"p":1,"i":"ab"}]|] [x|[{"p":0,"i":"abcdef"}]|])

      shouldBe [x|[{"i":"x", "p":10}]|] (transformRight [x|[{"i":"x", "p":10}]|] [x|[{"d":"a", "p":10}]|])

    it "deletes" $ do
      shouldBe ([x|[{"d":"abc", "p":8}]|], [x|[{"d":"xy", "p":4}]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"xy", "p":4}]|])
      shouldBe ([x|[{"d":"ac", "p":10}]|], [x|[]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"b", "p":11}]|])
      shouldBe ([x|[]|], [x|[{"d":"ac", "p":10}]|]) (transform [x|[{"d":"b", "p":11}]|] [x|[{"d":"abc", "p":10}]|])
      shouldBe ([x|[{"d":"a", "p":10}]|], [x|[]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"bc", "p":11}]|])
      shouldBe ([x|[{"d":"c", "p":10}]|], [x|[]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"ab", "p":10}]|])
      shouldBe ([x|[{"d":"a", "p":10}]|], [x|[{"d":"d", "p":10}]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"bcd", "p":11}]|])
      shouldBe ([x|[{"d":"d", "p":10}]|], [x|[{"d":"a", "p":10}]|]) (transform [x|[{"d":"bcd", "p":11}]|] [x|[{"d":"abc", "p":10}]|])
      shouldBe ([x|[{"d":"abc", "p":10}]|], [x|[{"d":"xy", "p":10}]|]) (transform [x|[{"d":"abc", "p":10}]|] [x|[{"d":"xy", "p":13}]|])

      shouldBe ([x|[{"p":1,"d":"q"},{"p":6,"d":"3"}]|], [x|[{"p":1,"i":"abcde"}]|])
               (transform [x|[{"p":1,"d":"q3"}]|] [x|[{"p":2,"i":"abcde"}]|])

  -- describe "transformCursor" $ do
  --   it "is sane" $ do
  --     shouldBe 0 (transformCursorRight 0 [x|[]|])
  --     shouldBe 0 (transformCursorLeft 0 [x|[]|])
  --     shouldBe 100 (transformCursor 100 [x|[]|])

  --   it "works vs insert" $ do
  --     shouldBe 0 (transformCursorRight 0 [x|{"i":"asdf", "p":100}|])
  --     shouldBe 0 (transformCursorLeft 0 [x|{"i":"asdf", "p":100}|])

  --     shouldBe 204 (transformCursorRight 200 [x|[{"i":"asdf", "p":100}]|])
  --     shouldBe 204 (transformCursorLeft 200 [x|[{"i":"asdf", "p":100}]|])
  --     shouldBe 104 (transformCursorRight 100 [x|[{"i":"asdf", "p":100}]|])
  --     shouldBe 100 (transformCursorLeft 100 [x|[{"i":"asdf", "p":100}]|])
  --   it "works vs delete" $ do
  --     shouldBe 0 (transformCursorRight 0 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 0 (transformCursorLeft 0 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 0 (transformCursor 0 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 196 (transformCursor 200 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 100 (transformCursor 100 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 100 (transformCursor 102 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 100 (transformCursor 104 [x|[{"d":"asdf", "p":100}]|])
  --     shouldBe 101 (transformCursor 105 [x|[{"d":"asdf", "p":100}]|])

  -- describe "normalize" $ do
  --   it "is sane" $ do
  --     let testUnchanged = \op -> shouldBe op (normalize op)
  --     testUnchanged [x|[]|]
  --     testUnchanged [x|[{"i":"asdf", "p":100}]|]
  --     testUnchanged [x|[{"i":"asdf", "p":100}, {"d":"fdsa", "p":123}]|]

  --   it "adds missing "p":0" $ do
  --     shouldBe [x|[{"i":"abc", "p":0}]|] (normalize [x|[{"i":"abc"}]|])
  --     shouldBe [x|[{"d":"abc", "p":0}]|] (normalize [x|[{"d":"abc"}]|])
  --     shouldBe [x|[{"i":"abc", "p":0}, {"d":"abc", "p":0}]|] (normalize [x|[{"i":"abc"}, {"d":"abc"}]|])

  --   it "converts op to an array" $ do
  --     shouldBe [x|[{"i":"abc", "p":0}]|] (normalize [x|[{"i":"abc", "p":0}]|])
  --     shouldBe [x|[{"d":"abc", "p":0}]|] (normalize [x|[{"d":"abc", "p":0}]|])

  --   it "works with a really simple op" $ do
  --     shouldBe [x|[{"i":"abc", "p":0}]|] (normalize [x|[{"i":"abc"}]|])

  --   it "compress inserts" $ do
  --     shouldBe [x|[{"i":"xyzabc", "p":10}]|] (normalize [x|[{"i":"abc", "p":10}]|] [x|[{"i":"xyz", "p":10}]|])
  --     shouldBe [x|[{"i":"axyzbc", "p":10}]|] (normalize [x|[{"i":"abc", "p":10}]|] [x|[{"i":"xyz", "p":11}]|])
  --     shouldBe [x|[{"i":"abcxyz", "p":10}]|] (normalize [x|[{"i":"abc", "p":10}]|] [x|[{"i":"xyz", "p":13}]|])

  --   it "doesnt compress separate inserts" $ do
  --     let t = \op -> shouldBe op (normalize op)
  --     t [x|[{"i":"abc", "p":10}, {"i":"xyz", "p":9}]|]
  --     t [x|[{"i":"abc", "p":10}, {"i":"xyz", "p":14}]|]

  --   it "compress deletes" $ do
  --     shouldBe [x|[{"d":"xyabc", "p":8}]|] (normalize [x|[{"d":"abc", "p":10}, {"d":"xy", "p":8}]|])
  --     shouldBe [x|[{"d":"xabcy", "p":9}]|] (normalize [x|[{"d":"abc", "p":10}, {"d":"xy", "p":9}]|])
  --     shouldBe [x|[{"d":"abcxy", "p":10}]|] (normalize [x|[{"d":"abc", "p":10}, {"d":"xy", "p":10}]|])

  --   it "doesnt compress separate deletes" $ do
  --     let t = \op -> shouldBe op (normalize op)
  --     t [x|[{"d":"abc", "p":10}, {"d":"xyz", "p":6}]|]
  --     t [x|[{"d":"abc", "p":10}, {"d":"xyz", "p":11}]|]


main :: IO ()
main = (testSpec "Text0 specs" specs) >>= defaultMain
