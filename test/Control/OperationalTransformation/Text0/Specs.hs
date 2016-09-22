{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.Text0.Specs where

import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.Text0
import Data.Aeson as A
import Data.String.Interpolate.IsString
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

-- These tests are taken directly from
-- https://github.com/ottypes/json0/blob/master/test/json0.coffee

compose :: A.Value -> A.Value -> A.Value
compose val1 val2 = case C.compose op1 op2 of
  Left err -> error err
  Right x -> toJSON x
  where
    Success (op1 :: Text0Operation) = fromJSON val1
    Success (op2 :: Text0Operation) = fromJSON val2

transform :: A.Value -> A.Value -> (A.Value, A.Value)
transform val1 val2 = (toJSON op1', toJSON op2')
  where
    op1 :: Text0Operation = case fromJSON val1 of
      Success op1 -> op1
      Error err -> error [i|Failed to decode val1: #{val1} (#{err})|]
    op2 :: Text0Operation = case fromJSON val2 of
      Success op2 -> op2
      Error err -> error [i|Failed to decode val2: #{val2} (#{err})|]

    (op1', op2') = case C.transform op1 op2 of
      Left err -> error err
      Right x -> x

-- for ghci
d :: A.Value -> Text0Operation
d jsonValue = op
  where
    Success op = fromJSON jsonValue

apply :: A.Value -> A.Value -> A.Value
apply input opval = case fromJSON opval of
  Error err -> error err
  Success (op :: Text0Operation) -> case C.apply op input of
    Left err -> error err
    Right x -> x


-- Just for REPL testing
transform' :: A.Value -> A.Value -> (Text0Operation, Text0Operation)
transform' val1 val2 = (op1', op2')
  where
    Success (op1 :: Text0Operation) = fromJSON val1
    Success (op2 :: Text0Operation) = fromJSON val2
    Right (op1', op2') = C.transform op1 op2

-- TODO: these might be backwards, not sure yet
transformLeft :: A.Value -> A.Value -> A.Value
transformLeft a b = a'
  where (a', _) = transform a b

transformRight :: A.Value -> A.Value -> A.Value
transformRight a b = a'
  where (_, a') = transform b a

specs :: SpecWith ()
specs = do
describe 'text0', ->
  describe 'compose', ->
    -- Compose is actually pretty easy
    it "is sane" $ do
      shouldBe (compose [] []) []
      shouldBe (compose [{i:"x", p:0}] []) [{i:"x", p:0}]
      shouldBe (compose [] [{i:"x", p:0}]) [{i:"x", p:0}]
      shouldBe (compose [{i:"y", p:100}] [{i:"x", p:0}]) [{i:"y", p:100}, {i:"x", p:0}]

  describe "transform", ->
    it "is sane" $ do
      shouldBe [] (transformLeft [] [])
      shouldBe [] (transformRight [] [])

      shouldBe [{i:"y", p:100}, {i:"x", p:0}] (transformLeft [{i:"y", p:100}, {i:"x", p:0}] [])
      shouldBe [] (transformRight [], [{i:"y", p:100}, {i:"x", p:0}])

    it "inserts" $ do
      shouldBe [[{i:"x", p:10}], [{i:"a", p:1}]] (transform [{i:"x", p:9}] [{i:"a", p:1}])
      shouldBe [[{i:"x", p:10}], [{i:"a", p:11}]] (transform [{i:"x", p:10}] [{i:"a", p:10}])

      shouldBe [[{i:"x", p:10}], [{d:"a", p:9}]] (transform [{i:"x", p:11}] [{d:"a", p:9}])
      shouldBe [[{i:"x", p:10}], [{d:"a", p:10}]] (transform [{i:"x", p:11}] [{d:"a", p:10}])
      shouldBe [[{i:"x", p:11}], [{d:"a", p:12}]] (transform [{i:"x", p:11}] [{d:"a", p:11}])

      shouldBe [{i:"x", p:10}] (transformLeft [{i:"x", p:10}] [{d:"a", p:11}])
      shouldBe [{i:"x", p:10}] (transformLeft [{i:"x", p:10}] [{d:"a", p:10}])
      shouldBe [{i:"x", p:10}] (transformRight [{i:"x", p:10}] [{d:"a", p:10}])

    it "deletes" $ do
      shouldBe [[{d:"abc", p:8}], [{d:"xy", p:4}]] (transform [{d:"abc", p:10}] [{d:"xy", p:4}])
      shouldBe [[{d:"ac", p:10}], []] (transform [{d:"abc", p:10}] [{d:"b", p:11}])
      shouldBe [[], [{d:"ac", p:10}]] (transform [{d:"b", p:11}] [{d:"abc", p:10}])
      shouldBe [[{d:"a", p:10}], []] (transform [{d:"abc", p:10}] [{d:"bc", p:11}])
      shouldBe [[{d:"c", p:10}], []] (transform [{d:"abc", p:10}] [{d:"ab", p:10}])
      shouldBe [[{d:"a", p:10}], [{d:"d", p:10}]] (transform [{d:"abc", p:10}] [{d:"bcd", p:11}])
      shouldBe [[{d:"d", p:10}], [{d:"a", p:10}]] (transform [{d:"bcd", p:11}] [{d:"abc", p:10}])
      shouldBe [[{d:"abc", p:10}], [{d:"xy", p:10}]] (transform [{d:"abc", p:10}] [{d:"xy", p:13}])

  -- describe "transformCursor" $ do
  --   it "is sane" $ do
  --     assert.strictEqual 0, text0.transformCursorRight 0, []
  --     assert.strictEqual 0, text0.transformCursorLeft 0, []
  --     assert.strictEqual 100, text0.transformCursor 100, []

  --   it "works vs insert" $ do
  --     assert.strictEqual 0, text0.transformCursorRight 0, [{i:"asdf", p:100}]
  --     assert.strictEqual 0, text0.transformCursorLeft 0, [{i:"asdf", p:100}]

  --     assert.strictEqual 204, text0.transformCursorRight 200, [{i:"asdf", p:100}]
  --     assert.strictEqual 204, text0.transformCursorLeft 200, [{i:"asdf", p:100}]

  --     assert.strictEqual 104, text0.transformCursorRight 100, [{i:"asdf", p:100}]
  --     assert.strictEqual 100, text0.transformCursorLeft 100, [{i:"asdf", p:100}]

  --   it "works vs delete" $ do
  --     assert.strictEqual 0, text0.transformCursorRight 0, [{d:"asdf", p:100}]
  --     assert.strictEqual 0, text0.transformCursorLeft 0, [{d:"asdf", p:100}]
  --     assert.strictEqual 0, text0.transformCursor 0, [{d:"asdf", p:100}]

  --     assert.strictEqual 196, text0.transformCursor 200, [{d:"asdf", p:100}]

  --     assert.strictEqual 100, text0.transformCursor 100, [{d:"asdf", p:100}]
  --     assert.strictEqual 100, text0.transformCursor 102, [{d:"asdf", p:100}]
  --     assert.strictEqual 100, text0.transformCursor 104, [{d:"asdf", p:100}]
  --     assert.strictEqual 101, text0.transformCursor 105, [{d:"asdf", p:100}]

  -- describe "normalize" $ do
  --   it "is sane" $ do
  --     let testUnchanged = \op -> shouldBe op (normalize op)
  --     testUnchanged []
  --     testUnchanged [{i:"asdf", p:100}]
  --     testUnchanged [{i:"asdf", p:100}, {d:"fdsa", p:123}]

  --   it "adds missing p:0" $ do
  --     shouldBe [{i:"abc", p:0}], normalize [{i:"abc"}]
  --     shouldBe [{d:"abc", p:0}], normalize [{d:"abc"}]
  --     shouldBe [{i:"abc", p:0}, {d:"abc", p:0}], normalize [{i:"abc"}, {d:"abc"}]

  --   it "converts op to an array" $ do
  --     shouldBe [{i:"abc", p:0}], normalize {i:"abc", p:0}
  --     shouldBe [{d:"abc", p:0}], normalize {d:"abc", p:0}

  --   it "works with a really simple op" $ do
  --     shouldBe [{i:"abc", p:0}], normalize {i:"abc"}

  --   it "compress inserts" $ do
  --     shouldBe [{i:"xyzabc", p:10}], normalize [{i:"abc", p:10}, {i:"xyz", p:10}]
  --     shouldBe [{i:"axyzbc", p:10}], normalize [{i:"abc", p:10}, {i:"xyz", p:11}]
  --     shouldBe [{i:"abcxyz", p:10}], normalize [{i:"abc", p:10}, {i:"xyz", p:13}]

  --   it "doesnt compress separate inserts" $ do
  --     let t = \op -> shouldBe op (normalize op)

  --     t [{i:"abc", p:10}, {i:"xyz", p:9}]
  --     t [{i:"abc", p:10}, {i:"xyz", p:14}]

  --   it "compress deletes" $ do
  --     shouldBe [{d:"xyabc", p:8}] (normalize [{d:"abc", p:10}, {d:"xy", p:8}])
  --     shouldBe [{d:"xabcy", p:9}] (normalize [{d:"abc", p:10}, {d:"xy", p:9}])
  --     shouldBe [{d:"abcxy", p:10}] (normalize [{d:"abc", p:10}, {d:"xy", p:10}])

  --   it "doesnt compress separate deletes" $ do
  --     t = (op) -> shouldBe op (normalize op)

  --     t [{d:"abc", p:10}, {d:"xyz", p:6}]
  --     t [{d:"abc", p:10}, {d:"xyz", p:11}]


main :: IO ()
main = (testSpec "Text0 specs" specs) >>= defaultMain
