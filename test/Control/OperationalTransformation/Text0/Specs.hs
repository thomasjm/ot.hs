{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.Text0.Specs where

import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.Text0
import Data.Aeson as A
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

-- These tests are taken directly from
-- https://github.com/ottypes/json0/blob/master/test/json0.coffee

compose :: A.Value -> A.Value -> A.Value
compose val1 val2 = case C.compose (T0 op1) (T0 op2) of
  Left err -> error err
  Right (T0 x) -> toJSON x
  where
    Success (op1 :: [SingleText0Operation]) = fromJSON val1
    Success (op2 :: [SingleText0Operation]) = fromJSON val2

normalize = undefined

transformCursor = undefined
transformCursorLeft = undefined
transformCursorRight = undefined

transform :: A.Value -> A.Value -> (A.Value, A.Value)
transform val1 val2 = (toJSON op1', toJSON op2')
  where
    op1 :: [SingleText0Operation] = case fromJSON val1 of
      Success op1 -> op1
      Error err -> error [i|Failed to decode val1: #{val1} (#{err})|]
    op2 :: [SingleText0Operation] = case fromJSON val2 of
      Success op2 -> op2
      Error err -> error [i|Failed to decode val2: #{val2} (#{err})|]

    (T0 op1', T0 op2') = case C.transform (T0 op1) (T0 op2) of
      Left err -> error err
      Right x -> x

-- for ghci
d :: A.Value -> Text0Operation
d jsonValue = op
  where
    Success op = T0 <$> fromJSON jsonValue

apply :: T.Text -> A.Value -> T.Text
apply input opval = case fromJSON opval of
  Error err -> error err
  Success (op :: SingleText0Operation) -> case C.apply (T0 [op]) input of
    Left err -> error err
    Right x -> x


-- Just for REPL testing
transform' :: A.Value -> A.Value -> (Text0Operation, Text0Operation)
transform' val1 val2 = (op1', op2')
  where
    Success (op1 :: [SingleText0Operation]) = fromJSON val1
    Success (op2 :: [SingleText0Operation]) = fromJSON val2
    Right (op1', op2') = C.transform (T0 op1) (T0 op2)

-- TODO: these might be backwards, not sure yet
transformLeft :: A.Value -> A.Value -> A.Value
transformLeft a b = a'
  where (a', _) = transform a b

transformRight :: A.Value -> A.Value -> A.Value
transformRight a b = a'
  where (_, a') = transform b a

specs :: SpecWith ()
specs = do
  describe "compose" $ do
    -- Compose is actually pretty easy
    it "is sane" $ do
      shouldBe (compose [j|[]|] [j|[]|]) [j|[]|]
      shouldBe (compose [j|[{i:"x", p:0}]|] [j|[]|]) [j|[{i:"x", p:0}]|]
      shouldBe (compose [j|[]|] [j|[{i:"x", p:0}]|]) [j|[{i:"x", p:0}]|]
      shouldBe (compose [j|[{i:"y", p:100}]|] [j|[{i:"x", p:0}]|]) [j|[{i:"y", p:100}, {i:"x", p:0}]|]

  describe "transform" $ do
    it "is sane" $ do
      shouldBe [j|[]|] (transformLeft [j|[]|] [j|[]|])
      shouldBe [j|[]|] (transformRight [j|[]|] [j|[]|])

      shouldBe [j|[{i:"y", p:100}, {i:"x", p:0}]|] (transformLeft [j|[{i:"y", p:100}, {i:"x", p:0}]|] [j|[]|])
      shouldBe [j|[]|] (transformRight [j|[]|] [j|[{i:"y", p:100}, {i:"x", p:0}]|])

    it "inserts" $ do
      shouldBe ([j|[{i:"x", p:10}]|], [j|[{i:"a", p:1}]|]) (transform [j|[{i:"x", p:9}]|] [j|[{i:"a", p:1}]|])
      shouldBe ([j|[{i:"x", p:10}]|], [j|[{i:"a", p:11}]|]) (transform [j|[{i:"x", p:10}]|] [j|[{i:"a", p:10}]|])

      shouldBe ([j|[{i:"x", p:10}]|], [j|[{d:"a", p:9}]|]) (transform [j|[{i:"x", p:11}]|] [j|[{d:"a", p:9}]|])
      shouldBe ([j|[{i:"x", p:10}]|], [j|[{d:"a", p:10}]|]) (transform [j|[{i:"x", p:11}]|] [j|[{d:"a", p:10}]|])
      shouldBe ([j|[{i:"x", p:11}]|], [j|[{d:"a", p:12}]|]) (transform [j|[{i:"x", p:11}]|] [j|[{d:"a", p:11}]|])

      shouldBe [j|[{i:"x", p:10}]|] (transformLeft [j|[{i:"x", p:10}]|] [j|[{d:"a", p:11}]|])
      shouldBe [j|[{i:"x", p:10}]|] (transformLeft [j|[{i:"x", p:10}]|] [j|[{d:"a", p:10}]|])
      shouldBe [j|[{i:"x", p:10}]|] (transformRight [j|[{i:"x", p:10}]|] [j|[{d:"a", p:10}]|])

    it "deletes" $ do
      shouldBe ([j|[{d:"abc", p:8}]|], [j|[{d:"xy", p:4}]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"xy", p:4}]|])
      shouldBe ([j|[{d:"ac", p:10}]|], [j|[]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"b", p:11}]|])
      shouldBe ([j|[]|], [j|[{d:"ac", p:10}]|]) (transform [j|[{d:"b", p:11}]|] [j|[{d:"abc", p:10}]|])
      shouldBe ([j|[{d:"a", p:10}]|], [j|[]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"bc", p:11}]|])
      shouldBe ([j|[{d:"c", p:10}]|], [j|[]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"ab", p:10}]|])
      shouldBe ([j|[{d:"a", p:10}]|], [j|[{d:"d", p:10}]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"bcd", p:11}]|])
      shouldBe ([j|[{d:"d", p:10}]|], [j|[{d:"a", p:10}]|]) (transform [j|[{d:"bcd", p:11}]|] [j|[{d:"abc", p:10}]|])
      shouldBe ([j|[{d:"abc", p:10}]|], [j|[{d:"xy", p:10}]|]) (transform [j|[{d:"abc", p:10}]|] [j|[{d:"xy", p:13}]|])

  describe "transformCursor" $ do
    it "is sane" $ do
      shouldBe 0 (transformCursorRight 0 [j|[]|])
      shouldBe 0 (transformCursorLeft 0 [j|[]|])
      shouldBe 100 (transformCursor 100 [j|[]|])

    it "works vs insert" $ do
      shouldBe 0 (transformCursorRight 0 [j|{i:"asdf", p:100}|])
      shouldBe 0 (transformCursorLeft 0 [j|{i:"asdf", p:100}|])

      shouldBe 204 (transformCursorRight 200 [j|[{i:"asdf", p:100}]|])
      shouldBe 204 (transformCursorLeft 200 [j|[{i:"asdf", p:100}]|])
      shouldBe 104 (transformCursorRight 100 [j|[{i:"asdf", p:100}]|])
      shouldBe 100 (transformCursorLeft 100 [j|[{i:"asdf", p:100}]|])
    it "works vs delete" $ do
      shouldBe 0 (transformCursorRight 0 [j|[{d:"asdf", p:100}]|])
      shouldBe 0 (transformCursorLeft 0 [j|[{d:"asdf", p:100}]|])
      shouldBe 0 (transformCursor 0 [j|[{d:"asdf", p:100}]|])
      shouldBe 196 (transformCursor 200 [j|[{d:"asdf", p:100}]|])
      shouldBe 100 (transformCursor 100 [j|[{d:"asdf", p:100}]|])
      shouldBe 100 (transformCursor 102 [j|[{d:"asdf", p:100}]|])
      shouldBe 100 (transformCursor 104 [j|[{d:"asdf", p:100}]|])
      shouldBe 101 (transformCursor 105 [j|[{d:"asdf", p:100}]|])

  describe "normalize" $ do
    it "is sane" $ do
      let testUnchanged = \op -> shouldBe op (normalize op)
      testUnchanged [j|[]|]
      testUnchanged [j|[{i:"asdf", p:100}]|]
      testUnchanged [j|[{i:"asdf", p:100}, {d:"fdsa", p:123}]|]

    it "adds missing p:0" $ do
      shouldBe [j|[{i:"abc", p:0}]|] (normalize [j|[{i:"abc"}]|])
      shouldBe [j|[{d:"abc", p:0}]|] (normalize [j|[{d:"abc"}]|])
      shouldBe [j|[{i:"abc", p:0}, {d:"abc", p:0}]|] (normalize [j|[{i:"abc"}, {d:"abc"}]|])

    it "converts op to an array" $ do
      shouldBe [j|[{i:"abc", p:0}]|] (normalize [j|[{i:"abc", p:0}]|])
      shouldBe [j|[{d:"abc", p:0}]|] (normalize [j|[{d:"abc", p:0}]|])

    it "works with a really simple op" $ do
      shouldBe [j|[{i:"abc", p:0}]|] (normalize [j|[{i:"abc"}]|])

    it "compress inserts" $ do
      shouldBe [j|[{i:"xyzabc", p:10}]|] (normalize [j|[{i:"abc", p:10}]|] [j|[{i:"xyz", p:10}]|])
      shouldBe [j|[{i:"axyzbc", p:10}]|] (normalize [j|[{i:"abc", p:10}]|] [j|[{i:"xyz", p:11}]|])
      shouldBe [j|[{i:"abcxyz", p:10}]|] (normalize [j|[{i:"abc", p:10}]|] [j|[{i:"xyz", p:13}]|])

    it "doesnt compress separate inserts" $ do
      let t = \op -> shouldBe op (normalize op)
      t [j|[{i:"abc", p:10}, {i:"xyz", p:9}]|]
      t [j|[{i:"abc", p:10}, {i:"xyz", p:14}]|]

    it "compress deletes" $ do
      shouldBe [j|[{d:"xyabc", p:8}]|] (normalize [j|[{d:"abc", p:10}, {d:"xy", p:8}]|])
      shouldBe [j|[{d:"xabcy", p:9}]|] (normalize [j|[{d:"abc", p:10}, {d:"xy", p:9}]|])
      shouldBe [j|[{d:"abcxy", p:10}]|] (normalize [j|[{d:"abc", p:10}, {d:"xy", p:10}]|])

    it "doesnt compress separate deletes" $ do
      let t = \op -> shouldBe op (normalize op)
      t [j|[{d:"abc", p:10}, {d:"xyz", p:6}]|]
      t [j|[{d:"abc", p:10}, {d:"xyz", p:11}]|]


main :: IO ()
main = (testSpec "Text0 specs" specs) >>= defaultMain
