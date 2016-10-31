{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.JSON.Specs where

import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON
import Control.OperationalTransformation.JSON.QuasiQuote
import Control.OperationalTransformation.JSON.Types
import Data.Aeson as A
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

-- These tests are taken directly from
-- https://github.com/ottypes/json0/blob/master/test/json0.coffee

apply :: A.Value -> JSONOperation -> A.Value
apply input op = case C.apply op input of
  Left err -> error err
  Right x -> x

compose :: JSONOperation -> JSONOperation -> JSONOperation
compose op1 op2 = case C.compose op1 op2 of
  Left err -> error err
  Right operation -> operation

transform :: JSONOperation -> JSONOperation -> (JSONOperation, JSONOperation)
transform op1 op2 = case C.transform op1 op2 of
  Left err -> error err
  Right x -> x

transformLeft :: JSONOperation -> JSONOperation -> JSONOperation
transformLeft a b = a'
  where (a', _) = transform a b

transformRight :: JSONOperation -> JSONOperation -> JSONOperation
transformRight a b = b'
  where (_, b') = transform a b

shouldBe' :: (Eq a, Show a) => a -> a -> Expectation
shouldBe' = flip shouldBe

specs :: SpecWith ()
specs = do
  describe "sanity" $ do
    describe "compose()" $ do
     it "od,oi --> od+oi" $ do
       shouldBe' [l|[{"p":["foo"], "od":1, "oi":2}]|] (compose [s|{"p":["foo"], "od":1}|] [s|{"p":["foo"], "oi":2}|])
       shouldBe' [l|[{"p":["foo"], "od":1}, {"p":["bar"], "oi":2}]|] (compose [s|{"p":["foo"], "od":1}|] [s|{"p":["bar"], "oi":2}|])

     it "merges od+oi, od+oi -> od+oi" $ do
       shouldBe' [l|[{"p":["foo"], "od":1, "oi":2}]|] (compose [s|{"p":["foo"], "od":1, "oi":3}|] [s|{"p":["foo"], "od":3, "oi":2}|])

  describe "transform() stuff" $ do
   it "returns sane values" $ do
     let t = \op1 op2 -> op1 `shouldBe'` transformLeft op1 op2

     t [s|{}|] [s|{}|]
     t [s|{"p":["foo"], "oi":1}|] [s|{}|]
     t [s|{"p":["foo"], "oi":1}|] [s|{"p":["bar"], "oi":2}|]
     t [s|{"p":["foo"], "oi":1}|] [s|{"p":["bar"], "oi":2}|]

  describe "number" $ do
    it "Adds a number" $ do
      shouldBe' [v|3|] (apply [v|1|] [s|{"p":[], "na":2}|])
      shouldBe' [v|[3]|] (apply [v|[1]|] [s|{"p":[0], "na":2}|])

    it "transforms adds" $ do
      shouldBe' [s|{"p":[], "na": 0}|] (transformRight [s|{"p":[], "na": 0}|] [s|{"p":[], "na": 0}|])

    it "compresses two adds together in compose" $ do
      shouldBe' [l|[{"p":["a", "b"], "na":3}]|] (compose [s|{"p":["a", "b"], "na":1}|] [s|{"p":["a", "b"], "na":2}|])
      -- shouldBe' [s|{"p":["a"], "na":1}, {"p":["b"], "na":2}], type.compose [{"p":["a"], "na":1}], [{"p":["b"], "na":2}]

  -- # Strings should be handled internally by the text type. We"ll just do some basic sanity checks here.
  describe "string" $ do
    describe "apply()" $ it "works" $ do
      shouldBe' [v|"abc"|] (apply [v|"a"|] [s|{"p":[1], "si":"bc"}|])
      shouldBe' [v|"bc"|] (apply [v|"abc"|] [s|{"p":[0], "sd":"a"}|])
      shouldBe' [v|{x:"abc"}|] (apply [v|{x:"a"}|] [s|{"p":["x", 1], "si":"bc"}|])

  describe "transform()" $ do
    it "splits deletes" $ do
      shouldBe' [l|[{"p":[0], "sd":"a"}, {"p":[1], "sd":"b"}]|] (transformLeft [l|[{"p":[0], "sd":"ab"}]|] [l|[{"p":[1], "si":"x"}]|])

    it "cancels out other deletes" $ do
      shouldBe' [l|[]|] (transformLeft [s|{"p":["k", 5], "sd":"a"}|] [s|{"p":["k", 5], "sd":"a"}|])

    it "does not throw errors with blank inserts" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p": ["k", 5], "si":""}|] [s|{"p": ["k", 3], "si": "a"}|])

  describe "string subtype" $ do
    describe "apply()" $ do
      it "works" $ do
        shouldBe [v|"abc"|] (apply [v|"a"|] [s|{"p":[], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
        shouldBe [v|{x:"abc"}|] (apply [v|{x:"a"}|] [s|{"p":["x"], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
        shouldBe [v|"bc"|] (apply [v|"abc"|] [s|{"p":[], "t":"text0", "o":[{"p":0, "d":"a"}]}|])

    describe "transform()" $ do
      it "splits deletes" $ do
        let a = [s|{"p":[], "t":"text0", "o":[{"p":0, "d":"ab"}]}|]
        let b = [s|{"p":[], "t":"text0", "o":[{"p":1, "i":"x"}]}|]
        shouldBe' [s|{"p":[], "t":"text0", "o":[{"p":0, "d":"a"}, {"p":1, "d":"b"}]}|] (transformLeft a b)

      it "cancels out other deletes" $ do
        shouldBe' [s|{}|] (transformLeft [s|{"p":["k"], "t":"text0", "o":[{"p":5, "d":"a"}]}|] [s|{"p":["k"], "t":"text0", "o":[{"p":5, "d":"a"}]}|])

      it "does not throw errors with blank inserts" $ do
        shouldBe' [s|{}|] (transformLeft [s|{"p":["k"], "t":"text0", "o":[{"p":5, "i":""}]}|] [s|{"p":["k"], "t":"text0", "o":[{"p":3, "i":"a"}]}|])

  describe "list" $ do
    describe "apply" $ do
      it "inserts" $ do
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "c"]|] [s|{"p":[0], "li":"a"}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["a", "c"]|] [s|{"p":[1], "li":"b"}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["a", "b"]|] [s|{"p":[2], "li":"c"}|])

      it "deletes" $ do
        shouldBe' [v|["b", "c"]|] (apply [v|["a", "b", "c"]|] [s|{"p":[0], "ld":"a"}|])
        shouldBe' [v|["a", "c"]|] (apply [v|["a", "b", "c"]|] [s|{"p":[1], "ld":"b"}|])
        shouldBe' [v|["a", "b"]|] (apply [v|["a", "b", "c"]|] [s|{"p":[2], "ld":"c"}|])

      it "replaces" $ do
        shouldBe' [v|["a", "y", "b"]|] (apply [v|["a", "x", "b"]|] [s|{"p":[1], "ld":"x", "li":"y"}|])

      it "moves" $ do
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "a", "c"]|] [s|{"p":[1], "lm":0}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "a", "c"]|] [s|{"p":[0], "lm":1}|])

      it "null moves compose to nops" $ do
        shouldBe' [l|[]|] (compose [l|[]|] [l|[{"p":[3], "lm":3}]|])
        shouldBe' [l|[]|] (compose [l|[]|] [l|[{"p":[0,3], "lm":3}]|])
        shouldBe' [l|[]|] (compose [l|[]|] [l|[{"p":["x","y",0], "lm":0}]|])

  describe "transform()" $ do
    it "bumps paths when list elements are inserted or removed" $ do
      shouldBe' [s|{"p":[2, 200], "si":"hi"}|] (transformLeft [s|{"p":[1, 200], "si":"hi"}|] [s|{"p":[0], "li":"x"}|])
      shouldBe' [s|{"p":[1, 201], "si":"hi"}|] (transformRight [s|{"p":[0], "li":"x"}|] [s|{"p":[0, 201], "si":"hi"}|])
      shouldBe' [s|{"p":[0, 202], "si":"hi"}|] (transformLeft [s|{"p":[0, 202], "si":"hi"}|] [s|{"p":[1], "li":"x"}|])
      shouldBe' [s|{"p":[2], "t":"text0", "o":[{"p":200, "i":"hi"}]}|] (transformLeft [s|{"p":[1], "t":"text0", "o":[{"p":200, "i":"hi"}]}|] [s|{"p":[0], "li":"x"}|])
      shouldBe' [s|{"p":[1], "t":"text0", "o":[{"p":201, "i":"hi"}]}|] (transformRight [s|{"p":[0], "li":"x"}|] [s|{"p":[0], "t":"text0", "o":[{"p":201, "i":"hi"}]}|])
      shouldBe' [s|{"p":[0], "t":"text0", "o":[{"p":202, "i":"hi"}]}|] (transformLeft [s|{"p":[0], "t":"text0", "o":[{"p":202, "i":"hi"}]}|] [s|{"p":[1], "li":"x"}|])

      shouldBe' [s|{"p":[0, 203], "si":"hi"}|] (transformLeft [s|{"p":[1, 203], "si":"hi"}|] [s|{"p":[0], "ld":"x"}|])
      shouldBe' [s|{"p":[0, 204], "si":"hi"}|] (transformLeft [s|{"p":[0, 204], "si":"hi"}|] [s|{"p":[1], "ld":"x"}|])

      -- TODO: seems to be an invalid list insert
      -- shouldBe' [s|{"p":["x",3], "si": "hi"}|] (transformLeft [s|{"p":["x",3], "si":"hi"}|] [s|{"p":["x",0,"x"], "li":0}|])

      shouldBe' [s|{"p":["x",3,2], "si": "hi"}|] (transformLeft [s|{"p":["x",3,2], "si":"hi"}|] [s|{"p":["x",5], "li":0}|])
      shouldBe' [s|{"p":["x",4,2], "si": "hi"}|] (transformLeft [s|{"p":["x",3,2], "si":"hi"}|] [s|{"p":["x",0], "li":0}|])

      shouldBe' [s|{"p":[1], "ld":2}|] (transformLeft [s|{"p":[0], "ld":2}|] [s|{"p":[0], "li":1}|])
      shouldBe' [s|{"p":[1], "ld":2}|] (transformRight [s|{"p":[0], "li":1}|] [s|{"p":[0], "ld":2}|])

      shouldBe' [s|{"p":[0], "t":"text0", "o":[{"p":203, "i":"hi"}]}|] (transformLeft [s|{"p":[1], "t":"text0", "o":[{"p":203, "i":"hi"}]}|] [s|{"p":[0], "ld":"x"}|])
      shouldBe' [s|{"p":[0], "t":"text0", "o":[{"p":204, "i":"hi"}]}|] (transformLeft [s|{"p":[0], "t":"text0", "o":[{"p":204, "i":"hi"}]}|] [s|{"p":[1], "ld":"x"}|])

      -- TODO: this looks like an invalid list insert because the list element is not a pos
      -- shouldBe' [s|{"p":["x"], "t":"text0", "o":[{"p":3, "i":"hi"}]}|] (transformLeft [s|{"p":["x"], "t":"text0", "o":[{"p":3, "i":"hi"}]}|] [s|{"p":["x",0,"x"], "li":0}|])


    it "converts ops on deleted elements to noops" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":[1, 0], "si":"hi"}|] [s|{"p":[1], "ld":"x"}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":[1], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [s|{"p":[1], "ld":"x"}|])
      shouldBe' [s|{"p":[0], "li":"x"}|] (transformLeft [s|{"p":[0], "li":"x"}|] [s|{"p":[0], "ld":"y"}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":[0],"na":-3}|] [s|{"p":[0], "ld":48}|])

    it "converts ops on replaced elements to noops" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":[1, 0], "si":"hi"}|] [s|{"p":[1], "ld":"x", "li":"y"}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":[1], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [s|{"p":[1], "ld":"x", "li":"y"}|])
      shouldBe' [s|{"p":[0], "li":"hi"}|] (transformLeft [s|{"p":[0], "li":"hi"}|] [s|{"p":[0], "ld":"x", "li":"y"}|])

    it "changes deleted data to reflect edits" $ do
      shouldBe' [s|{"p":[1], "ld":"abc"}|] (transformLeft [s|{"p":[1], "ld":"a"}|] [s|{"p":[1, 1], "si":"bc"}|])
      shouldBe' [s|{"p":[1], "ld":"abc"}|] (transformLeft [s|{"p":[1], "ld":"a"}|] [s|{"p":[1], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])

    it "Puts the left op first if two inserts are simultaneous" $ do
      shouldBe' [s|{"p":[1], "li":"a"}|] (transformLeft [s|{"p":[1], "li":"a"}|] [s|{"p":[1], "li":"b"}|])
      shouldBe' [s|{"p":[2], "li":"b"}|] (transformRight [s|{"p":[1], "li":"a"}|] [s|{"p":[1], "li":"b"}|])

    it "converts an attempt to re-delete a list element into a no-op" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":[1], "ld":"x"}|] [s|{"p":[1], "ld":"x"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":[1], "ld":"x"}|] [s|{"p":[1], "ld":"x"}|])


    describe "compose()" $ do
      it "composes insert then delete into a no-op" $ do
        shouldBe' [l|[{}]|] (compose [s|{"p":[1], "li":"abc"}|] [s|{"p":[1], "ld":"abc"}|])
        shouldBe' [l|[{"p":[0], "ld":"abc"}]|] (compose [s|{"p":[0], "ld":"abc", "li":null}|] [s|{"p":[0], "ld":null}|])
        shouldBe' [s|{"p":[1], "ld":null, "li":"x"}|] (transformRight [s|{"p":[0], "li":"The"}|] [s|{"p":[0], "ld":null, "li":"x"}|])

      it "composes together adjacent string ops" $ do
        shouldBe' [l|[{"p":[100], "si":"hi"}]|] (compose [s|{"p":[100], "si":"h"}|] [s|{"p":[101], "si":"i"}|])
        shouldBe' [l|[{"p":[], "t":"text0", "o":[{"p":100, "i":"hi"}]}]|] (compose [s|{"p":[], "t":"text0", "o":[{"p":100, "i":"h"}]}|] [s|{"p":[], "t":"text0", "o":[{"p":101, "i":"i"}]}|])

    it "moves ops on a moved element with the element" $ do
      shouldBe' [s|{"p":[10], "ld":"x"}|] (transformLeft [s|{"p":[4], "ld":"x"}|] [s|{"p":[4], "lm":10}|])
      shouldBe' [s|{"p":[10, 1], "si":"a"}|] (transformLeft [s|{"p":[4, 1], "si":"a"}|] [s|{"p":[4], "lm":10}|])
      shouldBe' [s|{"p":[10], "t":"text0", "o":[{"p":1, "i":"a"}]}|] (transformLeft [s|{"p":[4], "t":"text0", "o":[{"p":1, "i":"a"}]}|] [s|{"p":[4], "lm":10}|])
      shouldBe' [s|{"p":[10, 1], "li":"a"}|] (transformLeft [s|{"p":[4, 1], "li":"a"}|] [s|{"p":[4], "lm":10}|])
      shouldBe' [s|{"p":[10, 1], "ld":"b", "li":"a"}|] (transformLeft [s|{"p":[4, 1], "ld":"b", "li":"a"}|] [s|{"p":[4], "lm":10}|])

      shouldBe' [s|{"p":[0], "li":null}|] (transformLeft [s|{"p":[0], "li":null}|] [s|{"p":[0], "lm":1}|])
      -- -- [_,_,_,_,5,6,7,_]
      -- -- c: [_,_,_,_,5,"x",6,7,_]   p:5 "li":"x"
      -- -- s: [_,6,_,_,_,5,7,_]       p:5 "lm":1
      -- -- correct: [_,6,_,_,_,5,"x",7,_]
      shouldBe' [s|{"p":[6], "li":"x"}|] (transformLeft [s|{"p":[5], "li":"x"}|] [s|{"p":[5], "lm":1}|])
      -- -- [_,_,_,_,5,6,7,_]
      -- -- c: [_,_,_,_,5,6,7,_]  p:5 "ld":6
      -- -- s: [_,6,_,_,_,5,7,_]  p:5 "lm":1
      -- -- correct: [_,_,_,_,5,7,_]
      shouldBe' [s|{"p":[1], "ld":6}|] (transformLeft [s|{"p":[5], "ld":6}|] [s|{"p":[5], "lm":1}|])
      shouldBe' [s|{"p":[0], "li":{}}|] (transformRight [s|{"p":[0], "lm":0}|] [s|{"p":[0], "li":{}}|])
      shouldBe' [s|{"p":[0], "li":[]}|] (transformLeft [s|{"p":[0], "li":[]}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[2], "li":"x"}|] (transformLeft [s|{"p":[2], "li":"x"}|] [s|{"p":[0], "lm":1}|])

    it "moves target index on ld/li" $ do
      shouldBe' [s|{"p":[0], "lm":1}|] (transformLeft [s|{"p":[0], "lm": 2}|] [s|{"p":[1], "ld":"x"}|])
      shouldBe' [s|{"p":[1], "lm":3}|] (transformLeft [s|{"p":[2], "lm": 4}|] [s|{"p":[1], "ld":"x"}|])
      shouldBe' [s|{"p":[0], "lm":3}|] (transformLeft [s|{"p":[0], "lm": 2}|] [s|{"p":[1], "li":"x"}|])
      shouldBe' [s|{"p":[3], "lm":5}|] (transformLeft [s|{"p":[2], "lm": 4}|] [s|{"p":[1], "li":"x"}|])
      shouldBe' [s|{"p":[1], "lm":1}|] (transformLeft [s|{"p":[0], "lm": 0}|] [s|{"p":[0], "li":28}|])

    it "tiebreaks lm vs. ld/li" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":[0], "lm": 2}|] [s|{"p":[0], "ld":"x"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":[0], "ld":"x"}|] [s|{"p":[0], "lm": 2}|])
      shouldBe' [s|{"p":[1], "lm":3}|] (transformLeft [s|{"p":[0], "lm": 2}|] [s|{"p":[0], "li":"x"}|])
      shouldBe' [s|{"p":[1], "lm":3}|] (transformRight [s|{"p":[0], "li":"x"}|] [s|{"p":[0], "lm": 2}|])

    it "replacement vs. deletion" $ do
      shouldBe' [s|{"p":[0], "li":"y"}|] (transformRight [s|{"p":[0], "ld":"x"}|] [s|{"p":[0], "ld":"x", "li":"y"}|])

    it "replacement vs. insertion" $ do
      shouldBe' [s|{"p":[1], "ld":{}, "li":"brillig"}|] (transformLeft [s|{"p":[0], "ld":{}, "li":"brillig"}|] [s|{"p":[0], "li":36}|])

    it "replacement vs. replacement" $ do
      shouldBe' [s|{}|]                 (transformRight [s|{"p":[0], "ld":null, "li":0}|] [s|{"p":[0], "ld":null, "li":[]}|])
      shouldBe' [s|{"p":[0], "ld":[], "li":0}|] (transformLeft  [s|{"p":[0], "ld":null, "li":0}|] [s|{"p":[0], "ld":null, "li":[]}|])

    it "composes replace with delete of replaced element results in insert" $ do
      shouldBe' [l|[{"p":[2], "ld":[]}]|] (compose [s|{"p":[2], "ld":[], "li":null}|] [s|{"p":[2], "ld":null}|])

    it "lm vs lm" $ do
      shouldBe' [s|{"p":[0], "lm":2}|] (transformLeft [s|{"p":[0], "lm":2}|] [s|{"p":[2], "lm":1}|])

      shouldBe' [s|{"p":[4], "lm":4}|] (transformLeft [s|{"p":[3], "lm":3}|] [s|{"p":[5], "lm":0}|])

      shouldBe' [s|{"p":[2], "lm":0}|] (transformLeft [s|{"p":[2], "lm":0}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[2], "lm":1}|] (transformRight [s|{"p":[1], "lm":0}|] [s|{"p":[2], "lm":0}|])

      shouldBe' [s|{"p":[3], "lm":1}|] (transformRight [s|{"p":[5], "lm":0}|] [s|{"p":[2], "lm":0}|])
      shouldBe' [s|{"p":[3], "lm":0}|] (transformLeft [s|{"p":[2], "lm":0}|] [s|{"p":[5], "lm":0}|])

      -- TODO: why is this test duplicated. One side should be right I guess?
      shouldBe' [s|{"p":[0], "lm":5}|] (transformLeft [s|{"p":[2], "lm":5}|] [s|{"p":[2], "lm":0}|])
      shouldBe' [s|{"p":[0], "lm":5}|] (transformLeft [s|{"p":[2], "lm":5}|] [s|{"p":[2], "lm":0}|])

      shouldBe' [s|{"p":[0], "lm":0}|] (transformRight [s|{"p":[0], "lm":5}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[0], "lm":0}|] (transformRight [s|{"p":[0], "lm":1}|] [s|{"p":[1], "lm":0}|])

      shouldBe' [s|{"p":[1], "lm":1}|] (transformLeft [s|{"p":[0], "lm":1}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[1], "lm":2}|] (transformRight [s|{"p":[5], "lm":0}|] [s|{"p":[0], "lm":1}|])

      shouldBe' [s|{"p":[3], "lm":2}|] (transformRight [s|{"p":[5], "lm":0}|] [s|{"p":[2], "lm":1}|])

      shouldBe' [s|{"p":[2], "lm":1}|] (transformLeft [s|{"p":[3], "lm":1}|] [s|{"p":[1], "lm":3}|])
      shouldBe' [s|{"p":[2], "lm":3}|] (transformLeft [s|{"p":[1], "lm":3}|] [s|{"p":[3], "lm":1}|])

      shouldBe' [s|{"p":[2], "lm":6}|] (transformLeft [s|{"p":[2], "lm":6}|] [s|{"p":[0], "lm":1}|])
      shouldBe' [s|{"p":[2], "lm":6}|] (transformRight [s|{"p":[0], "lm":1}|] [s|{"p":[2], "lm":6}|])

      shouldBe' [s|{"p":[2], "lm":6}|] (transformLeft [s|{"p":[2], "lm":6}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[2], "lm":6}|] (transformRight [s|{"p":[1], "lm":0}|] [s|{"p":[2], "lm":6}|])

      shouldBe' [s|{"p":[0], "lm":2}|] (transformLeft [s|{"p":[0], "lm":1}|] [s|{"p":[2], "lm":1}|])
      shouldBe' [s|{"p":[2], "lm":0}|] (transformRight [s|{"p":[0], "lm":1}|] [s|{"p":[2], "lm":1}|])

      shouldBe' [s|{"p":[1], "lm":1}|] (transformLeft [s|{"p":[0], "lm":0}|] [s|{"p":[1], "lm":0}|])

      shouldBe' [s|{"p":[0], "lm":0}|] (transformLeft [s|{"p":[0], "lm":1}|] [s|{"p":[1], "lm":3}|])

      shouldBe' [s|{"p":[3], "lm":1}|] (transformLeft [s|{"p":[2], "lm":1}|] [s|{"p":[3], "lm":2}|])
      shouldBe' [s|{"p":[3], "lm":3}|] (transformLeft [s|{"p":[3], "lm":2}|] [s|{"p":[2], "lm":1}|])

    it "changes indices correctly around a move" $ do
      shouldBe' [s|{"p":[1,0], "li":{}}|] (transformLeft [s|{"p":[0,0], "li":{}}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[0], "lm":0}|] (transformLeft [s|{"p":[1], "lm":0}|] [s|{"p":[0], "ld":{}}|])
      shouldBe' [s|{"p":[0], "lm":0}|] (transformLeft [s|{"p":[0], "lm":1}|] [s|{"p":[1], "ld":{}}|])
      shouldBe' [s|{"p":[5], "lm":0}|] (transformLeft [s|{"p":[6], "lm":0}|] [s|{"p":[2], "ld":{}}|])
      shouldBe' [s|{"p":[1], "lm":0}|] (transformLeft [s|{"p":[1], "lm":0}|] [s|{"p":[2], "ld":{}}|])
      shouldBe' [s|{"p":[1], "lm":1}|] (transformRight [s|{"p":[1], "ld":3}|] [s|{"p":[2], "lm":1}|])

      shouldBe' [s|{"p":[1], "ld":{}}|] (transformRight [s|{"p":[1], "lm":2}|] [s|{"p":[2], "ld":{}}|])
      shouldBe' [s|{"p":[2], "ld":{}}|] (transformLeft [s|{"p":[1], "ld":{}}|] [s|{"p":[2], "lm":1}|])

      shouldBe' [s|{"p":[0], "ld":{}}|] (transformRight [s|{"p":[0], "lm":1}|] [s|{"p":[1], "ld":{}}|])

      shouldBe' [s|{"p":[0], "ld":1, "li":2}|] (transformLeft [s|{"p":[1], "ld":1, "li":2}|] [s|{"p":[1], "lm":0}|])
      shouldBe' [s|{"p":[0], "ld":2, "li":3}|] (transformLeft [s|{"p":[1], "ld":2, "li":3}|] [s|{"p":[0], "lm":1}|])
      shouldBe' [s|{"p":[1], "ld":3, "li":4}|] (transformLeft [s|{"p":[0], "ld":3, "li":4}|] [s|{"p":[1], "lm":0}|])

    it "li vs lm" $ do
      let li = \(p :: Int) -> JSONOperation [ListInsert [] p [v|[]|]]
      let lm :: Int -> Int -> JSONOperation
          lm = \f t -> JSONOperation [ListMove [] f t]

      shouldBe' (li 0) (transformLeft (li 0) (lm 1 3))
      shouldBe' (li 1) (transformLeft (li 1) (lm 1 3))
      shouldBe' (li 1) (transformLeft (li 2) (lm 1 3))
      shouldBe' (li 2) (transformLeft (li 3) (lm 1 3))
      shouldBe' (li 4) (transformLeft (li 4) (lm 1 3))

      shouldBe' (lm 2 4) (transformRight (li 0) (lm 1 3))
      shouldBe' (lm 2 4) (transformRight (li 1) (lm 1 3))
      shouldBe' (lm 1 4) (transformRight (li 2) (lm 1 3))
      shouldBe' (lm 1 4) (transformRight (li 3) (lm 1 3))
      shouldBe' (lm 1 3) (transformRight (li 4) (lm 1 3))

      shouldBe' (li 0) (transformLeft (li 0) (lm 1 2))
      shouldBe' (li 1) (transformLeft (li 1) (lm 1 2))
      shouldBe' (li 1) (transformLeft (li 2) (lm 1 2))
      shouldBe' (li 3) (transformLeft (li 3) (lm 1 2))

      shouldBe' (li 0) (transformLeft (li 0) (lm 3 1))
      shouldBe' (li 1) (transformLeft (li 1) (lm 3 1))
      shouldBe' (li 3) (transformLeft (li 2) (lm 3 1))
      shouldBe' (li 4) (transformLeft (li 3) (lm 3 1))
      shouldBe' (li 4) (transformLeft (li 4) (lm 3 1))

      shouldBe' (lm 4 2) (transformRight (li 0) (lm 3 1))
      shouldBe' (lm 4 2) (transformRight (li 1) (lm 3 1))
      shouldBe' (lm 4 1) (transformRight (li 2) (lm 3 1))
      shouldBe' (lm 4 1) (transformRight (li 3) (lm 3 1))
      shouldBe' (lm 3 1) (transformRight (li 4) (lm 3 1))

      shouldBe' (li 0) (transformLeft (li 0) (lm 2 1))
      shouldBe' (li 1) (transformLeft (li 1) (lm 2 1))
      shouldBe' (li 3) (transformLeft (li 2) (lm 2 1))
      shouldBe' (li 3) (transformLeft (li 3) (lm 2 1))


  describe "object" $ do
    it "passes sanity checks" $ do
      shouldBe' [v|{x:"a", y:"b"}|] (apply [v|{x:"a"}|] [s|{"p":["y"], "oi":"b"}|])
      shouldBe' [v|{}|] (apply [v|{x:"a"}|] [s|{"p":["x"], "od":"a"}|])
      shouldBe' [v|{x:"b"}|] (apply [v|{x:"a"}|] [s|{"p":["x"], "od":"a", "oi":"b"}|])

      shouldBe' [v|null|] (apply [v|"abc"|] [s|{"p":[], "od":"abc"}|])
      shouldBe' [v|42|] (apply [v|"abc"|] [s|{"p":[], "od":"abc", "oi":42}|])


    it "Ops on deleted elements become noops" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":["1", 0], "si":"hi"}|] [s|{"p":["1"], "od":"x"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":[], "od":"agimble s","oi":null}|] [s|{"p":[9],"si":"bite "}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":["1"], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [s|{"p":["1"], "od":"x"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":[], "od":"agimble s","oi":null}|] [s|{"p":[], "t":"text0", "o":[{"p":9, "i":"bite "}]}|])

    it "Ops on replaced elements become noops" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":["1", 0], "si":"hi"}|] [s|{"p":["1"], "od":"x", "oi":"y"}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":["1"], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [s|{"p":["1"], "od":"x", "oi":"y"}|])

    it "Deleted data is changed to reflect edits" $ do
      shouldBe' [s|{"p":["1"], "od":"abc"}|] (transformLeft [s|{"p":["1"], "od":"a"}|] [s|{"p":["1", 1], "si":"bc"}|])
      shouldBe' [s|{"p":["1"], "od":"abc"}|] (transformLeft [s|{"p":["1"], "od":"a"}|] [s|{"p":["1"], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
      shouldBe' [s|{"p":[], "od":25,"oi":[]}|] (transformLeft [s|{"p":[], "od":22,"oi":[]}|] [s|{"p":[],"na":3}|])
      shouldBe' [s|{"p":[], "od":{"toves":""},"oi":4}|] (transformLeft [s|{"p":[], "od":{"toves":0},"oi":4}|] [s|{"p":["toves"], "od":0,"oi":""}|])
      shouldBe' [s|{"p":[], "od":"thou an","oi":[]}|] (transformLeft [s|{"p":[], "od":"thou and ","oi":[]}|] [s|{"p":[7],"sd":"d "}|])
      shouldBe' [s|{"p":[], "od":"thou an","oi":[]}|] (transformLeft [s|{"p":[], "od":"thou and ","oi":[]}|] [s|{"p":[], "t":"text0", "o":[{"p":7, "d":"d "}]}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":[], "od":{"bird":38},"oi":20}|] [s|{"p":["bird"],"na":2}|])
      shouldBe' [s|{"p":[], "od":{"bird":40},"oi":20}|] (transformLeft [s|{"p":[], "od":{"bird":38},"oi":20}|] [s|{"p":["bird"],"na":2}|])
      shouldBe' [s|{"p":["He"], "od":[]}|] (transformRight [s|{"p":["The"],"na":-3}|] [s|{"p":["He"], "od":[]}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":["He"],"oi":{}}|] [s|{"p":[], "od":{},"oi":"the"}|])

    it "If two inserts are simultaneous, the lefts insert will win" $ do
      shouldBe' [s|{"p":["1"], "oi":"a", "od":"b"}|] (transformLeft [s|{"p":["1"], "oi":"a"}|] [s|{"p":["1"], "oi":"b"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":["1"], "oi":"a"}|] [s|{"p":["1"], "oi":"b"}|])

    it "parallel ops on different keys miss each other" $ do
      shouldBe' [s|{"p":["a"], "oi": "x"}|] (transformLeft [s|{"p":["a"], "oi":"x"}|] [s|{"p":["b"], "oi":"z"}|])
      shouldBe' [s|{"p":["a"], "oi": "x"}|] (transformLeft [s|{"p":["a"], "oi":"x"}|] [s|{"p":["b"], "od":"z"}|])
      shouldBe' [s|{"p":["in","he"],"oi":{}}|] (transformRight [s|{"p":["and"], "od":{}}|] [s|{"p":["in","he"],"oi":{}}|])
      shouldBe' [s|{"p":["x",0],"si":"his "}|] (transformRight [s|{"p":["y"], "od":0,"oi":1}|] [s|{"p":["x",0],"si":"his "}|])
      shouldBe' [s|{"p":["x"], "t":"text0", "o":[{"p":0, "i":"his "}]}|] (transformRight [s|{"p":["y"], "od":0, "oi":1}|] [s|{"p":["x"], "t":"text0", "o":[{"p":0, "i":"his "}]}|])

    it "replacement vs. deletion" $ do
      shouldBe' [s|{"p":[], "oi":{}}|] (transformRight [s|{"p":[], "od":[""]}|] [s|{"p":[], "od":[""], "oi":{}}|])

      shouldBe' [s|{"p":[], "oi": 42}|] (transformRight [s|{"p":[], "od":"foo"}|] [s|{"p":[], "od": "foo", "oi": 42}|])
      shouldBe' [s|{}|] (transformLeft [s|{"p":[], "od":"foo"}|] [s|{"p":[], "od": "foo", "oi": 42}|])

    it "replacement vs. replacement" $ do
      shouldBe' [l|[]|] (transformRight [l|[{"p":[], "od":[""]},{"p":[],"oi":null}]|] [l|[{"p":[], "od":[""]},{"p":[],"oi":{}}]|])
      shouldBe' [l|[{"p":[], "od":null,"oi":{}}]|] (transformLeft [l|[{"p":[], "od":[""]},{"p":[],"oi":{}}]|] [l|[{"p":[], "od":[""]},{"p":[],"oi":null}]|])
      -- shouldBe' [l|[]|] (transformRight' [l|[{"p":[], "od":[""],"oi":null}]|] [l|[{"p":[], "od":[""],"oi":{}}]|])
      -- shouldBe' [s|{"p":[], "od":null,"oi":{}}|] (transformLeft [s|{"p":[], "od":[""],"oi":{}}|] [s|{"p":[], "od":[""],"oi":null}|])

      -- -- test diamond property
      -- rightOps = [{"p":[],"od":null,"oi":{}}]
      -- leftOps = [{"p":[],"od":null,"oi":""}]
      -- rightHas = apply(null, rightOps)
      -- leftHas = apply(null, leftOps)

      -- let [left_, right_] = transformX type, leftOps, rightOps
      -- shouldBe' leftHas, apply rightHas, left_
      -- shouldBe' leftHas, apply leftHas, right_


    it "An attempt to re-delete a key becomes a no-op" $ do
      shouldBe' [s|{}|] (transformLeft [s|{"p":["k"], "od":"x"}|] [s|{"p":["k"], "od":"x"}|])
      shouldBe' [s|{}|] (transformRight [s|{"p":["k"], "od":"x"}|] [s|{"p":["k"], "od":"x"}|])


main :: IO ()
main = (testSpec "JSON specs" specs) >>= defaultMain
