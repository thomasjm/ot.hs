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

compose :: JSONOp -> JSONOp -> JSONOperation
compose op1 op2 = case C.compose (JSONOperation [op1]) (JSONOperation [op2]) of
  Left err -> error err
  Right operation -> operation

transform :: JSONOp -> JSONOp -> (JSONOp, JSONOp)
transform op1 op2 = case C.transform (JSONOperation [op1]) (JSONOperation [op2]) of
  Left err -> error err
  Right (JSONOperation [op1'], JSONOperation [op2']) -> (op1', op2')

apply :: A.Value -> JSONOp -> A.Value
apply input op = case C.apply (JSONOperation [op]) input of
  Left err -> error err
  Right x -> x

transformLeft :: JSONOp -> JSONOp -> JSONOp
transformLeft a b = a'
  where (a', _) = transform a b

transformRight :: JSONOp -> JSONOp -> JSONOp
transformRight a b = a'
  where (_, a') = transform b a

compose' :: JSONOperation -> JSONOperation -> JSONOperation
compose' op1 op2 = case C.compose op1 op2 of
  Left err -> error err
  Right operation -> operation

transform' :: JSONOperation -> JSONOperation -> (JSONOperation, JSONOperation)
transform' op1 op2 = case C.transform op1 op2 of
  Left err -> error err
  Right x -> x

transformLeft' :: JSONOperation -> JSONOperation -> JSONOperation
transformLeft' a b = a'
  where (a', _) = transform' a b

transformRight' :: JSONOperation -> JSONOperation -> JSONOperation
transformRight' a b = a'
  where (_, a') = transform' b a


shouldBe' :: (Eq a, Show a) => a -> a -> Expectation
shouldBe' = flip shouldBe

specs :: SpecWith ()
specs = do
  describe "sanity" $ do
    describe "compose()" $ do
     it "od,oi --> od+oi" $ do
       shouldBe' [l|[{"p":["foo"], "od":1, "oi":2}]|] (compose [j|{"p":["foo"], "od":1}|] [j|{"p":["foo"], "oi":2}|])
       shouldBe' [l|[{"p":["foo"], "od":1}, {"p":["bar"], "oi":2}]|] (compose [j|{"p":["foo"], "od":1}|] [j|{"p":["bar"], "oi":2}|])

     it "merges od+oi, od+oi -> od+oi" $ do
       shouldBe' [l|[{"p":["foo"], "od":1, "oi":2}]|] (compose [j|{"p":["foo"], "od":1, "oi":3}|] [j|{"p":["foo"], "od":3, "oi":2}|])

  describe "transform() stuff" $ do
   it "returns sane values" $ do
     let t = \op1 op2 -> op1 `shouldBe'` transformLeft op1 op2

     t [j|{}|] [j|{}|]
     t [j|{"p":["foo"], "oi":1}|] [j|{}|]
     t [j|{"p":["foo"], "oi":1}|] [j|{"p":["bar"], "oi":2}|]
     t [j|{"p":["foo"], "oi":1}|] [j|{"p":["bar"], "oi":2}|]

  describe "number" $ do
    it "Adds a number" $ do
      shouldBe' [v|3|] (apply [v|1|] [j|{"p":[], "na":2}|])
      shouldBe' [v|[3]|] (apply [v|[1]|] [j|{"p":[0], "na":2}|])

    it "compresses two adds together in compose" $ do
      shouldBe' [l|[{"p":["a", "b"], "na":3}]|] (compose [j|{"p":["a", "b"], "na":1}|] [j|{"p":["a", "b"], "na":2}|])
      -- shouldBe' [j|{"p":["a"], "na":1}, {"p":["b"], "na":2}], type.compose [{"p":["a"], "na":1}], [{"p":["b"], "na":2}]

    -- it "doesn\"t overwrite values when it merges na in append" $ do
    --   let rightHas = A.Number 21
    --   let leftHas = A.Number 3

    --   let rightOp = [[j|{"p":[],"od":0,"oi":15}|], [j|{"p":[],"na":4}|], [j|{"p":[],"na":1}|], [j|{"p":[],"na":1}|]]
    --   let leftOp = [[j|{"p":[],"na":4}|], [j|{"p":[],"na":-1}|]]

    --   let left = [map (transformLeft x) rightOp | x <- leftOp]
    --   let right = fmap transformRight leftOp

    --   let s_c = foldl (flip apply) rightHas left
    --   let c_s = apply right leftHas
    --   s_c `shouldBe'` c_s


  -- # Strings should be handled internally by the text type. We"ll just do some basic sanity checks here.
  describe "string" $ do
    describe "apply()" $ it "works" $ do
      shouldBe' [v|"abc"|] (apply [v|"a"|] [j|{"p":[1], "si":"bc"}|])
      shouldBe' [v|"bc"|] (apply [v|"abc"|] [j|{"p":[0], "sd":"a"}|])
      shouldBe' [v|{x:"abc"}|] (apply [v|{x:"a"}|] [j|{"p":["x", 1], "si":"bc"}|])

  describe "transform()" $ do
    it "splits deletes" $ do
      shouldBe' [l|[{"p":[0], "sd":"a"}, {"p":[1], "sd":"b"}]|] (transformLeft' [l|[{"p":[0], "sd":"ab"}]|] [l|[{"p":[1], "si":"x"}]|])

    it "cancels out other deletes" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":["k", 5], "sd":"a"}|] [j|{"p":["k", 5], "sd":"a"}|])

    it "does not throw errors with blank inserts" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p": ["k", 5], "si":""}|] [j|{"p": ["k", 3], "si": "a"}|])

  describe "string subtype" $ do
    describe "apply()" $ do
      it "works" $ do
        shouldBe [v|"abc"|] (apply [v|"a"|] [j|{"p":[], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
        shouldBe [v|{x:"abc"}|] (apply [v|{x:"a"}|] [j|{"p":["x"], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
        shouldBe [v|"bc"|] (apply [v|"abc"|] [j|{"p":[], "t":"text0", "o":[{"p":0, "d":"a"}]}|])

    describe "transform()" $ do
      it "splits deletes" $ do
        let a = [j|{"p":[], "t":"text0", "o":[{"p":0, "d":"ab"}]}|]
        let b = [j|{"p":[], "t":"text0", "o":[{"p":1, "i":"x"}]}|]
        shouldBe' [j|{"p":[], "t":"text0", "o":[{"p":0, "d":"a"}, {"p":1, "d":"b"}]}|] (transformLeft a b)

      it "cancels out other deletes" $ do
        shouldBe' [j|{}|] (transformLeft [j|{"p":["k"], "t":"text0", "o":[{"p":5, "d":"a"}]}|] [j|{"p":["k"], "t":"text0", "o":[{"p":5, "d":"a"}]}|])

      it "does not throw errors with blank inserts" $ do
        shouldBe' [j|{}|] (transformLeft [j|{"p":["k"], "t":"text0", "o":[{"p":5, "i":""}]}|] [j|{"p":["k"], "t":"text0", "o":[{"p":3, "i":"a"}]}|])

  describe "list" $ do
    describe "apply" $ do
      it "inserts" $ do
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "c"]|] [j|{"p":[0], "li":"a"}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["a", "c"]|] [j|{"p":[1], "li":"b"}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["a", "b"]|] [j|{"p":[2], "li":"c"}|])

      it "deletes" $ do
        shouldBe' [v|["b", "c"]|] (apply [v|["a", "b", "c"]|] [j|{"p":[0], "ld":"a"}|])
        shouldBe' [v|["a", "c"]|] (apply [v|["a", "b", "c"]|] [j|{"p":[1], "ld":"b"}|])
        shouldBe' [v|["a", "b"]|] (apply [v|["a", "b", "c"]|] [j|{"p":[2], "ld":"c"}|])

      it "replaces" $ do
        shouldBe' [v|["a", "y", "b"]|] (apply [v|["a", "x", "b"]|] [j|{"p":[1], "ld":"x", "li":"y"}|])

      it "moves" $ do
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "a", "c"]|] [j|{"p":[1], "lm":0}|])
        shouldBe' [v|["a", "b", "c"]|] (apply [v|["b", "a", "c"]|] [j|{"p":[0], "lm":1}|])

      it "null moves compose to nops" $ do
        shouldBe' [l|[]|] (compose' [l|[]|] [l|[{"p":[3], "lm":3}]|])
        shouldBe' [l|[]|] (compose' [l|[]|] [l|[{"p":[0,3], "lm":3}]|])
        shouldBe' [l|[]|] (compose' [l|[]|] [l|[{"p":["x","y",0], "lm":0}]|])

  describe "transform()" $ do
    it "bumps paths when list elements are inserted or removed" $ do
      shouldBe' [j|{"p":[2, 200], "si":"hi"}|] (transformLeft [j|{"p":[1, 200], "si":"hi"}|] [j|{"p":[0], "li":"x"}|])
      shouldBe' [j|{"p":[1, 201], "si":"hi"}|] (transformRight [j|{"p":[0, 201], "si":"hi"}|] [j|{"p":[0], "li":"x"}|])
      shouldBe' [j|{"p":[0, 202], "si":"hi"}|] (transformLeft [j|{"p":[0, 202], "si":"hi"}|] [j|{"p":[1], "li":"x"}|])
      shouldBe' [j|{"p":[2], "t":"text0", "o":[{"p":200, "i":"hi"}]}|] (transformLeft [j|{"p":[1], "t":"text0", "o":[{"p":200, "i":"hi"}]}|] [j|{"p":[0], "li":"x"}|])
      shouldBe' [j|{"p":[1], "t":"text0", "o":[{"p":201, "i":"hi"}]}|] (transformRight [j|{"p":[0], "t":"text0", "o":[{"p":201, "i":"hi"}]}|] [j|{"p":[0], "li":"x"}|])
      shouldBe' [j|{"p":[0], "t":"text0", "o":[{"p":202, "i":"hi"}]}|] (transformLeft [j|{"p":[0], "t":"text0", "o":[{"p":202, "i":"hi"}]}|] [j|{"p":[1], "li":"x"}|])

      shouldBe' [j|{"p":[0, 203], "si":"hi"}|] (transformLeft [j|{"p":[1, 203], "si":"hi"}|] [j|{"p":[0], "ld":"x"}|])
      shouldBe' [j|{"p":[0, 204], "si":"hi"}|] (transformLeft [j|{"p":[0, 204], "si":"hi"}|] [j|{"p":[1], "ld":"x"}|])

      -- TODO: seems to be an invalid list insert
      -- shouldBe' [j|{"p":["x",3], "si": "hi"}|] (transformLeft [j|{"p":["x",3], "si":"hi"}|] [j|{"p":["x",0,"x"], "li":0}|])

      shouldBe' [j|{"p":["x",3,2], "si": "hi"}|] (transformLeft [j|{"p":["x",3,2], "si":"hi"}|] [j|{"p":["x",5], "li":0}|])
      shouldBe' [j|{"p":["x",4,2], "si": "hi"}|] (transformLeft [j|{"p":["x",3,2], "si":"hi"}|] [j|{"p":["x",0], "li":0}|])

      shouldBe' [j|{"p":[1], "ld":2}|] (transformLeft [j|{"p":[0], "ld":2}|] [j|{"p":[0], "li":1}|])
      shouldBe' [j|{"p":[1], "ld":2}|] (transformRight [j|{"p":[0], "ld":2}|] [j|{"p":[0], "li":1}|])

      shouldBe' [j|{"p":[0], "t":"text0", "o":[{"p":203, "i":"hi"}]}|] (transformLeft [j|{"p":[1], "t":"text0", "o":[{"p":203, "i":"hi"}]}|] [j|{"p":[0], "ld":"x"}|])
      shouldBe' [j|{"p":[0], "t":"text0", "o":[{"p":204, "i":"hi"}]}|] (transformLeft [j|{"p":[0], "t":"text0", "o":[{"p":204, "i":"hi"}]}|] [j|{"p":[1], "ld":"x"}|])

      -- TODO: this looks like an invalid list insert because the list element is not a pos
      -- shouldBe' [j|{"p":["x"], "t":"text0", "o":[{"p":3, "i":"hi"}]}|] (transformLeft [j|{"p":["x"], "t":"text0", "o":[{"p":3, "i":"hi"}]}|] [j|{"p":["x",0,"x"], "li":0}|])


    it "converts ops on deleted elements to noops" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":[1, 0], "si":"hi"}|] [j|{"p":[1], "ld":"x"}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":[1], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [j|{"p":[1], "ld":"x"}|])
      shouldBe' [j|{"p":[0], "li":"x"}|] (transformLeft [j|{"p":[0], "li":"x"}|] [j|{"p":[0], "ld":"y"}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":[0],"na":-3}|] [j|{"p":[0], "ld":48}|])

    it "converts ops on replaced elements to noops" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":[1, 0], "si":"hi"}|] [j|{"p":[1], "ld":"x", "li":"y"}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":[1], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [j|{"p":[1], "ld":"x", "li":"y"}|])
      shouldBe' [j|{"p":[0], "li":"hi"}|] (transformLeft [j|{"p":[0], "li":"hi"}|] [j|{"p":[0], "ld":"x", "li":"y"}|])

    it "changes deleted data to reflect edits" $ do
      shouldBe' [j|{"p":[1], "ld":"abc"}|] (transformLeft [j|{"p":[1], "ld":"a"}|] [j|{"p":[1, 1], "si":"bc"}|])
      shouldBe' [j|{"p":[1], "ld":"abc"}|] (transformLeft [j|{"p":[1], "ld":"a"}|] [j|{"p":[1], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])

    it "Puts the left op first if two inserts are simultaneous" $ do
      shouldBe' [j|{"p":[1], "li":"a"}|] (transformLeft [j|{"p":[1], "li":"a"}|] [j|{"p":[1], "li":"b"}|])
      shouldBe' [j|{"p":[2], "li":"b"}|] (transformRight [j|{"p":[1], "li":"b"}|] [j|{"p":[1], "li":"a"}|])

    it "converts an attempt to re-delete a list element into a no-op" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":[1], "ld":"x"}|] [j|{"p":[1], "ld":"x"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":[1], "ld":"x"}|] [j|{"p":[1], "ld":"x"}|])


    describe "compose()" $ do
      it "composes insert then delete into a no-op" $ do
        shouldBe' [l|[{}]|] (compose [j|{"p":[1], "li":"abc"}|] [j|{"p":[1], "ld":"abc"}|])
        shouldBe' [l|[{"p":[0], "ld":"abc"}]|] (compose [j|{"p":[0], "ld":"abc", "li":null}|] [j|{"p":[0], "ld":null}|])
        shouldBe' [j|{"p":[1], "ld":null, "li":"x"}|] (transformRight [j|{"p":[0], "ld":null, "li":"x"}|] [j|{"p":[0], "li":"The"}|])

      it "composes together adjacent string ops" $ do
        shouldBe' [l|[{"p":[100], "si":"hi"}]|] (compose [j|{"p":[100], "si":"h"}|] [j|{"p":[101], "si":"i"}|])
        shouldBe' [l|[{"p":[], "t":"text0", "o":[{"p":100, "i":"hi"}]}]|] (compose [j|{"p":[], "t":"text0", "o":[{"p":100, "i":"h"}]}|] [j|{"p":[], "t":"text0", "o":[{"p":101, "i":"i"}]}|])

    it "moves ops on a moved element with the element" $ do
      shouldBe' [j|{"p":[10], "ld":"x"}|] (transformLeft [j|{"p":[4], "ld":"x"}|] [j|{"p":[4], "lm":10}|])
      shouldBe' [j|{"p":[10, 1], "si":"a"}|] (transformLeft [j|{"p":[4, 1], "si":"a"}|] [j|{"p":[4], "lm":10}|])
      shouldBe' [j|{"p":[10], "t":"text0", "o":[{"p":1, "i":"a"}]}|] (transformLeft [j|{"p":[4], "t":"text0", "o":[{"p":1, "i":"a"}]}|] [j|{"p":[4], "lm":10}|])
      shouldBe' [j|{"p":[10, 1], "li":"a"}|] (transformLeft [j|{"p":[4, 1], "li":"a"}|] [j|{"p":[4], "lm":10}|])
      shouldBe' [j|{"p":[10, 1], "ld":"b", "li":"a"}|] (transformLeft [j|{"p":[4, 1], "ld":"b", "li":"a"}|] [j|{"p":[4], "lm":10}|])

      shouldBe' [j|{"p":[0], "li":null}|] (transformLeft [j|{"p":[0], "li":null}|] [j|{"p":[0], "lm":1}|])
      -- -- [_,_,_,_,5,6,7,_]
      -- -- c: [_,_,_,_,5,"x",6,7,_]   p:5 "li":"x"
      -- -- s: [_,6,_,_,_,5,7,_]       p:5 "lm":1
      -- -- correct: [_,6,_,_,_,5,"x",7,_]
      shouldBe' [j|{"p":[6], "li":"x"}|] (transformLeft [j|{"p":[5], "li":"x"}|] [j|{"p":[5], "lm":1}|])
      -- -- [_,_,_,_,5,6,7,_]
      -- -- c: [_,_,_,_,5,6,7,_]  p:5 "ld":6
      -- -- s: [_,6,_,_,_,5,7,_]  p:5 "lm":1
      -- -- correct: [_,_,_,_,5,7,_]
      shouldBe' [j|{"p":[1], "ld":6}|] (transformLeft [j|{"p":[5], "ld":6}|] [j|{"p":[5], "lm":1}|])
      shouldBe' [j|{"p":[0], "li":{}}|] (transformRight [j|{"p":[0], "li":{}}|] [j|{"p":[0], "lm":0}|])
      shouldBe' [j|{"p":[0], "li":[]}|] (transformLeft [j|{"p":[0], "li":[]}|] [j|{"p":[1], "lm":0}|])
      shouldBe' [j|{"p":[2], "li":"x"}|] (transformLeft [j|{"p":[2], "li":"x"}|] [j|{"p":[0], "lm":1}|])

    it "moves target index on ld/li" $ do
      shouldBe' [j|{"p":[0], "lm":1}|] (transformLeft [j|{"p":[0], "lm": 2}|] [j|{"p":[1], "ld":"x"}|])
      shouldBe' [j|{"p":[1], "lm":3}|] (transformLeft [j|{"p":[2], "lm": 4}|] [j|{"p":[1], "ld":"x"}|])
      shouldBe' [j|{"p":[0], "lm":3}|] (transformLeft [j|{"p":[0], "lm": 2}|] [j|{"p":[1], "li":"x"}|])
      shouldBe' [j|{"p":[3], "lm":5}|] (transformLeft [j|{"p":[2], "lm": 4}|] [j|{"p":[1], "li":"x"}|])
      shouldBe' [j|{"p":[1], "lm":1}|] (transformLeft [j|{"p":[0], "lm": 0}|] [j|{"p":[0], "li":28}|])

    it "tiebreaks lm vs. ld/li" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":[0], "lm": 2}|] [j|{"p":[0], "ld":"x"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":[0], "lm": 2}|] [j|{"p":[0], "ld":"x"}|])
      shouldBe' [j|{"p":[1], "lm":3}|] (transformLeft [j|{"p":[0], "lm": 2}|] [j|{"p":[0], "li":"x"}|])
      shouldBe' [j|{"p":[1], "lm":3}|] (transformRight [j|{"p":[0], "lm": 2}|] [j|{"p":[0], "li":"x"}|])

    it "replacement vs. deletion" $ do
      shouldBe' [j|{"p":[0], "li":"y"}|] (transformRight [j|{"p":[0], "ld":"x", "li":"y"}|] [j|{"p":[0], "ld":"x"}|])

    it "replacement vs. insertion" $ do
      shouldBe' [j|{"p":[1], "ld":{}, "li":"brillig"}|] (transformLeft [j|{"p":[0], "ld":{}, "li":"brillig"}|] [j|{"p":[0], "li":36}|])

    it "replacement vs. replacement" $ do
      shouldBe' [j|{}|]                 (transformRight [j|{"p":[0], "ld":null, "li":[]}|] [j|{"p":[0], "ld":null, "li":0}|])
      shouldBe' [j|{"p":[0], "ld":[], "li":0}|] (transformLeft  [j|{"p":[0], "ld":null, "li":0}|] [j|{"p":[0], "ld":null, "li":[]}|])

    it "composes replace with delete of replaced element results in insert" $ do
      shouldBe' [l|[{"p":[2], "ld":[]}]|] (compose [j|{"p":[2], "ld":[], "li":null}|] [j|{"p":[2], "ld":null}|])

    it "lm vs lm" $ do
      shouldBe' [j|{"p":[0], "lm":2}|] (transformLeft [j|{"p":[0], "lm":2}|] [j|{"p":[2], "lm":1}|])

      shouldBe' [j|{"p":[4], "lm":4}|] (transformLeft [j|{"p":[3], "lm":3}|] [j|{"p":[5], "lm":0}|])

      shouldBe' [j|{"p":[2], "lm":0}|] (transformLeft [j|{"p":[2], "lm":0}|] [j|{"p":[1], "lm":0}|])
      shouldBe' [j|{"p":[2], "lm":1}|] (transformRight [j|{"p":[2], "lm":0}|] [j|{"p":[1], "lm":0}|])

      shouldBe' [j|{"p":[3], "lm":1}|] (transformRight [j|{"p":[2], "lm":0}|] [j|{"p":[5], "lm":0}|])
      shouldBe' [j|{"p":[3], "lm":0}|] (transformLeft [j|{"p":[2], "lm":0}|] [j|{"p":[5], "lm":0}|])

      -- TODO: why is this test duplicated. One side should be right I guess?
      shouldBe' [j|{"p":[0], "lm":5}|] (transformLeft [j|{"p":[2], "lm":5}|] [j|{"p":[2], "lm":0}|])
      shouldBe' [j|{"p":[0], "lm":5}|] (transformLeft [j|{"p":[2], "lm":5}|] [j|{"p":[2], "lm":0}|])

      shouldBe' [j|{"p":[0], "lm":0}|] (transformRight [j|{"p":[1], "lm":0}|] [j|{"p":[0], "lm":5}|])
      shouldBe' [j|{"p":[0], "lm":0}|] (transformRight [j|{"p":[1], "lm":0}|] [j|{"p":[0], "lm":1}|])

      shouldBe' [j|{"p":[1], "lm":1}|] (transformLeft [j|{"p":[0], "lm":1}|] [j|{"p":[1], "lm":0}|])
      shouldBe' [j|{"p":[1], "lm":2}|] (transformRight [j|{"p":[0], "lm":1}|] [j|{"p":[5], "lm":0}|])

      shouldBe' [j|{"p":[3], "lm":2}|] (transformRight [j|{"p":[2], "lm":1}|] [j|{"p":[5], "lm":0}|])

      shouldBe' [j|{"p":[2], "lm":1}|] (transformLeft [j|{"p":[3], "lm":1}|] [j|{"p":[1], "lm":3}|])
      shouldBe' [j|{"p":[2], "lm":3}|] (transformLeft [j|{"p":[1], "lm":3}|] [j|{"p":[3], "lm":1}|])

      shouldBe' [j|{"p":[2], "lm":6}|] (transformLeft [j|{"p":[2], "lm":6}|] [j|{"p":[0], "lm":1}|])
      shouldBe' [j|{"p":[2], "lm":6}|] (transformRight [j|{"p":[2], "lm":6}|] [j|{"p":[0], "lm":1}|])

      shouldBe' [j|{"p":[2], "lm":6}|] (transformLeft [j|{"p":[2], "lm":6}|] [j|{"p":[1], "lm":0}|])
      shouldBe' [j|{"p":[2], "lm":6}|] (transformRight [j|{"p":[2], "lm":6}|] [j|{"p":[1], "lm":0}|])

      shouldBe' [j|{"p":[0], "lm":2}|] (transformLeft [j|{"p":[0], "lm":1}|] [j|{"p":[2], "lm":1}|])
      shouldBe' [j|{"p":[2], "lm":0}|] (transformRight [j|{"p":[2], "lm":1}|] [j|{"p":[0], "lm":1}|])

      shouldBe' [j|{"p":[1], "lm":1}|] (transformLeft [j|{"p":[0], "lm":0}|] [j|{"p":[1], "lm":0}|])

      shouldBe' [j|{"p":[0], "lm":0}|] (transformLeft [j|{"p":[0], "lm":1}|] [j|{"p":[1], "lm":3}|])

      shouldBe' [j|{"p":[3], "lm":1}|] (transformLeft [j|{"p":[2], "lm":1}|] [j|{"p":[3], "lm":2}|])
      shouldBe' [j|{"p":[3], "lm":3}|] (transformLeft [j|{"p":[3], "lm":2}|] [j|{"p":[2], "lm":1}|])

    it "changes indices correctly around a move" $ do
      shouldBe' [j|{"p":[1,0], "li":{}}|] (transformLeft [j|{"p":[0,0], "li":{}}|] [j|{"p":[1], "lm":0}|])
      -- shouldBe' [j|{"p":[0], "lm":0}|] (transformLeft [j|{"p":[1], "lm":0}|] [j|{"p":[0], "ld":{}}|])
      -- shouldBe' [j|{"p":[0], "lm":0}|] (transformLeft [j|{"p":[0], "lm":1}|] [j|{"p":[1], "ld":{}}|])
      -- shouldBe' [j|{"p":[5], "lm":0}|] (transformLeft [j|{"p":[6], "lm":0}|] [j|{"p":[2], "ld":{}}|])
      -- shouldBe' [j|{"p":[1], "lm":0}|] (transformLeft [j|{"p":[1], "lm":0}|] [j|{"p":[2], "ld":{}}|])
      -- shouldBe' [j|{"p":[1], "lm":1}|] (transformRight [j|{"p":[2], "lm":1}|] [j|{"p":[1], "ld":3}|])

      -- shouldBe' [j|{"p":[1], "ld":{}}|] (transformRight [j|{"p":[2], "ld":{}}|] [j|{"p":[1], "lm":2}|])
      -- shouldBe' [j|{"p":[2], "ld":{}}|] (transformLeft [j|{"p":[1], "ld":{}}|] [j|{"p":[2], "lm":1}|])


      -- shouldBe' [j|{"p":[0], "ld":{}}|] (transformRight [j|{"p":[1], "ld":{}}|] [j|{"p":[0], "lm":1}|])

      -- shouldBe' [j|{"p":[0], "ld":1, "li":2}|] (transformLeft [j|{"p":[1], "ld":1, "li":2}|] [j|{"p":[1], "lm":0}|])
      -- shouldBe' [j|{"p":[0], "ld":2, "li":3}|] (transformLeft [j|{"p":[1], "ld":2, "li":3}|] [j|{"p":[0], "lm":1}|])
      -- shouldBe' [j|{"p":[1], "ld":3, "li":4}|] (transformLeft [j|{"p":[0], "ld":3, "li":4}|] [j|{"p":[1], "lm":0}|])

    it "li vs lm" $ do
      let li = \(p :: Int) -> ListInsert [] p [v|[]|]
      let lm :: Int -> Int -> JSONOp
          lm = \f t -> ListMove [] f t

      shouldBe' (li 0) (transformLeft (li 0) (lm 1 3))
      shouldBe' (li 1) (transformLeft (li 1) (lm 1 3))
      shouldBe' (li 1) (transformLeft (li 2) (lm 1 3))
      shouldBe' (li 2) (transformLeft (li 3) (lm 1 3))
      shouldBe' (li 4) (transformLeft (li 4) (lm 1 3))

      shouldBe' (lm 2 4) (transformRight (lm 1 3) (li 0))
      shouldBe' (lm 2 4) (transformRight (lm 1 3) (li 1))
      shouldBe' (lm 1 4) (transformRight (lm 1 3) (li 2))
      shouldBe' (lm 1 4) (transformRight (lm 1 3) (li 3))
      shouldBe' (lm 1 3) (transformRight (lm 1 3) (li 4))

      shouldBe' (li 0) (transformLeft (li 0) (lm 1 2))
      shouldBe' (li 1) (transformLeft (li 1) (lm 1 2))
      shouldBe' (li 1) (transformLeft (li 2) (lm 1 2))
      shouldBe' (li 3) (transformLeft (li 3) (lm 1 2))

      shouldBe' (li 0) (transformLeft (li 0) (lm 3 1))
      shouldBe' (li 1) (transformLeft (li 1) (lm 3 1))
      shouldBe' (li 3) (transformLeft (li 2) (lm 3 1))
      shouldBe' (li 4) (transformLeft (li 3) (lm 3 1))
      shouldBe' (li 4) (transformLeft (li 4) (lm 3 1))

      shouldBe' (lm 4 2) (transformRight (lm 3 1) (li 0))
      shouldBe' (lm 4 2) (transformRight (lm 3 1) (li 1))
      shouldBe' (lm 4 1) (transformRight (lm 3 1) (li 2))
      shouldBe' (lm 4 1) (transformRight (lm 3 1) (li 3))
      shouldBe' (lm 3 1) (transformRight (lm 3 1) (li 4))

      shouldBe' (li 0) (transformLeft (li 0) (lm 2 1))
      shouldBe' (li 1) (transformLeft (li 1) (lm 2 1))
      shouldBe' (li 3) (transformLeft (li 2) (lm 2 1))
      shouldBe' (li 3) (transformLeft (li 3) (lm 2 1))


  describe "object" $ do
    it "passes sanity checks" $ do
      shouldBe' [v|{x:"a", y:"b"}|] (apply [v|{x:"a"}|] [j|{"p":["y"], "oi":"b"}|])
      shouldBe' [v|{}|] (apply [v|{x:"a"}|] [j|{"p":["x"], "od":"a"}|])
      shouldBe' [v|{x:"b"}|] (apply [v|{x:"a"}|] [j|{"p":["x"], "od":"a", "oi":"b"}|])

      shouldBe' [v|null|] (apply [v|"abc"|] [j|{"p":[], "od":"abc"}|])
      shouldBe' [v|42|] (apply [v|"abc"|] [j|{"p":[], "od":"abc", "oi":42}|])


    it "Ops on deleted elements become noops" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":["1", 0], "si":"hi"}|] [j|{"p":["1"], "od":"x"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":[9],"si":"bite "}|] [j|{"p":[], "od":"agimble s","oi":null}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":["1"], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [j|{"p":["1"], "od":"x"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":[], "t":"text0", "o":[{"p":9, "i":"bite "}]}|] [j|{"p":[], "od":"agimble s","oi":null}|])

    it "Ops on replaced elements become noops" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":["1", 0], "si":"hi"}|] [j|{"p":["1"], "od":"x", "oi":"y"}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":["1"], "t":"text0", "o":[{"p":0, "i":"hi"}]}|] [j|{"p":["1"], "od":"x", "oi":"y"}|])

    it "Deleted data is changed to reflect edits" $ do
      shouldBe' [j|{"p":["1"], "od":"abc"}|] (transformLeft [j|{"p":["1"], "od":"a"}|] [j|{"p":["1", 1], "si":"bc"}|])
      shouldBe' [j|{"p":["1"], "od":"abc"}|] (transformLeft [j|{"p":["1"], "od":"a"}|] [j|{"p":["1"], "t":"text0", "o":[{"p":1, "i":"bc"}]}|])
      shouldBe' [j|{"p":[], "od":25,"oi":[]}|] (transformLeft [j|{"p":[], "od":22,"oi":[]}|] [j|{"p":[],"na":3}|])
      shouldBe' [j|{"p":[], "od":{"toves":""},"oi":4}|] (transformLeft [j|{"p":[], "od":{"toves":0},"oi":4}|] [j|{"p":["toves"], "od":0,"oi":""}|])
      shouldBe' [j|{"p":[], "od":"thou an","oi":[]}|] (transformLeft [j|{"p":[], "od":"thou and ","oi":[]}|] [j|{"p":[7],"sd":"d "}|])
      shouldBe' [j|{"p":[], "od":"thou an","oi":[]}|] (transformLeft [j|{"p":[], "od":"thou and ","oi":[]}|] [j|{"p":[], "t":"text0", "o":[{"p":7, "d":"d "}]}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":["bird"],"na":2}|] [j|{"p":[], "od":{"bird":38},"oi":20}|])
      shouldBe' [j|{"p":[], "od":{"bird":40},"oi":20}|] (transformLeft [j|{"p":[], "od":{"bird":38},"oi":20}|] [j|{"p":["bird"],"na":2}|])
      shouldBe' [j|{"p":["He"], "od":[]}|] (transformRight [j|{"p":["He"], "od":[]}|] [j|{"p":["The"],"na":-3}|])
      shouldBe' [j|{}|] (transformLeft [j|{"p":["He"],"oi":{}}|] [j|{"p":[], "od":{},"oi":"the"}|])

    it "If two inserts are simultaneous, the lefts insert will win" $ do
      shouldBe' [j|{"p":["1"], "oi":"a", "od":"b"}|] (transformLeft [j|{"p":["1"], "oi":"a"}|] [j|{"p":["1"], "oi":"b"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":["1"], "oi":"b"}|] [j|{"p":["1"], "oi":"a"}|])

    it "parallel ops on different keys miss each other" $ do
      shouldBe' [j|{"p":["a"], "oi": "x"}|] (transformLeft [j|{"p":["a"], "oi":"x"}|] [j|{"p":["b"], "oi":"z"}|])
      shouldBe' [j|{"p":["a"], "oi": "x"}|] (transformLeft [j|{"p":["a"], "oi":"x"}|] [j|{"p":["b"], "od":"z"}|])
      shouldBe' [j|{"p":["in","he"],"oi":{}}|] (transformRight [j|{"p":["in","he"],"oi":{}}|] [j|{"p":["and"], "od":{}}|])
      shouldBe' [j|{"p":["x",0],"si":"his "}|] (transformRight [j|{"p":["x",0],"si":"his "}|] [j|{"p":["y"], "od":0,"oi":1}|])
      shouldBe' [j|{"p":["x"], "t":"text0", "o":[{"p":0, "i":"his "}]}|] (transformRight [j|{"p":["x"], "t":"text0", "o":[{"p":0, "i":"his "}]}|] [j|{"p":["y"], "od":0, "oi":1}|])

    it "replacement vs. deletion" $ do
      shouldBe' [j|{"p":[], "oi":{}}|] (transformRight [j|{"p":[], "od":[""], "oi":{}}|] [j|{"p":[], "od":[""]}|])

    -- it "replacement vs. replacement" $ do
    --   shouldBe' [] (transformRight [j|{"p":[], "od":[""]}|,{"p":[],"oi":{}}|] [j|{"p":[], "od":[""]}|,{"p":[],"oi":null}])|]
    --   shouldBe' [j|{"p":[], "od":null,"oi":{}}|] (transformLeft [j|{"p":[], "od":[""]}|,{"p":[],"oi":{}}] [j|{"p":[], "od":[""]}|,{"p":[],"oi":null}])
    --   shouldBe' [] (transformRight [j|{"p":[], "od":[""],"oi":{}}|] [j|{"p":[], "od":[""],"oi":null}|])
    --   shouldBe' [j|{"p":[], "od":null,"oi":{}}|] (transformLeft [j|{"p":[], "od":[""],"oi":{}}|] [j|{"p":[], "od":[""],"oi":null}|])

    --   -- test diamond property
    --   rightOps = [{"p":[],"od":null,"oi":{}}]
    --   leftOps = [{"p":[],"od":null,"oi":""}]
    --   rightHas = apply(null, rightOps)
    --   leftHas = apply(null, leftOps)

    --   let [left_, right_] = transformX type, leftOps, rightOps
    --   shouldBe' leftHas, apply rightHas, left_
    --   shouldBe' leftHas, apply leftHas, right_


    it "An attempt to re-delete a key becomes a no-op" $ do
      shouldBe' [j|{}|] (transformLeft [j|{"p":["k"], "od":"x"}|] [j|{"p":["k"], "od":"x"}|])
      shouldBe' [j|{}|] (transformRight [j|{"p":["k"], "od":"x"}|] [j|{"p":["k"], "od":"x"}|])


main :: IO ()
main = (testSpec "JSON specs" specs) >>= defaultMain
