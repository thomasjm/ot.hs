{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.JSON.Specs where

import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Util
import Data.Aeson as A
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

-- These tests are taken directly from
-- https://github.com/ottypes/json0/blob/master/test/json0.coffee

compose :: A.Value -> A.Value -> A.Value
compose = error "`compose` not yet implemented"

transform :: A.Value -> A.Value -> (A.Value, A.Value)
transform val1 val2 = (toJSON op1', toJSON op2')
  where
    Success (op1 :: JSONOperation) = fromJSON val1
    Success (op2 :: JSONOperation) = fromJSON val2
    (op1', op2') = case C.transform op1 op2 of
      Left err -> error err
      Right x -> x


-- Just for REPL testing
transform' :: A.Value -> A.Value -> (JSONOperation, JSONOperation)
transform' val1 val2 = (op1', op2')
  where
    Success (op1 :: JSONOperation) = fromJSON val1
    Success (op2 :: JSONOperation) = fromJSON val2
    Right (op1', op2') = C.transform op1 op2

-- TODO: these might be backwards, not sure yet
transformLeft :: A.Value -> A.Value -> A.Value
transformLeft a b = a'
  where (a', b') = transform a b

transformRight :: A.Value -> A.Value -> A.Value
transformRight a b = a'
  where (b', a') = transform b a

specs :: SpecWith ()
specs = do
  describe "sanity" $ do
    --describe "#compose()" $ do
    --  it "od,oi --> od+oi" $ do
    --    shouldBe [j|{p:["foo"], od:1, oi:2}|] (compose [j|{p:["foo"],od:1}|] [j|{p:["foo"],oi:2}|])
    --    shouldBe [j|{p:["foo"], od:1}|],[j|{p:["bar"], oi:2}|] (compose [j|{p:["foo"],od:1}|] [j|{p:["bar"],oi:2}|])

    --  it "merges od+oi, od+oi -> od+oi" $ do
    --    shouldBe [j|{p:["foo"], od:1, oi:2}|] (compose [j|{p:["foo"],od:1,oi:3}|] [j|{p:["foo"],od:3,oi:2}|])


-- * Everything below this point isn't 100% converted from Coffeescript to Haskell yet
-- Uncommand and finish converting as needed

     --describe "#transform() stuff" $ do
     --  it "returns sane values" $ do
     --    let t = \op1 op2 -> do
     --         op1 `shouldBe` transformLeft op1 op2

     --    -- t [] []
     --    -- t [j|{p:["foo"], oi:1}|] []
     --    -- t [j|{p:["foo"], oi:1}|] [j|{p:["bar"], oi:2}|]
     --    t [j|{p:["foo"], oi:1}|] [j|{p:["bar"], oi:2}|]
     --    shouldBe True True

  -- describe "number" $ do
  --   it "Adds a number" $ do
  --     shouldBe 3, type.apply 1, [{p:[], na:2}]
  --     shouldBe [3], type.apply [1], [{p:[0], na:2}]

  --   it "compresses two adds together in compose" $ do
  --     shouldBe [{p:["a", "b"], na:3}], type.compose [{p:["a", "b"], na:1}], [{p:["a", "b"], na:2}]
  --     shouldBe [{p:["a"], na:1}, {p:["b"], na:2}], type.compose [{p:["a"], na:1}], [{p:["b"], na:2}]

  --   it "doesn\"t overwrite values when it merges na in append" $ do
  --     rightHas = 21
  --     leftHas = 3

  --     rightOp = [{"p":[],"od":0,"oi":15},{"p":[],"na":4},{"p":[],"na":1},{"p":[],"na":1}]
  --     leftOp = [{"p":[],"na":4},{"p":[],"na":-1}]
  --     [right_, left_] = transformX type, rightOp, leftOp

  --     s_c = type.apply rightHas, left_
  --     c_s = type.apply leftHas, right_
  --     shouldBe s_c, c_s


  -- # Strings should be handled internally by the text type. We"ll just do some basic sanity checks here.
  -- describe "string" $ do
  --   describe "#apply()", -> it "works" $ do
  --     shouldBe "abc", type.apply "a", [{p:[1], si:"bc"}]
  --     shouldBe "bc", type.apply "abc", [{p:[0], sd:"a"}]
  --     shouldBe {x:"abc"}, type.apply {x:"a"}, [{p:["x", 1], si:"bc"}]

  --   describe "#transform()" $ do
  --     it "splits deletes" $ do
  --       shouldBe type.transform([{p:[0], sd:"ab"}], [{p:[1], si:"x"}], "left"), [{p:[0], sd:"a"}, {p:[1], sd:"b"}]

  --     it "cancels out other deletes" $ do
  --       shouldBe type.transform([{p:["k", 5], sd:"a"}], [{p:["k", 5], sd:"a"}], "left"), []

  --     it "does not throw errors with blank inserts" $ do
  --       shouldBe type.transform([{p: ["k", 5], si:""}], [{p: ["k", 3], si: "a"}], "left"), []

  -- describe "string subtype" $ do
  --   describe "#apply()" $ do
  --     it "works" $ do
  --       shouldBe "abc", type.apply "a", [{p:[], t:"text0", o:[{p:1, i:"bc"}]}]
  --       shouldBe "bc", type.apply "abc", [{p:[], t:"text0", o:[{p:0, d:"a"}]}]
  --       shouldBe {x:"abc"}, type.apply {x:"a"}, [{p:["x"], t:"text0", o:[{p:1, i:"bc"}]}]

  --   describe "#transform()" $ do
  --     it "splits deletes" $ do
  --       a = [{p:[], t:"text0", o:[{p:0, d:"ab"}]}]
  --       b = [{p:[], t:"text0", o:[{p:1, i:"x"}]}]
  --       shouldBe type.transform(a, b, "left"), [{p:[], t:"text0", o:[{p:0, d:"a"}, {p:1, d:"b"}]}]

  --     it "cancels out other deletes" $ do
  --       shouldBe type.transform([{p:["k"], t:"text0", o:[{p:5, d:"a"}]}], [{p:["k"], t:"text0", o:[{p:5, d:"a"}]}], "left"), []

  --     it "does not throw errors with blank inserts" $ do
  --       shouldBe type.transform([{p:["k"], t:"text0", o:[{p:5, i:""}]}], [{p:["k"], t:"text0", o:[{p:3, i:"a"}]}], "left"), []

  -- describe "subtype with non-array operation" $ do
  --   describe "#transform()" $ do
  --     it "works" $ do
  --       a = [{p:[], t:"mock", o:"foo"}]
  --       b = [{p:[], t:"mock", o:"bar"}]
  --       shouldBe type.transform(a, b, "left"), [{p:[], t:"mock", o:{mock:true}}]

  -- describe "list" $ do
  --   describe "apply" $ do
  --     it "inserts" $ do
  --       shouldBe ["a", "b", "c"], type.apply ["b", "c"], [{p:[0], li:"a"}]
  --       shouldBe ["a", "b", "c"], type.apply ["a", "c"], [{p:[1], li:"b"}]
  --       shouldBe ["a", "b", "c"], type.apply ["a", "b"], [{p:[2], li:"c"}]

  --     it "deletes" $ do
  --       shouldBe ["b", "c"], type.apply ["a", "b", "c"], [{p:[0], ld:"a"}]
  --       shouldBe ["a", "c"], type.apply ["a", "b", "c"], [{p:[1], ld:"b"}]
  --       shouldBe ["a", "b"], type.apply ["a", "b", "c"], [{p:[2], ld:"c"}]

  --     it "replaces" $ do
  --       shouldBe ["a", "y", "b"], type.apply ["a", "x", "b"], [{p:[1], ld:"x", li:"y"}]

  --     it "moves" $ do
  --       shouldBe ["a", "b", "c"], type.apply ["b", "a", "c"], [{p:[1], lm:0}]
  --       shouldBe ["a", "b", "c"], type.apply ["b", "a", "c"], [{p:[0], lm:1}]

  --     ###
  --     "null moves compose to nops" $ do
  --       shouldBe [], type.compose [], [{p:[3],lm:3}]
  --       shouldBe [], type.compose [], [{p:[0,3],lm:3}]
  --       shouldBe [], type.compose [], [{p:["x","y",0],lm:0}]
  --     ###

  describe "#transform()" $ do
    it "bumps paths when list elements are inserted or removed" $ do
      shouldBe [j|{p:[2, 200], si:"hi"}|] (transformLeft [j|{p:[1, 200], si:"hi"}|] [j|{p:[0], li:"x"}|])
      shouldBe [j|{p:[1, 201], si:"hi"}|] (transformRight [j|{p:[0, 201], si:"hi"}|] [j|{p:[0], li:"x"}|])
      shouldBe [j|{p:[0, 202], si:"hi"}|] (transformLeft [j|{p:[0, 202], si:"hi"}|] [j|{p:[1], li:"x"}|])
      shouldBe [j|{p:[2], t:"text0", o:[{p:200, i:"hi"}]}|] (transformLeft [j|{p:[1], t:"text0", o:[{p:200, i:"hi"}]}|] [j|{p:[0], li:"x"}|])
      shouldBe [j|{p:[1], t:"text0", o:[{p:201, i:"hi"}]}|] (transformRight [j|{p:[0], t:"text0", o:[{p:201, i:"hi"}]}|] [j|{p:[0], li:"x"}|])
      shouldBe [j|{p:[0], t:"text0", o:[{p:202, i:"hi"}]}|] (transformLeft [j|{p:[0], t:"text0", o:[{p:202, i:"hi"}]}|] [j|{p:[1], li:"x"}|])

      shouldBe [j|{p:[0, 203], si:"hi"}|] (transformLeft [j|{p:[1, 203], si:"hi"}|] [j|{p:[0], ld:"x"}|])
      shouldBe [j|{p:[0, 204], si:"hi"}|] (transformLeft [j|{p:[0, 204], si:"hi"}|] [j|{p:[1], ld:"x"}|])
      shouldBe [j|{p:["x",3], si: "hi"}|] (transformLeft [j|{p:["x",3], si:"hi"}|] [j|{p:["x",0,"x"], li:0}|])

      -- TODO: these seem to contain invalid string inserts -- the last element of the path is not a number
      -- shouldBe [j|{p:["x",3,"x"], si: "hi"}|] (transformLeft [j|{p:["x",3,"x"], si:"hi"}|] [j|{p:["x",5], li:0}|])
      -- shouldBe [j|{p:["x",4,"x"], si: "hi"}|] (transformLeft [j|{p:["x",3,"x"], si:"hi"}|] [j|{p:["x",0], li:0}|])

      shouldBe [j|{p:[0], t:"text0", o:[{p:203, i:"hi"}]}|] (transformLeft [j|{p:[1], t:"text0", o:[{p:203, i:"hi"}]}|] [j|{p:[0], ld:"x"}|])
      shouldBe [j|{p:[0], t:"text0", o:[{p:204, i:"hi"}]}|] (transformLeft [j|{p:[0], t:"text0", o:[{p:204, i:"hi"}]}|] [j|{p:[1], ld:"x"}|])
      -- shouldBe [j|{p:["x"], t:"text0", o:[{p:3, i:"hi"}]}|] (transformLeft [j|{p:["x"], t:"text0", o:[{p:3, i:"hi"}]}|] [j|{p:["x",0,"x"], li:0}|])


      shouldBe [j|{p:[1],ld:2}|] (transformLeft [j|{p:[0],ld:2}|] [j|{p:[0],li:1}|])
      -- shouldBe [j|{p:[1],ld:2}|] (transformRight [j|{p:[0],ld:2}|] [j|{p:[0],li:1}|])

    it "converts ops on deleted elements to noops" $ do
      shouldBe [j|{}|] (transformLeft [j|{p:[1, 0], si:"hi"}|] [j|{p:[1], ld:"x"}|])
      -- shouldBe [] (transformLeft [j|{p:[1], t:"text0", o:[j|{p:0, i:"hi"}|]}|] [j|{p:[1], ld:"x"}|])
      -- shouldBe [j|{p:[0],li:"x"}|] (transformLeft [j|{p:[0],li:"x"}|] [j|{p:[0],ld:"y"}|])
      -- shouldBe [] (transformLeft [j|{p:[0],na:-3}|] [j|{p:[0],ld:48}|])

        --     it "converts ops on replaced elements to noops" $ do
               --shouldBe [], type.transform [{p:[1, 0], si:"hi"}], [{p:[1], ld:"x", li:"y"}], "left"
        --       shouldBe [], type.transform [{p:[1], t:"text0", o:[{p:0, i:"hi"}]}], [{p:[1], ld:"x", li:"y"}], "left"
        --       shouldBe [{p:[0], li:"hi"}], type.transform [{p:[0], li:"hi"}], [{p:[0], ld:"x", li:"y"}], "left"

        --     it "changes deleted data to reflect edits" $ do
        --       shouldBe [{p:[1], ld:"abc"}], type.transform [{p:[1], ld:"a"}], [{p:[1, 1], si:"bc"}], "left"
        --       shouldBe [{p:[1], ld:"abc"}], type.transform [{p:[1], ld:"a"}], [{p:[1], t:"text0", o:[{p:1, i:"bc"}]}], "left"

    it "Puts the left op first if two inserts are simultaneous" $ do
      [j|{p:[1], li:"a"}|] `shouldBe` transformLeft  [j|{p:[1], li:"a"}|] [j|{p:[1], li:"b"}|]
      [j|{p:[2], li:"b"}|] `shouldBe` transformRight [j|{p:[1], li:"b"}|] [j|{p:[1], li:"a"}|]

  --     it "converts an attempt to re-delete a list element into a no-op" $ do
  --       shouldBe [], type.transform [{p:[1], ld:"x"}], [{p:[1], ld:"x"}], "left"
  --       shouldBe [], type.transform [{p:[1], ld:"x"}], [{p:[1], ld:"x"}], "right"


  --   describe "#compose()" $ do
  --     it "composes insert then delete into a no-op" $ do
  --       shouldBe [], type.compose [{p:[1], li:"abc"}], [{p:[1], ld:"abc"}]
  --       shouldBe [{p:[1],ld:null,li:"x"}], type.transform [{p:[0],ld:null,li:"x"}], [{p:[0],li:"The"}], "right"

  --     it "doesn\"t change the original object" $ do
  --       a = [{p:[0],ld:"abc",li:null}]
  --       shouldBe [{p:[0],ld:"abc"}], type.compose a, [{p:[0],ld:null}]
  --       shouldBe [{p:[0],ld:"abc",li:null}], a

  --     it "composes together adjacent string ops" $ do
  --       shouldBe [{p:[100], si:"hi"}], type.compose [{p:[100], si:"h"}], [{p:[101], si:"i"}]
  --       shouldBe [{p:[], t:"text0", o:[{p:100, i:"hi"}]}], type.compose [{p:[], t:"text0", o:[{p:100, i:"h"}]}], [{p:[], t:"text0", o:[{p:101, i:"i"}]}]

  --   it "moves ops on a moved element with the element" $ do
  --     shouldBe [{p:[10], ld:"x"}], type.transform [{p:[4], ld:"x"}], [{p:[4], lm:10}], "left"
  --     shouldBe [{p:[10, 1], si:"a"}], type.transform [{p:[4, 1], si:"a"}], [{p:[4], lm:10}], "left"
  --     shouldBe [{p:[10], t:"text0", o:[{p:1, i:"a"}]}], type.transform [{p:[4], t:"text0", o:[{p:1, i:"a"}]}], [{p:[4], lm:10}], "left"
  --     shouldBe [{p:[10, 1], li:"a"}], type.transform [{p:[4, 1], li:"a"}], [{p:[4], lm:10}], "left"
  --     shouldBe [{p:[10, 1], ld:"b", li:"a"}], type.transform [{p:[4, 1], ld:"b", li:"a"}], [{p:[4], lm:10}], "left"

  --     shouldBe [{p:[0],li:null}], type.transform [{p:[0],li:null}], [{p:[0],lm:1}], "left"
  --     # [_,_,_,_,5,6,7,_]
  --     # c: [_,_,_,_,5,"x",6,7,_]   p:5 li:"x"
  --     # s: [_,6,_,_,_,5,7,_]       p:5 lm:1
  --     # correct: [_,6,_,_,_,5,"x",7,_]
  --     shouldBe [{p:[6],li:"x"}], type.transform [{p:[5],li:"x"}], [{p:[5],lm:1}], "left"
  --     # [_,_,_,_,5,6,7,_]
  --     # c: [_,_,_,_,5,6,7,_]  p:5 ld:6
  --     # s: [_,6,_,_,_,5,7,_]  p:5 lm:1
  --     # correct: [_,_,_,_,5,7,_]
  --     shouldBe [{p:[1],ld:6}], type.transform [{p:[5],ld:6}], [{p:[5],lm:1}], "left"
  --     #shouldBe [{p:[0],li:{}}], type.transform [{p:[0],li:{}}], [{p:[0],lm:0}], "right"
  --     shouldBe [{p:[0],li:[]}], type.transform [{p:[0],li:[]}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[2],li:"x"}], type.transform [{p:[2],li:"x"}], [{p:[0],lm:1}], "left"

  --   it "moves target index on ld/li" $ do
  --     shouldBe [{p:[0],lm:1}], type.transform [{p:[0], lm: 2}], [{p:[1], ld:"x"}], "left"
  --     shouldBe [{p:[1],lm:3}], type.transform [{p:[2], lm: 4}], [{p:[1], ld:"x"}], "left"
  --     shouldBe [{p:[0],lm:3}], type.transform [{p:[0], lm: 2}], [{p:[1], li:"x"}], "left"
  --     shouldBe [{p:[3],lm:5}], type.transform [{p:[2], lm: 4}], [{p:[1], li:"x"}], "left"
  --     shouldBe [{p:[1],lm:1}], type.transform [{p:[0], lm: 0}], [{p:[0], li:28}], "left"

  --   it "tiebreaks lm vs. ld/li" $ do
  --     shouldBe [], type.transform [{p:[0], lm: 2}], [{p:[0], ld:"x"}], "left"
  --     shouldBe [], type.transform [{p:[0], lm: 2}], [{p:[0], ld:"x"}], "right"
  --     shouldBe [{p:[1], lm:3}], type.transform [{p:[0], lm: 2}], [{p:[0], li:"x"}], "left"
  --     shouldBe [{p:[1], lm:3}], type.transform [{p:[0], lm: 2}], [{p:[0], li:"x"}], "right"

  --   it "replacement vs. deletion" $ do
  --     shouldBe [{p:[0],li:"y"}], type.transform [{p:[0],ld:"x",li:"y"}], [{p:[0],ld:"x"}], "right"

  --   it "replacement vs. insertion" $ do
  --     shouldBe [{p:[1],ld:{},li:"brillig"}], type.transform [{p:[0],ld:{},li:"brillig"}], [{p:[0],li:36}], "left"

  --   it "replacement vs. replacement" $ do
  --     shouldBe [], type.transform [{p:[0],ld:null,li:[]}], [{p:[0],ld:null,li:0}], "right"
  --     shouldBe [{p:[0],ld:[],li:0}], type.transform [{p:[0],ld:null,li:0}], [{p:[0],ld:null,li:[]}], "left"

  --   it "composes replace with delete of replaced element results in insert" $ do
  --     shouldBe [{p:[2],ld:[]}], type.compose [{p:[2],ld:[],li:null}], [{p:[2],ld:null}]

  --   it "lm vs lm" $ do
  --     shouldBe [{p:[0],lm:2}], type.transform [{p:[0],lm:2}], [{p:[2],lm:1}], "left"
  --     shouldBe [{p:[4],lm:4}], type.transform [{p:[3],lm:3}], [{p:[5],lm:0}], "left"
  --     shouldBe [{p:[2],lm:0}], type.transform [{p:[2],lm:0}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[2],lm:1}], type.transform [{p:[2],lm:0}], [{p:[1],lm:0}], "right"
  --     shouldBe [{p:[3],lm:1}], type.transform [{p:[2],lm:0}], [{p:[5],lm:0}], "right"
  --     shouldBe [{p:[3],lm:0}], type.transform [{p:[2],lm:0}], [{p:[5],lm:0}], "left"
  --     shouldBe [{p:[0],lm:5}], type.transform [{p:[2],lm:5}], [{p:[2],lm:0}], "left"
  --     shouldBe [{p:[0],lm:5}], type.transform [{p:[2],lm:5}], [{p:[2],lm:0}], "left"
  --     shouldBe [{p:[0],lm:0}], type.transform [{p:[1],lm:0}], [{p:[0],lm:5}], "right"
  --     shouldBe [{p:[0],lm:0}], type.transform [{p:[1],lm:0}], [{p:[0],lm:1}], "right"
  --     shouldBe [{p:[1],lm:1}], type.transform [{p:[0],lm:1}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[1],lm:2}], type.transform [{p:[0],lm:1}], [{p:[5],lm:0}], "right"
  --     shouldBe [{p:[3],lm:2}], type.transform [{p:[2],lm:1}], [{p:[5],lm:0}], "right"
  --     shouldBe [{p:[2],lm:1}], type.transform [{p:[3],lm:1}], [{p:[1],lm:3}], "left"
  --     shouldBe [{p:[2],lm:3}], type.transform [{p:[1],lm:3}], [{p:[3],lm:1}], "left"
  --     shouldBe [{p:[2],lm:6}], type.transform [{p:[2],lm:6}], [{p:[0],lm:1}], "left"
  --     shouldBe [{p:[2],lm:6}], type.transform [{p:[2],lm:6}], [{p:[0],lm:1}], "right"
  --     shouldBe [{p:[2],lm:6}], type.transform [{p:[2],lm:6}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[2],lm:6}], type.transform [{p:[2],lm:6}], [{p:[1],lm:0}], "right"
  --     shouldBe [{p:[0],lm:2}], type.transform [{p:[0],lm:1}], [{p:[2],lm:1}], "left"
  --     shouldBe [{p:[2],lm:0}], type.transform [{p:[2],lm:1}], [{p:[0],lm:1}], "right"
  --     shouldBe [{p:[1],lm:1}], type.transform [{p:[0],lm:0}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[0],lm:0}], type.transform [{p:[0],lm:1}], [{p:[1],lm:3}], "left"
  --     shouldBe [{p:[3],lm:1}], type.transform [{p:[2],lm:1}], [{p:[3],lm:2}], "left"
  --     shouldBe [{p:[3],lm:3}], type.transform [{p:[3],lm:2}], [{p:[2],lm:1}], "left"

  --   it "changes indices correctly around a move" $ do
  --     shouldBe [{p:[1,0],li:{}}], type.transform [{p:[0,0],li:{}}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[0],lm:0}], type.transform [{p:[1],lm:0}], [{p:[0],ld:{}}], "left"
  --     shouldBe [{p:[0],lm:0}], type.transform [{p:[0],lm:1}], [{p:[1],ld:{}}], "left"
  --     shouldBe [{p:[5],lm:0}], type.transform [{p:[6],lm:0}], [{p:[2],ld:{}}], "left"
  --     shouldBe [{p:[1],lm:0}], type.transform [{p:[1],lm:0}], [{p:[2],ld:{}}], "left"
  --     shouldBe [{p:[1],lm:1}], type.transform [{p:[2],lm:1}], [{p:[1],ld:3}], "right"

  --     shouldBe [{p:[1],ld:{}}], type.transform [{p:[2],ld:{}}], [{p:[1],lm:2}], "right"
  --     shouldBe [{p:[2],ld:{}}], type.transform [{p:[1],ld:{}}], [{p:[2],lm:1}], "left"


  --     shouldBe [{p:[0],ld:{}}], type.transform [{p:[1],ld:{}}], [{p:[0],lm:1}], "right"

  --     shouldBe [{p:[0],ld:1,li:2}], type.transform [{p:[1],ld:1,li:2}], [{p:[1],lm:0}], "left"
  --     shouldBe [{p:[0],ld:2,li:3}], type.transform [{p:[1],ld:2,li:3}], [{p:[0],lm:1}], "left"
  --     shouldBe [{p:[1],ld:3,li:4}], type.transform [{p:[0],ld:3,li:4}], [{p:[1],lm:0}], "left"

  --   it "li vs lm" $ do
  --     li = (p) -> [{p:[p],li:[]}]
  --     lm = (f,t) -> [{p:[f],lm:t}]
  --     xf = type.transform

  --     shouldBe (li 0), xf (li 0), (lm 1, 3), "left"
  --     shouldBe (li 1), xf (li 1), (lm 1, 3), "left"
  --     shouldBe (li 1), xf (li 2), (lm 1, 3), "left"
  --     shouldBe (li 2), xf (li 3), (lm 1, 3), "left"
  --     shouldBe (li 4), xf (li 4), (lm 1, 3), "left"

  --     shouldBe (lm 2, 4), xf (lm 1, 3), (li 0), "right"
  --     shouldBe (lm 2, 4), xf (lm 1, 3), (li 1), "right"
  --     shouldBe (lm 1, 4), xf (lm 1, 3), (li 2), "right"
  --     shouldBe (lm 1, 4), xf (lm 1, 3), (li 3), "right"
  --     shouldBe (lm 1, 3), xf (lm 1, 3), (li 4), "right"

  --     shouldBe (li 0), xf (li 0), (lm 1, 2), "left"
  --     shouldBe (li 1), xf (li 1), (lm 1, 2), "left"
  --     shouldBe (li 1), xf (li 2), (lm 1, 2), "left"
  --     shouldBe (li 3), xf (li 3), (lm 1, 2), "left"

  --     shouldBe (li 0), xf (li 0), (lm 3, 1), "left"
  --     shouldBe (li 1), xf (li 1), (lm 3, 1), "left"
  --     shouldBe (li 3), xf (li 2), (lm 3, 1), "left"
  --     shouldBe (li 4), xf (li 3), (lm 3, 1), "left"
  --     shouldBe (li 4), xf (li 4), (lm 3, 1), "left"

  --     shouldBe (lm 4, 2), xf (lm 3, 1), (li 0), "right"
  --     shouldBe (lm 4, 2), xf (lm 3, 1), (li 1), "right"
  --     shouldBe (lm 4, 1), xf (lm 3, 1), (li 2), "right"
  --     shouldBe (lm 4, 1), xf (lm 3, 1), (li 3), "right"
  --     shouldBe (lm 3, 1), xf (lm 3, 1), (li 4), "right"

  --     shouldBe (li 0), xf (li 0), (lm 2, 1), "left"
  --     shouldBe (li 1), xf (li 1), (lm 2, 1), "left"
  --     shouldBe (li 3), xf (li 2), (lm 2, 1), "left"
  --     shouldBe (li 3), xf (li 3), (lm 2, 1), "left"


  -- describe "object" $ do
  --   it "passes sanity checks" $ do
  --     shouldBe {x:"a", y:"b"}, type.apply {x:"a"}, [{p:["y"], oi:"b"}]
  --     shouldBe {}, type.apply {x:"a"}, [{p:["x"], od:"a"}]
  --     shouldBe {x:"b"}, type.apply {x:"a"}, [{p:["x"], od:"a", oi:"b"}]

  --   it "Ops on deleted elements become noops" $ do
  --     shouldBe [], type.transform [{p:[1, 0], si:"hi"}], [{p:[1], od:"x"}], "left"
  --     shouldBe [], type.transform [{p:[1], t:"text0", o:[{p:0, i:"hi"}]}], [{p:[1], od:"x"}], "left"
  --     shouldBe [], type.transform [{p:[9],si:"bite "}], [{p:[],od:"agimble s",oi:null}], "right"
  --     shouldBe [], type.transform [{p:[], t:"text0", o:[{p:9, i:"bite "}]}], [{p:[],od:"agimble s",oi:null}], "right"

  --   it "Ops on replaced elements become noops" $ do
  --     shouldBe [], type.transform [{p:[1, 0], si:"hi"}], [{p:[1], od:"x", oi:"y"}], "left"
  --     shouldBe [], type.transform [{p:[1], t:"text0", o:[{p:0, i:"hi"}]}], [{p:[1], od:"x", oi:"y"}], "left"

  --   it "Deleted data is changed to reflect edits" $ do
  --     shouldBe [{p:[1], od:"abc"}], type.transform [{p:[1], od:"a"}], [{p:[1, 1], si:"bc"}], "left"
  --     shouldBe [{p:[1], od:"abc"}], type.transform [{p:[1], od:"a"}], [{p:[1], t:"text0", o:[{p:1, i:"bc"}]}], "left"
  --     shouldBe [{p:[],od:25,oi:[]}], type.transform [{p:[],od:22,oi:[]}], [{p:[],na:3}], "left"
  --     shouldBe [{p:[],od:{toves:""},oi:4}], type.transform [{p:[],od:{toves:0},oi:4}], [{p:["toves"],od:0,oi:""}], "left"
  --     shouldBe [{p:[],od:"thou an",oi:[]}], type.transform [{p:[],od:"thou and ",oi:[]}], [{p:[7],sd:"d "}], "left"
  --     shouldBe [{p:[],od:"thou an",oi:[]}], type.transform [{p:[],od:"thou and ",oi:[]}], [{p:[], t:"text0", o:[{p:7, d:"d "}]}], "left"
  --     shouldBe [], type.transform([{p:["bird"],na:2}], [{p:[],od:{bird:38},oi:20}], "right")
  --     shouldBe [{p:[],od:{bird:40},oi:20}], type.transform([{p:[],od:{bird:38},oi:20}], [{p:["bird"],na:2}], "left")
  --     shouldBe [{p:["He"],od:[]}], type.transform [{p:["He"],od:[]}], [{p:["The"],na:-3}], "right"
  --     shouldBe [], type.transform [{p:["He"],oi:{}}], [{p:[],od:{},oi:"the"}], "left"

  --   it "If two inserts are simultaneous, the lefts insert will win" $ do
  --     shouldBe [{p:[1], oi:"a", od:"b"}], type.transform [{p:[1], oi:"a"}], [{p:[1], oi:"b"}], "left"
  --     shouldBe [], type.transform [{p:[1], oi:"b"}], [{p:[1], oi:"a"}], "right"

  --   it "parallel ops on different keys miss each other" $ do
  --     shouldBe [{p:["a"], oi: "x"}], type.transform [{p:["a"], oi:"x"}], [{p:["b"], oi:"z"}], "left"
  --     shouldBe [{p:["a"], oi: "x"}], type.transform [{p:["a"], oi:"x"}], [{p:["b"], od:"z"}], "left"
  --     shouldBe [{p:["in","he"],oi:{}}], type.transform [{p:["in","he"],oi:{}}], [{p:["and"],od:{}}], "right"
  --     shouldBe [{p:["x",0],si:"his "}], type.transform [{p:["x",0],si:"his "}], [{p:["y"],od:0,oi:1}], "right"
  --     shouldBe [{p:["x"], t:"text0", o:[{p:0, i:"his "}]}], type.transform [{p:["x"],t:"text0", o:[{p:0, i:"his "}]}], [{p:["y"],od:0,oi:1}], "right"

  --   it "replacement vs. deletion" $ do
  --     shouldBe [{p:[],oi:{}}], type.transform [{p:[],od:[""],oi:{}}], [{p:[],od:[""]}], "right"

  --   it "replacement vs. replacement" $ do
  --     shouldBe [],                     type.transform [{p:[],od:[""]},{p:[],oi:{}}], [{p:[],od:[""]},{p:[],oi:null}], "right"
  --     shouldBe [{p:[],od:null,oi:{}}], type.transform [{p:[],od:[""]},{p:[],oi:{}}], [{p:[],od:[""]},{p:[],oi:null}], "left"
  --     shouldBe [],                     type.transform [{p:[],od:[""],oi:{}}], [{p:[],od:[""],oi:null}], "right"
  --     shouldBe [{p:[],od:null,oi:{}}], type.transform [{p:[],od:[""],oi:{}}], [{p:[],od:[""],oi:null}], "left"

  --     # test diamond property
  --     rightOps = [ {"p":[],"od":null,"oi":{}} ]
  --     leftOps = [ {"p":[],"od":null,"oi":""} ]
  --     rightHas = type.apply(null, rightOps)
  --     leftHas = type.apply(null, leftOps)

  --     [left_, right_] = transformX type, leftOps, rightOps
  --     shouldBe leftHas, type.apply rightHas, left_
  --     shouldBe leftHas, type.apply leftHas, right_


  --   it "An attempt to re-delete a key becomes a no-op" $ do
  --     shouldBe [], type.transform [{p:["k"], od:"x"}], [{p:["k"], od:"x"}], "left"
  --     shouldBe [], type.transform [{p:["k"], od:"x"}], [{p:["k"], od:"x"}], "right"


main :: IO ()
main = (testSpec "JSON specs" specs) >>= defaultMain
