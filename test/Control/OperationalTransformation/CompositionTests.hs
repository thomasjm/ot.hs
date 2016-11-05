{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module Control.OperationalTransformation.JSON.Tests where

import Control.OperationalTransformation
import qualified Control.OperationalTransformation.JSON ()
import qualified Control.OperationalTransformation.JSON.Gen as JSONGen
import Control.OperationalTransformation.JSON.QuasiQuote
import qualified Control.OperationalTransformation.Text0.Gen as Text0Gen
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.String.Interpolate.IsString
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)

document = [v|[{}, null, "z"]|]
o1 = [l|[{"p":[2],"li":"aa"},{"p":[2,0],"si":"bbb"}]|]
o2 = [l|[{"p":[2,0],"si":"cccc"}]|]


prop_operations_compose :: (ToJSON doc, ToJSON op, Show doc, Show op, Eq doc, Arbitrary doc, OTSystem doc op, OTComposableOperation op)
                   => (doc -> Gen op) -> Property
prop_operations_compose genOp = property $ do
  document <- arbitrary
  op1 <- genOp document
  op2 <- genOp document

  return $ testOps document op1 op2

testOps :: (ToJSON doc, ToJSON op, Show doc, Show op, Eq doc, Arbitrary doc, OTSystem doc op, OTComposableOperation op)
                   => doc -> op -> op -> Property
testOps document op1 op2 = property $ do
  let doc1 = case apply op1 document of
        Left err -> error [i|(1) Couldn't apply
#{encode op1} to
#{encodePretty document}

Err: #{err}}|]
        Right doc -> doc
  let doc2 = case apply op2 document of
        Left err -> error [i|(2) Couldn't apply
#{encode op2} to
#{encodePretty document}

Err: #{err}|]
        Right doc -> doc

  let (op1', op2') = case transform op1 op2 of
        Left err -> error [i|(3) Couldn't transform ops

op1 = #{encode op1}
op2 = #{encode op2}

document = #{encodePretty document}

Err: #{err}|]
        Right (op1', op2') -> (op1', op2')

  let doc1' = case apply op2' doc1 of
        Left err -> error [i|(4) Couldn't apply op2': #{err}
document = #{encodePretty document}

op1 = #{encode op1}
op2 = #{encode op2}

doc1 = #{encode doc1}
op2' = #{encode op2'}|]
        Right doc -> doc
  let doc2' = case apply op1' doc2 of
        Left err -> error [i|(5) Couldn't apply op1': #{err}
document = #{encodePretty document}

op1 = #{encode op1}
op2 = #{encode op2}

doc2 = #{encode doc2}
op1' = #{encode op1'}|]
        Right doc -> doc

  if doc1' == doc2' then property True
  else property $ failed { reason = [i|(Final) Transformed documents don't match.
document = #{encodePretty document}

op1 = #{encode op1}
op2 = #{encode op2}

doc1 = #{encode doc1}
doc2 = #{encode doc2}

op1' = #{encode op1'}
op2' = #{encode op2'}

doc1' = #{encodePretty doc1'}
doc2' = #{encodePretty doc2'}|] }





jsonTests = testGroup "Control.OperationalTransformation.CompositionTests.JSON" [
  testProperty "prop_operations_compose_single" $ prop_operations_compose JSONGen.genOperation,
  testProperty "prop_operations_compose_multi" $ prop_operations_compose JSONGen.genMultiOperation
  ]

textTests = testGroup "Control.OperationalTransformation.CompositionTests.Text0" [
  testProperty "prop_operations_compose_single" $ prop_operations_compose Text0Gen.genOperation,
  testProperty "prop_operations_compose_multi" $ prop_operations_compose Text0Gen.genMultiOperation
  ]

jsonMain :: IO ()
jsonMain = defaultMain jsonTests

textMain :: IO ()
textMain = defaultMain textTests

main :: IO ()
main = defaultMain $ testGroup "All composition tests"   [jsonTests
                                                         , textTests
                                                         ]
