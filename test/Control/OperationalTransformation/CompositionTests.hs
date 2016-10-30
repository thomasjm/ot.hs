{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, BangPatterns #-}

module Control.OperationalTransformation.JSON.Tests where

import Control.OperationalTransformation
import qualified Control.OperationalTransformation.JSON ()
import qualified Control.OperationalTransformation.JSON.Gen as JSONGen
import qualified Control.OperationalTransformation.Text0.Gen as Text0Gen
import Data.String.Interpolate.IsString
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Property
import Test.Tasty
import Test.Tasty.QuickCheck hiding (reason)


prop_operations_compose :: (Show doc, Show op, Eq doc, Arbitrary doc, OTSystem doc op, OTComposableOperation op)
                   => (doc -> Gen op) -> Property
prop_operations_compose genOp = property $ do
  document <- arbitrary
  op1 <- genOp document
  op2 <- genOp document

  let doc1 = case apply op1 document of
        Left err -> error [i|Couldn't apply #{op1} to #{document}|]
        Right doc -> doc
  let doc2 = case apply op2 document of
        Left err -> error [i|Couldn't apply #{op2} to #{document}|]
        Right doc -> doc

  let Right (op1', op2') = transform op1 op2

  let Right doc1' = apply op2' doc1
  let Right doc2' = apply op1' doc2

  if doc1' == doc2' then return $ property True
  else return $ property $ failed { reason = [i|transformed documents don't match.
document = #{document}

op1 = #{op1}
op2 = #{op2}

op1' = #{op1'}
op2' = #{op2'}

doc1' = #{doc1'}
doc2' = #{doc2'}|] }





jsonTests = testGroup "Control.OperationalTransformation.CompositionTests.JSON" [
  testProperty "prop_operations_compose" $ prop_operations_compose JSONGen.genOperation
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
