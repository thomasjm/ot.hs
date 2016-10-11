module Main (main) where

import qualified Control.OperationalTransformation.ClientServerTests
import qualified Control.OperationalTransformation.JSON.Specs
import qualified Control.OperationalTransformation.JSON.Tests
import qualified Control.OperationalTransformation.Selection.Tests
import qualified Control.OperationalTransformation.Text.Tests
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  jsonHspecTests <- testSpec "JSON specs" Control.OperationalTransformation.JSON.Specs.specs
  defaultMain $ testGroup "All tests" [
    Control.OperationalTransformation.ClientServerTests.tests
    , jsonHspecTests
    , Control.OperationalTransformation.JSON.Tests.tests
    , Control.OperationalTransformation.Selection.Tests.tests
    , Control.OperationalTransformation.Text.Tests.tests
    ]
