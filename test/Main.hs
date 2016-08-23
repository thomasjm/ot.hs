module Main (main) where

import Test.Tasty

import qualified Control.OperationalTransformation.ClientServerTests
import qualified Control.OperationalTransformation.JSON.Tests
import qualified Control.OperationalTransformation.Selection.Tests
import qualified Control.OperationalTransformation.Text.Tests

main :: IO ()
main = defaultMain $ testGroup "All tests" [Control.OperationalTransformation.ClientServerTests.tests
                                           , Control.OperationalTransformation.JSON.Tests.tests
                                           , Control.OperationalTransformation.Selection.Tests.tests
                                           , Control.OperationalTransformation.Text.Tests.tests
                                           ]
