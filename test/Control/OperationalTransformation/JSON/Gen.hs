{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.OperationalTransformation.JSON.Gen
  ( genOperation
  ) where

import Control.OperationalTransformation.JSON
import Control.OperationalTransformation.Properties (ArbitraryFor (..))
import qualified Data.Aeson as A
import Test.QuickCheck hiding (Result)

-- |TODO: generate arbitrary JSONOperations
genOperation :: A.Value -> Gen JSONOperation
genOperation = undefined

instance ArbitraryFor A.Value JSONOperation where
  arbitraryFor = genOperation

instance Arbitrary A.Value where
  arbitrary = undefined -- TODO: write instance

instance Arbitrary JSONOperation where
  arbitrary = arbitrary >>= genOperation
