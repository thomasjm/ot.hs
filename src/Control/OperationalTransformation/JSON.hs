{-# LANGUAGE TupleSections, ViewPatterns, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where


import Control.OperationalTransformation
import Control.OperationalTransformation.JSON.Affects (affects)
import qualified Control.OperationalTransformation.JSON.Apply as AP
import qualified Control.OperationalTransformation.JSON.Compose as C
import Control.OperationalTransformation.JSON.Transform (transformDouble, transformRight)
import Control.OperationalTransformation.JSON.Types
import qualified Data.Aeson as A


invertOperation = undefined

instance OTOperation JSONOperation where
  -- Handle identities up front
  transform Identity op = Right (Identity, op)
  transform op Identity = Right (op, Identity)

  -- Operations that both affect each other
  transform x y | x `affects` y && y `affects` x = transformDouble x y

  -- Operations where the left affects the right
  -- since `x` is unaffected by `y`, `x'` is just `x`
  transform x y | x `affects` y = (x, ) <$> (transformRight x y)

  -- Operations where the right affects the left
  -- since `y` is unaffected by `x`, `y'` is just `y`
  transform x y | y `affects` x = (, y) <$> (transformRight y x)

  -- Operations that don't affect each other
  transform x y = Right (x, y)

-- Not sure if it's possible to write a total compose function...
-- But the tests have some compose in them
instance OTComposableOperation JSONOperation where
  compose = C.compose

instance OTSystem A.Value JSONOperation where
  apply = AP.apply
