{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where


import Control.OperationalTransformation
import Control.OperationalTransformation.JSON.Types

invertOperation = undefined

-- TODO: implement these operations
instance OTOperation JSONOperation where
  transform (ListInsert path1 index1 value1) (ListInsert path2 index2 value2) = undefined
