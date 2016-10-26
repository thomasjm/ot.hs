{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TupleSections, ViewPatterns, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where


import Control.Monad
import Control.OperationalTransformation
import Control.OperationalTransformation.JSON.Affects (affects)
import qualified Control.OperationalTransformation.JSON.Apply as AP
import qualified Control.OperationalTransformation.JSON.Compose as C
import Control.OperationalTransformation.JSON.Transform (transformDouble, transformRight)
import Control.OperationalTransformation.JSON.Types
import qualified Data.Aeson as A


invertOperation = undefined


-- * Functions for doing the N^2 operation of transforming two JSONOperations by rebasing the
-- individual operations on each other. Note that this is inefficient at the moment, because it
-- throws away results from transform' that it has to recompute later.
transformRightOp :: JSONOp -> JSONOp -> Either String JSONOp
transformRightOp rightOp leftOp = snd <$> transform' leftOp rightOp
transformRightOnes :: [JSONOp] -> [JSONOp] -> Either String JSONOperation
transformRightOnes leftOps rightOps = (normalize . JSONOperation) <$> (sequence [foldM transformRightOp rightOp leftOps | rightOp <- rightOps])
transformLeftOp :: JSONOp -> JSONOp -> Either String JSONOp
transformLeftOp leftOp rightOp = fst <$> transform' leftOp rightOp
transformLeftOnes :: [JSONOp] -> [JSONOp] -> Either String JSONOperation
transformLeftOnes leftOps rightOps = (normalize . JSONOperation) <$> (sequence [foldM transformLeftOp leftOp rightOps | leftOp <- leftOps])


-- * Normalize a JSONOperation
-- For now, this will just filter out identity ops
-- It could also potentially merge composable things
normalize :: JSONOperation -> JSONOperation
normalize (JSONOperation ops) = JSONOperation $ filter (/= Identity) ops


instance OTOperation JSONOperation where
  transform (JSONOperation [op1]) (JSONOperation [op2]) = case transform' op1 op2 of
    Left s -> Left s
    Right (op1, op2) -> Right (JSONOperation [op1], JSONOperation [op2])
  transform (JSONOperation leftOps) (JSONOperation rightOps) = (, ) <$> (transformLeftOnes leftOps rightOps) <*> (transformRightOnes leftOps rightOps)

-- Handle identities up front
transform' Identity op = Right (Identity, op)
transform' op Identity = Right (op, Identity)

-- Operations that both affect each other
transform' x y | x `affects` y && y `affects` x = transformDouble x y

-- Operations where the left affects the right
-- since `x` is unaffected by `y`, `x'` is just `x`
transform' x y | x `affects` y = (x, ) <$> (transformRight x y)

-- Operations where the right affects the left
-- since `y` is unaffected by `x`, `y'` is just `y`
transform' x y | y `affects` x = (, y) <$> (transformRight y x)

-- Operations that don't affect each other
transform' x y = Right (x, y)


instance OTComposableOperation JSONOperation where
  compose = C.compose

instance OTSystem A.Value JSONOperation where
  apply (JSONOperation ops) val = foldM (flip AP.apply) val ops
