{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiWayIf #-}
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
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import qualified Data.Aeson as A


invertOperation = undefined

j x = JSONOperation [x]

-- * Functions for doing the N^2 operation of transforming two JSONOperations by rebasing the
-- individual operations on each other. Note that this is inefficient at the moment, because it
-- throws away results from transform' that it has to recompute later.
transformRightOp :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformRightOp (JSONOperation [rightOp]) (JSONOperation [leftOp]) = snd <$> transform' leftOp rightOp

transformRightOnes :: [JSONOp] -> [JSONOp] -> Either String JSONOperation
transformRightOnes leftOps rightOps = (normalize . mconcat) <$> (sequence [foldM transformRightOp (j rightOp) (fmap j leftOps) | rightOp <- rightOps])

transformLeftOp :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformLeftOp (JSONOperation [leftOp]) (JSONOperation [rightOp]) = fst <$> transform' leftOp rightOp

transformLeftOnes :: [JSONOp] -> [JSONOp] -> Either String JSONOperation
transformLeftOnes leftOps rightOps = (normalize . mconcat) <$> (sequence [foldM transformLeftOp (j leftOp) (fmap j rightOps) | leftOp <- leftOps])


-- * Normalize a JSONOperation
-- For now, this will just filter out identity ops
-- It could also potentially merge composable things
normalize :: JSONOperation -> JSONOperation
normalize (JSONOperation ops) = JSONOperation $ filter (/= Identity) ops


instance Monoid JSONOperation where
  mempty = JSONOperation []
  (JSONOperation ops1) `mappend` (JSONOperation ops2) = JSONOperation (ops1 ++ ops2)


instance OTOperation JSONOperation where
  transform (JSONOperation [op1]) (JSONOperation [op2]) = transform' op1 op2
  transform (JSONOperation leftOps) (JSONOperation rightOps) = (, ) <$> (transformLeftOnes leftOps rightOps) <*> (transformRightOnes leftOps rightOps)

-- Handle identities up front
transform' :: JSONOp -> JSONOp -> Either String (JSONOperation, JSONOperation)
transform' Identity op = Right (j Identity, j op)
transform' op Identity = Right (j op, j Identity)

-- String operations are deferred to Text0
transform' op1@(isStringOp -> True) op2@(isStringOp -> True) =
  if | path1 == path2 -> case result of
         Left err -> Left err
         Right (textOp1, textOp2) -> Right (toJSONOperation path1 textOp1, toJSONOperation path2 textOp2)
     | otherwise -> Right (j op1, j op2)
  where
    path1 = getFullPath op1
    path2 = getFullPath op1

    op1' :: Text0Operation = toText0Operation op1
    op2' :: Text0Operation = toText0Operation op2

    result = transform op1' op2'

-- Operations that both affect each other
transform' x y | x `affects` y && y `affects` x = (\(x, y) -> (j x, j y)) <$> transformDouble x y

-- Operations where the left affects the right
-- since `x` is unaffected by `y`, `x'` is just `x`
transform' x y | x `affects` y = ((j x, ) . j) <$> (transformRight x y)

-- Operations where the right affects the left
-- since `y` is unaffected by `x`, `y'` is just `y`
transform' x y | y `affects` x = ((, j y) . j) <$> (transformRight y x)

-- Operations that don't affect each other
transform' x y = Right (j x, j y)


instance OTComposableOperation JSONOperation where
  compose = C.compose

instance OTSystem A.Value JSONOperation where
  apply (JSONOperation ops) val = foldM (flip AP.apply) val ops
