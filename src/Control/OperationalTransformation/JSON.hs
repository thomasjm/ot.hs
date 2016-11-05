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
import Control.OperationalTransformation.JSON.QuasiQuote
import Control.OperationalTransformation.JSON.Transform (transformDouble, transformRight)
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import qualified Data.Aeson as A


document = [v|[{}, null, "z"]|]
getOps (JSONOperation ops) = ops
ops1 = getOps [l|[{"p":[2],"li":"aa"},{"p":[2,0],"si":"bbb"}]|]
ops2 = getOps [l|[{"p":[2,0],"si":"cccc"}]|]
o1 = JSONOperation [StringInsert [Pos 2] 0 "bbb"]
o2 = JSONOperation [StringInsert [Pos 3] 0 "cccc"]


invertOperation = undefined

j x = JSONOperation [x]

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
  transform (JSONOperation ops1) (JSONOperation ops2) = (\(x, y) -> (JSONOperation x, JSONOperation y)) <$> transformList2 ops1 ops2


-- TODO: unify this transform logic with Text0, where it's identical
transformList1 :: JSONOp -> [JSONOp] -> Either String ([JSONOp], [JSONOp])
transformList1 o [] = return ([o], [])
transformList1 o (p:ps) = do
  (JSONOperation o', JSONOperation p') <- transform (JSONOperation [o]) (JSONOperation [p])
  (o'', ps') <- transformList2 o' ps
  return (o'', p' ++ ps')

transformList2 :: [JSONOp] -> [JSONOp] -> Either String ([JSONOp], [JSONOp])
transformList2 [] ps = return ([], ps)
transformList2 (o:os) ps = do
  (o', ps') <- transformList1 o ps
  (os', ps'') <- transformList2 os ps'
  return (o' ++ os', ps'')


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
    path2 = getFullPath op2

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
