{-# LANGUAGE TupleSections, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Transform where


import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.Affects
import qualified Control.OperationalTransformation.JSON.Apply as Ap
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import Data.List
import Data.String.Interpolate.IsString
import qualified Data.Text as T


invertOperation = error "invertOperation not implemented"

op1 = parseOp [j|{p:["He"],oi:{}}|]
op2 = parseOp [j|{p:[],od:{},oi:"the"}|]
foo = affects -- Just to avoid warning that the import is unused

----------------------------------------------------------------------------------
-- Transform right
-- In transformRight, the left operation affects the right operation.
-- So, transform the right operation properly and return it
----------------------------------------------------------------------------------

-- ListInsert: bump up the index on the right operation
-- TODO: using init here is hideous, find another way
transformRight :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformRight op1@(ListInsert listPath _ val) op2 = Right $ setFullPath path' op2 where
  (beginning, rest) = splitAt ((length $ getFullPath op1) + 1) (getFullPath op2)
  listPos@(Pos x) = last beginning
  path' = (init beginning) ++ [inc listPos] ++ rest

-- ListDelete/ListInsert at same index: ListInsert is unchanged
transformRight op1@(ListDelete path1 index1 val1) op2@(ListInsert path2 index2 val2)
  | getFullPath op1 == getFullPath op2 = Right op2
-- ListDelete/Anything
transformRight op1@(ListDelete listPath i1 val) op2@(((\x -> x !! (length listPath)) . getFullPath) -> Pos i2)
  = if | i1 == i2 -> Right Identity
       | i1 < i2 -> Right $ replaceIndex op2 (length listPath) (i2 - 1)
       | True -> Right op2

transformRight op1@(StringInsert path i str) op2 = Right $ setPath path' op2 where
  (beginning, rest) = splitAt ((length path) + 1) (getPath op2)
  prop@(Prop x) = last beginning
  (pre, post) = T.splitAt i x
  prop' = Prop $ pre `T.append` str `T.append` post
  path' = (init beginning) ++ [prop'] ++ rest

-- An operation that affects a replace or delete means we need to change what's removed
transformRight op1 op2@(ObjectReplace path key old new) =
  (\old' -> ObjectReplace path key old' new) <$> (Ap.apply (setPath (drop (length path) (getPath op1)) op1) old)
transformRight op1 op2@(ObjectDelete path key old) =
  (\old' -> ObjectDelete path key old') <$> (Ap.apply (setPath (drop (length path) (getPath op1)) op1) old)

-- An object delete or replace turns the other operation into a no-op
transformRight op1@(ObjectDelete {}) op2 = Right Identity
transformRight op1@(ObjectReplace {}) op2 = Right Identity

-- ListReplace/Anything: a list replace on the same index turns the other thing into a no-op
transformRight op1@(ListReplace path1 index1 _ _) (getPath -> path2) | (getFullPath op1) `isPrefixOf` path2
  = Right Identity

-- ListMove/ListInsert on same index when the ListMove moves it to earlier: the ListInsert gets bumped up by 1
transformRight op1@(ListMove path1 index11 index12) op2@(ListInsert path2 index2 _)
  | path1 == path2 && index11 == index2 && index12 < index11 = Right $ replaceIndex op2 (length path1) (index2 + 1)
-- ListMove/Anything
transformRight (ListMove listPath1 listIndex1 listIndex2) op2@(((\x -> x !! (length listPath1)) . getFullPath) -> Pos i) | i == listIndex1 = Right $ replaceIndex op2 (length listPath1) listIndex2
transformRight (ListMove listPath1 listIndex1 listIndex2) op2@(((\x -> x !! (length listPath1)) . getFullPath) -> Pos i)
  | i > listIndex1 && i <= listIndex2 = Right $ replaceIndex op2 (length listPath1) (i - 1)


transformRight x y = Left [i|transformRight not handled: #{x} affecting #{y}|]

replaceIndex obj at newIndex = setFullPath path' obj where
  path = getFullPath obj
  path' = (take at path) ++ [Pos newIndex] ++ (drop (at + 1) path)

----------------------------------------------------------------------------------
--- Transform double
----------------------------------------------------------------------------------

-- |In transformDouble, both operations affect the other
transformDouble :: JSONOperation -> JSONOperation -> Either String (JSONOperation, JSONOperation)
transformDouble op1@(ListInsert path1 i1 value1) op2@(ListInsert path2 i2 value2)
  | (path2 == path1) && (i1 > i2) = error "Problem with transformDouble ListInsert/ListInsert" -- TODO rev <$> transform op2 op1 -- WLOG
  | (path2 == path1) && (i1 <= i2) = Right (op1, ListInsert path2 (succ i2) value2)
  | (path2 `isPrefixOf` path1) = undefined -- TODO rev <$> transform op2 op1 -- WLOG
  | (path1 `isPrefixOf` path2) = Right (op1, op2) -- TODO: increment the appropriate part of path2
  | otherwise = Right (op1, op2)

-- For dueling subtype operations, defer to the operation's transform function
transformDouble (ApplySubtypeOperation path1 typ1 op1) (ApplySubtypeOperation path2 typ2 op2) = case (C.transform op1 op2) of
  Left err -> Left err
  Right (T0 [], T0 []) -> Right (Identity, Identity)
  Right (T0 [], op2') -> Right (Identity, ApplySubtypeOperation path2 typ2 op2')
  Right (op1', T0 []) -> Right (ApplySubtypeOperation path1 typ1 op1', Identity)
  Right (op1', op2') -> Right (ApplySubtypeOperation path1 typ1 op1', ApplySubtypeOperation path2 typ2 op2')

transformDouble op1@(ListDelete path1 i1 value1) op2@(ListDelete path2 i2 value2)
  | op1 /= op2 = error "Fatal: operations do not both affect each other"
  | otherwise = Right (Identity, Identity)

transformDouble op1@(StringInsert {}) op2@(ListDelete {}) = rev <$> transformDouble op2 op1
-- We also assume `value` must be a string; we should probably assert that(?)
transformDouble op1@(ListDelete path1 i value) op2@(StringInsert {})
  = (\v -> (ListDelete path1 i v, Identity)) <$> Ap.apply op2' value -- TODO: lens this up
  where
    -- Here `'` does not mean it's a part of the output of `transform`; it's just a modified `op2`
    -- We use `[]` for the path because we're applying the operation to `value`, not to the
    -- entire JSON structure
    op2' = setPath [] op2

transformDouble op1@(StringDelete {}) op2@(ListDelete {}) = rev <$> transformDouble op2 op1
-- We also assume `value` must be a string; we should probably assert that(?)
transformDouble op1@(ListDelete path1 i value) op2@(StringDelete {})
  = (\v -> (ListDelete path1 i v, Identity)) <$> Ap.apply op2' value -- TODO: lens this up
  where
    -- Here `'` does not mean it's a part of the output of `transform`; it's just a modified `op2`
    -- We use `[]` for the path because we're applying the operation to `value`, not to the
    -- entire JSON structure
    op2' = setPath [] op2

transformDouble op1@(ApplySubtypeOperation {}) op2@(ListDelete {}) = rev <$> transformDouble op2 op1
transformDouble op1@(ListDelete path1 i1 value1) op2@(ApplySubtypeOperation {})
  = (\v -> (ListDelete path1 i1 v, Identity)) <$> Ap.apply op2' value1
  where
    -- Here `'` does not mean it's a part of the output of `transform`; it's just a modified `op2`
    -- We use `[]` for the path because we're applying the operation to `value`, not to the
    -- entire JSON structure
    op2' = setPath [] op2

-- we know both operations affect each other, and our operations are both
-- `ObjectDelete`s, so we know `path1 == path2`
transformDouble op1@(ObjectDelete _ key1 _) op2@(ObjectDelete _ key2 _)
  | op1 == op2 = Right (Identity, Identity)
  | key1 == key1 = Right (Identity, Identity) -- inconsistent state, so we behave forgivingly
  | otherwise = Right (op1, op2) -- deleting different keys; should just do those ops


-- On simultaneous inserts, the left insert wins
transformDouble op1@(ObjectInsert path1 key1 value1) op2@(ObjectInsert path2 key2 value2) |
  path1 == path2 && key1 == key2 = Right (ObjectReplace path1 key1 value2 value1, Identity)

transformDouble sd1@(StringDelete {}) sd2@(StringDelete {}) |
  sd1 == sd2 = Right (Identity, Identity)

transformDouble op1@(ObjectReplace {}) op2@(ObjectDelete {}) = rev <$> transformDouble op2 op1
transformDouble op1@(ObjectDelete path1 key1 value1) op2@(ObjectReplace path2 key2 old2 new2)
  | value1 == old2 = Right (Identity, ObjectInsert path1 key1 new2)
  | otherwise = error "unhandled so far :/"

transformDouble op1 op2@(ObjectDelete {}) = rev <$> transformDouble op2 op1
transformDouble op1@(ObjectDelete path key value) op2
  = (\v -> (ObjectDelete path key v, Identity)) <$> Ap.apply op2' value -- TODO: lens this up
  where
    -- Here `'` does not mean it's a part of the output of `transform`; it's just a modified `op2`
    -- We use `[]` for the path because we're applying the operation to `value`, not to the
    -- entire JSON structure
    op2' = setPath [] op2


-- The right default behavior for transformDouble is to transform the two sides independently,
-- for the cases where the transformations don't depend on each other
transformDouble x y = (, ) <$> transformRight y x <*> transformRight x y
