{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Transform where


import qualified Control.OperationalTransformation as C
import qualified Control.OperationalTransformation.JSON.Apply as Ap
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Data.List
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import Debug.Trace

invertOperation = undefined

-- | Helper function to deal with list operations.
-- Called when we know that (listPath `isPrefixOf` (getPath op)), so op operates on either
-- a) an element in the list pointed to by listPath, or
-- b) a child of such an element
-- In either case, returns the index in the list pointed to be listPath
getIndexInList listPath op | (length listPath) < (length path) = unPos (path !! (length listPath))
  where path = getPath op
getIndexInList listPath (ListInsert path pos value) | (length listPath) == (length path) = pos
getIndexInList listPath (ListDelete path pos value) | (length listPath) == (length path) = pos
getIndexInList listPath (ListReplace path pos old new) | (length listPath) == (length path) = pos
getIndexInList listPath (ListMove path pos1 pos2) = error "ListMove is tricky here..."
getIndexInList x y = error [i|Failed to getIndexInList: #{show x}, #{show y}|]

-- Define x `affects` y if operation x affects operation y
-- Rationale:
-- All list inserts that affect another operation do so in the same way:
-- the affected element needs to have a number in its path bumped up. But the
-- affected element never affects the list back.
-- The same applies to other list operations like ListDelete
-- Pretty sure this is a well-defined partial ordering.
-- If x `affects` y and y `affects` x, then they must be touching the same thing -- for example, two
-- list inserts in the same location, in which case we should break ties arbitrarily
affects :: JSONOperation -> JSONOperation -> Bool
affects (ListInsert listPath listIndex value) op | path <- getPath op
                                                 , listPath `isPrefixOf` path
                                                 , index <- getIndexInList listPath op = index >= listIndex
affects (ListDelete listPath listIndex value) op | path <- getPath op
                                                 , listPath `isPrefixOf` path
                                                 , index <- getIndexInList listPath op = index >= listIndex
affects (ListReplace listPath listIndex old new) op | path <- getPath op
                                                    , listPath `isPrefixOf` path
                                                    , index <- getIndexInList listPath op = index == listIndex
affects (ListMove listPath listIndex1 listIndex2) op | path <- getPath op
                                                     , listPath `isPrefixOf` path
                                                     , index <- getIndexInList listPath op = listIndex1 <= index && index <= listIndex2

-- Objects are simpler
affects (ObjectInsert path1 key val) (getPath -> path2) = path1 `isPrefixOf` path2
affects (ObjectDelete path1 key val) (getPath -> path2) = path1 `isPrefixOf` path2
affects (ObjectReplace path1 key old new) (getPath -> path2) = path1 `isPrefixOf` path2

affects (StringInsert path1 pos1 s1) (StringInsert path2 pos2 s2) = pos1 <= pos2
affects (StringDelete path1 pos1 s1) (StringDelete path2 pos2 s2) = pos1 <= pos2

-- TODO: some of these are shadowed by earlier pattern matches, I think?
affects Identity (ListDelete path2 _ _) = False
affects op1@(StringInsert {}) (ListDelete path2 _ _)
  = (not . null $ getPath op1) && (init (getPath op1) == path2)
affects op1@(StringDelete {}) (ListDelete path2 _ _)
  = (not . null $ getPath op1) && (init (getPath op1) == path2)
affects op1@(ListInsert {}) (ListDelete path2 _ _) = getPath op1 == path2
affects op1@(ListDelete {}) (ListDelete path2 _ _) = getPath op1 == path2
affects op1@(ApplySubtypeOperation {}) (ListDelete path2 _ _)
  = (not . null $ getPath op1) && (init (getPath op1) == path2)
--affects (Add {}) (ListDelete _ _ _) =
--affects (ObjectInsert {}) (ListDelete _ _ _) =
--affects (ObjectDelete {}) (ListDelete _ _ _) =
--affects (ObjectReplace {}) (ListDelete _ _ _) =
--affects (ListReplace {}) (ListDelete _ _ _) =
--affects (ListMove {}) (ListDelete _ _ _) =

affects (StringInsert {}) _ = False
affects (StringDelete {}) _ = False

-- Subtype operations could technically do anything...
affects (ApplySubtypeOperation path1 _ _) (getPath -> path2) = path1 `isPrefixOf` path2

affects _ _ = False

op1 = parseOp [j|{p:[0],ld:2}|]
op2 = parseOp [j|{p:[0],li:1}|]

-- | In transformRight, the left operation affects the right operation.
-- So, transform the right operation properly and return it
transformRight :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformRight op1@(ListInsert listPath _ val) op2
  | True = Right $ setPath path' op2 where
      (beginning, rest) = splitAt ((length listPath) + 1) (getPath op2)
      listPos@(Pos x) = last beginning
      path' = (init beginning) ++ [inc listPos] ++ rest

-- TODO: be able to distinguish <= from ==
transformRight op1@(ListDelete listPath i val) op2
  | True = if x == i
      then Right Identity -- LD deletes index op2 is trying to do something to; deletion
                          -- takes priority
                          -- TODO: takes priority over ALL other ops? Should do something more
                          -- general here?
      else Right $ setPath path' op2
      where
        (beginning, rest) = splitAt ((length listPath) + 1) (getPath op2)
        listPos@(Pos x) = last beginning
        path' = (init beginning) ++ [dec listPos] ++ rest

transformRight op1@(StringInsert path i str) op2
  | True = Right $ setPath path' op2
    where
      -- this ain't right but it maybe ain't too wrong :o
      (beginning, rest) = splitAt ((length path) + 1) (getPath op2)
      prop@(Prop x) = last beginning
      (pre, post) = T.splitAt i x
      prop' = Prop $ pre `T.append` str `T.append` post
      path' = (init beginning) ++ [prop'] ++ rest

transformRight x y = Left [i|Not handled: #{x} affecting #{y}|]

-- |In transformDouble, both operations affect the other
transformDouble :: JSONOperation -> JSONOperation -> Either String (JSONOperation, JSONOperation)
transformDouble op1@(ListInsert path1 i1 value1) op2@(ListInsert path2 i2 value2)
  | (path2 == path1) && (i1 >  i2) = undefined -- TODO rev <$> transform op2 op1 -- WLOG
  | (path2 == path1) && (i1 <= i2) = Right (op1, ListInsert path2 (succ i2) value2)
  | (path2 `isPrefixOf` path1) = undefined -- TODO rev <$> transform op2 op1 -- WLOG
  | (path1 `isPrefixOf` path2) = Right (op1, op2) -- TODO: increment the appropriate part of path2
  | otherwise = Right (op1, op2)

-- For dueling subtype operations, defer to the operation's transform function
transformDouble (ApplySubtypeOperation path1 typ1 op1) (ApplySubtypeOperation path2 typ2 op2) = case (C.transform op1 op2) of
  Left err -> Left err
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


transformDouble x y = Left [i|Not handled: #{x} and #{y}|]
