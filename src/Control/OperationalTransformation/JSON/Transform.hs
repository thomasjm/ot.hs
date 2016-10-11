{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Transform where


import qualified Control.OperationalTransformation as C
import qualified Control.OperationalTransformation.JSON.Apply as Ap
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import Data.List
import Data.String.Interpolate.IsString
import qualified Data.Text as T


invertOperation = error "invertOperation not implemented"

-- | Helper function to deal with list operations.
-- Called when we know that (listPath `isPrefixOf` (getPath op)), so op operates on either
-- a) an element in the list pointed to by listPath, or
-- b) a child of such an element
-- In either case, returns the index in the list pointed to by listPath
getIndexInList listPath op | (length listPath) < (length path) = unPos (path !! (length listPath))
  where path = getPath op
getIndexInList listPath (ListInsert path pos value) | (length listPath) == (length path) = pos
getIndexInList listPath (ListDelete path pos value) | (length listPath) == (length path) = pos
getIndexInList listPath (ListReplace path pos old new) | (length listPath) == (length path) = pos
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


-- If an operation operates on the same index as a list insert, it does *not* affect the list insert
-- The exception to this is another list insert, in which case we must break ties
affects op1@(getFullPath -> path1) (ListInsert path2 index2 _) | (not $ isListInsert op1)
                                                               , (path2 ++ [Pos index2]) `isPrefixOf` path1 = False

-- ListInsert/ListDelete
affects op1@(ListInsert path1 index1 val1) (ListDelete path2 index2 val2) | path1 == path2 = index1 <= index2
-- ListInsert/Anything
affects (ListInsert listPath listIndex value) op | path <- getPath op
                                                 , listPath `isPrefixOf` path
                                                 , index <- getIndexInList listPath op = index >= listIndex

-- ListDelete/Anything
affects (ListDelete listPath listIndex value) op | path <- getPath op
                                                 , listPath `isPrefixOf` path
                                                 , index <- getIndexInList listPath op = index >= listIndex

-- ListReplace/Anything
affects (ListReplace listPath listIndex old new) op | path <- getPath op
                                                    , listPath `isPrefixOf` path
                                                    = listIndex == getIndexInList listPath op

-- ListMove/ListMove (operating on same list)
affects (ListMove listPath1 listIndex11 listIndex12) (ListMove listPath2 listIndex21 listIndex22)
  | listPath1 == listPath2 = (inRange listIndex21 || inRange listIndex22)
  where inRange x = (x >= listIndex11) || (x <= listIndex22)
-- ListMove/Anything
affects (ListMove listPath listIndex1 listIndex2) op | path <- getPath op
                                                     , listPath `isPrefixOf` path
                                                     , index <- getIndexInList listPath op
                                                     = listIndex1 <= index && index <= listIndex2

-- Objects are simpler

-- Parallel object operations only affect each other if they touch the same object
affects (getObjectPathAndKey -> Just (path1, key1)) (getObjectPathAndKey -> Just (path2, key2)) = path1 == path2 && key1 == key2

-- Object*/Anything: other operations are only affected if the path is a prefix
affects (ObjectInsert path1 key val) (getPath -> path2) = path1 `isPrefixOf` path2
affects (ObjectDelete path1 (Just key) val) (getPath -> path2) = (path1 ++ [Prop key]) `isPrefixOf` path2
affects (ObjectDelete path1 Nothing val) (getPath -> path2) = path1 `isPrefixOf` path2
affects (ObjectReplace path1 (Just key) old new) (getPath -> path2) = (path1 ++ [Prop key]) `isPrefixOf` path2
affects (ObjectReplace path1 Nothing old new) (getPath -> path2) = path1 `isPrefixOf` path2

-- String*/String*
affects (StringInsert path1 pos1 s1) (StringInsert path2 pos2 s2) = pos1 <= pos2
affects (StringDelete path1 pos1 s1) (StringDelete path2 pos2 s2) = pos1 <= pos2

-- A string or subtype operation affects a list delete only if it touches the string being deleted
-- TODO: collapse this into a single case
affects (StringInsert path1 pos1 _) op2@(ListDelete path2 pos2 _) = path1 == getFullPath op2
affects (StringDelete path1 pos1 _) op2@(ListDelete path2 pos2 _) = path1 == getFullPath op2
affects (ApplySubtypeOperation path1 _ _) (ListDelete path2 pos2 _) = path1 == (path2 ++ [Pos pos2])

-- A string or subtype operation affects an object delete only if it touches the string being deleted
affects (StringInsert path1 pos1 _) op2@(ObjectDelete path2 (Just prop2) _) = path1 == getFullPath op2
affects (StringDelete path1 pos1 _) op2@(ObjectDelete path2 (Just prop2) _) = path1 == getFullPath op2
affects (ApplySubtypeOperation path1 _ _) op2@(ObjectDelete path2 (Just prop2) _) = path1 == getFullPath op2


-- Object delete or replace takes precedence over subtype ops
affects (ApplySubtypeOperation path1 _ _) (ObjectDelete path2 key2 _) | path1 == path2 = False
affects (ApplySubtypeOperation path1 _ _) (ObjectReplace path2 key2 _ _) | path1 == path2 = False

-- Subtype operations could technically do anything...
affects (ApplySubtypeOperation path1 _ _) (getPath -> path2) = path1 `isPrefixOf` path2

affects _ _ = False

op1 = parseOp [j|{p:[1, 0], si:"hi"}|]
op2 = parseOp [j|{p:[1], ld:"x"}|]

-- | In transformRight, the left operation affects the right operation.
-- So, transform the right operation properly and return it

-- ListInsert: bump up the index on the right operation
-- TODO: using init here is hideous, find another way
transformRight :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformRight op1@(ListInsert listPath _ val) op2 = Right $ setFullPath path' op2 where
  (beginning, rest) = splitAt ((length $ getFullPath op1) + 1) (getFullPath op2)
  listPos@(Pos x) = last beginning
  path' = (init beginning) ++ [inc listPos] ++ rest

-- TODO: be able to distinguish <= from ==
-- TODO: unify these two ListDelete rules. The second one uses "last" unsafely
transformRight op1@(ListDelete path1 index1 val1) op2@(ListInsert path2 index2 val2)
  | getFullPath op1 == getFullPath op2 = Right op2
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

transformRight op1@(StringInsert path i str) op2 = Right $ setPath path' op2 where
  (beginning, rest) = splitAt ((length path) + 1) (getPath op2)
  prop@(Prop x) = last beginning
  (pre, post) = T.splitAt i x
  prop' = Prop $ pre `T.append` str `T.append` post
  path' = (init beginning) ++ [prop'] ++ rest

-- An object delete or replace turns the other operation into a no-op
transformRight op1@(ObjectDelete {}) op2 = Right Identity
transformRight op1@(ObjectReplace {}) op2 = Right Identity

-- A list replace on the same index turns the other thing into a no-op
transformRight op1@(ListReplace path1 index1 _ _) (getPath -> path2) | (path1 ++ [Pos index1]) `isPrefixOf` path2
  = Right Identity
transformRight x y = Left [i|transformRight not handled: #{x} affecting #{y}|]

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

-- The right default behavior for transformDouble is to transform the two sides independently,
-- for the cases where the transformations don't depend on each other
transformDouble x y = (, ) <$> transformRight y x <*> transformRight x y
