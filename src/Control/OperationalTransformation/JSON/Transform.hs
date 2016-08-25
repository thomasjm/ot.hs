{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Transform where


import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.QuasiQuote (j)
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Data.List
import Data.String.Interpolate.IsString

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

-- String inserts and deletes can only affect each other
affects (StringInsert path1 pos1 s1) (StringInsert path2 pos2 s2) = pos1 <= pos2
affects (StringInsert {}) _ = False
affects (StringDelete path1 pos1 s1) (StringDelete path2 pos2 s2) = pos1 <= pos2
affects (StringDelete {}) _ = False

-- Subtype operations could technically do anything...
affects (ApplySubtypeOperation path1 _ _) (getPath -> path2) = path1 `isPrefixOf` path2

affects _ _ = False


op1 = parseOp [j|{p:[0],ld:2}|]
op2 = parseOp [j|{p:[0],li:1}|]


-- |In transformRight, the left operation affects the right operation.
-- So, transform the right operation properly and return it.
transformRight :: JSONOperation -> JSONOperation -> Either String JSONOperation
transformRight op1@(ListInsert listPath _ val) op2
  | True = Right $ setPath path' op2 where
      (beginning, rest) = splitAt ((length listPath) + 1) (getPath op2)
      listPos@(Pos x) = last beginning
      path' = (init beginning) ++ [inc listPos] ++ rest

-- TODO: be able to distinguish <= from ==
transformRight op1@(ListDelete listPath _ val) op2
  | True = Right $ setPath path' op2 where
      (beginning, rest) = splitAt ((length listPath) + 1) (getPath op2)
      listPos@(Pos x) = last beginning
      path' = (init beginning) ++ [dec listPos] ++ rest
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

transformDouble x y = Left [i|Not handled: #{x} and #{y}|]
