{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Affects where

import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Data.List
import Data.String.Interpolate.IsString


-- | Helper function to deal with list operations.
-- Called when we know that (listPath `isPrefixOf` (getFullPath op)), so op operates on either
-- a) an element in the list pointed to by listPath, or
-- b) a child of such an element
-- In either case, returns the index in the list pointed to by listPath
getIndexInList listPath op | (length listPath) < (length path) = unPos (path !! (length listPath))
  where path = getFullPath op
getIndexInList x y = error [i|Failed to getIndexInList (this shouldn't happen): #{show x}, #{show y}|]

-- Define x `affects` y if operation x affects operation y
affects :: JSONOperation -> JSONOperation -> Bool

-- If an operation operates on the same index as a list insert, it does *not* affect the list insert
-- The exception to this is another list insert, in which case we must break ties
-- The other exception to this is a ListMove, which is also tricky
affects op1@(getFullPath -> path1) (ListInsert path2 index2 _) | (not $ isListInsert op1)
                                                               , (not $ isListMove op1)
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
  where inRange x = (x >= (min listIndex11 listIndex12)) && (x <= (max listIndex11 listIndex12))
-- ListMove/Anything
affects (ListMove listPath listIndex1 listIndex2) op | listPath `isPrefixOf` (getFullPath op)
                                                     , index <- getIndexInList listPath op
                                                     = inRange index
  where inRange x = (x >= (min listIndex1 listIndex2)) && (x <= (max listIndex1 listIndex2))

-- * Objects are simpler

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
