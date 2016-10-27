{-# LANGUAGE TupleSections, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Transform where


import qualified Control.OperationalTransformation as C
import Control.OperationalTransformation.JSON.Affects
import qualified Control.OperationalTransformation.JSON.Apply as Ap
import Control.OperationalTransformation.JSON.QuasiQuote
import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import Data.List
import Data.String.Interpolate.IsString


invertOperation = error "invertOperation not implemented"

op1 = [s|{"p":[0],"ld":"x","li":"y"}|]
op2 = [s|{"p":[0],"ld":"x"}|]
foo = affects -- Just to avoid warning that the import is unused

----------------------------------------------------------------------------------
-- Transform right
-- In transformRight, the left operation affects the right operation.
-- So, transform the right operation properly and return it
----------------------------------------------------------------------------------


transformRight :: JSONOp -> JSONOp -> Either String JSONOp

-- ListDelete/ListMove
transformRight op1@(ListDelete {}) op2@(ListMove path2 index21 index22) =
  if -- Delete of the thing being moved makes the move a no-op
     | index1 == index21 -> Right Identity
     -- Delete in the middle of the range causes the top index to go down
     | index21 <= index1 && index1 <= index22 -> Right $ ListMove path2 index21 (index22 - 1)
     | index22 <= index1 && index1 <= index21 -> Right $ ListMove path2 (index21 - 1) index22
     -- Delete before the range causes both indices to go down
     | index1 < bottom -> Right $ ListMove path2 (index21 - 1) (index22 - 1)
     -- Otherwise, no change
     | otherwise -> Right op2
  where bottom = min index21 index22
        top = max index21 index22
        Pos index1 = (getFullPath op1) !! length path2

-- ListInsert/ListMove
transformRight op1@(ListInsert path1 index1 _) op2@(ListMove path2 index21 index22) =
  if -- Insert in the middle of the range causes the top index to go up
     | path1 == path2 && bottom < index1 && index1 <= top && index22 > index21 -> Right $ ListMove path2 index21 (index22 + 1)
     | path1 == path2 && bottom < index1 && index1 <= top && index22 < index21 -> Right $ ListMove path2 (index21 + 1) index22
     -- Insert before the range causes both indices to go up
     | path1 == path2 && index1 <= bottom -> Right $ ListMove path2 (index21 + 1) (index22 + 1)
     -- Otherwise, no change
     | otherwise -> Right op2
  where bottom = min index21 index22
        top = max index21 index22

-- ListMove/ListInsert
transformRight op1@(ListMove path1 index11 index12) op2@(ListInsert path2 index2 _)
  -- on same index when the ListMove moves it to earlier: the ListInsert gets bumped up by 1
  | path1 == path2 && index11 == index2 && index12 < index11 = Right $ replaceIndex op2 (length path1) (index2 + 1)
  -- in between
  | path1 == path2 && index11 < index2 && index2 <= index12 = Right $ replaceIndex op2 (length path1) (index2 - 1)
  | path1 == path2 && index12 < index2 && index2 <= index11 = Right $ replaceIndex op2 (length path1) (index2 + 1)
  -- If the ListInsert is at or before the smaller index of the ListMove, it's not affected. TODO: cover this in `affects`
  | path1 == path2 && index2 <= (min index11 index12) = Right op2
  -- If the ListInsert is at or after the larger index of the ListMove, it's not affected. TODO: cover this in `affects`
  | path1 == path2 && index2 >= (max index11 index12) = Right op2
-- ListMove/Anything
transformRight (ListMove listPath1 listIndex1 listIndex2) op2@(((\x -> x !! (length listPath1)) . getFullPath) -> Pos i)
  | i == listIndex1 = Right $ replaceIndex op2 (length listPath1) listIndex2
  -- in between
  | listIndex1 <= i && i <= listIndex2 = Right $ replaceIndex op2 (length listPath1) (i - 1)
  | listIndex2 <= i && i <= listIndex1 = Right $ replaceIndex op2 (length listPath1) (i + 1)
  | otherwise = Right op2

-- ListDelete/ListReplace: a delete affecting a replace turns into an insert. TODO: what if the delete is inside the replace?
transformRight op1@(ListDelete path1 index1 value1) op2@(ListReplace path2 index2 old new)
  | path1 == path2 && index1 == index2 = Right $ ListInsert path2 index2 new

-- ListInsert/Anything
transformRight op1@(ListInsert listPath _ val) op2 = Right $ replaceIndexFn op2 (length listPath) (+ 1)

-- ListDelete/ListInsert at same index: ListInsert is unchanged
transformRight op1@(ListDelete path1 index1 val1) op2@(ListInsert path2 index2 val2)
  | getFullPath op1 == getFullPath op2 = Right op2
-- ListDelete/Anything
transformRight op1@(ListDelete listPath i1 val) op2@(((\x -> x !! (length listPath)) . getFullPath) -> Pos i2)
  = if | i1 == i2 -> Right Identity
       | i1 < i2 -> Right $ replaceIndex op2 (length listPath) (i2 - 1)
       | True -> Right op2


-- ListReplace/ListDelete: a list replace on the same index as a list delete turns into a list insert
transformRight op1@(ListReplace path1 index1 _ _) op2@(ListDelete path2 index2 item)
  | (getFullPath op1) == (getFullPath op2) = Right $ ListInsert path2 index2 item
-- ListReplace/Anything: a list replace on the same index turns the other thing into a no-op
transformRight op1@(ListReplace path1 index1 _ _) (getFullPath -> fullPath2)
  | (getFullPath op1) `isPrefixOf` fullPath2 = Right Identity

-- A delete affecting a replace turns the replace into an insert
transformRight op1@(ObjectDelete path1 key1 value1) op2@(ObjectReplace path2 key2 old2 new2)
  | getFullPath op1 == getFullPath op2
  , value1 == old2 = Right $ ObjectInsert path1 key1 new2
  | otherwise = error "unhandled so far :/"
-- Otherwise, an operation that affects a replace or delete means we need to change what's removed
transformRight op1 op2@(ObjectReplace path key old new) =
  (\old' -> ObjectReplace path key old' new) <$> (Ap.apply (setPath (drop (length $ getFullPath op2) (getPath op1)) op1) old)
transformRight op1 op2@(ObjectDelete path key old) =
  (\old' -> ObjectDelete path key old') <$> (Ap.apply (setPath (drop (length $ getFullPath op2) (getPath op1)) op1) old)
transformRight op1 op2@(ListDelete path i value) =
  (\value' -> ListDelete path i value') <$> Ap.apply (setPath (drop (length $ getFullPath op2) (getPath op1)) op1) value

-- A delete or replace turns the other operation into a no-op
transformRight op1@(ObjectDelete {}) op2 = Right Identity
transformRight op1@(ObjectReplace {}) op2 = Right Identity
transformRight op1@(ListDelete {}) op2 = Right Identity

transformRight x y = Left [i|transformRight not handled: #{x} affecting #{y}|]

----------------------------------------------------------------------------------
--- Transform double
----------------------------------------------------------------------------------

-- |In transformDouble, both operations affect the other
transformDouble :: JSONOp -> JSONOp -> Either String (JSONOp, JSONOp)
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

-- ObjectDelete/ObjectDelete: we know `path1 == path2`
transformDouble op1@(ObjectDelete _ key1 _) op2@(ObjectDelete _ key2 _)
  | key1 == key1 = Right (Identity, Identity)
  | otherwise = Right (op1, op2) -- deleting different keys; should just do those ops

-- On simultaneous inserts, the left one wins
transformDouble op1@(ObjectInsert path1 key1 value1) op2@(ObjectInsert path2 key2 value2) |
  path1 == path2 && key1 == key2 = Right (ObjectReplace path1 key1 value2 value1, Identity)

-- On simultaneous list replaces, the left one wins
transformDouble op1@(ListReplace path1 key1 old1 new1) op2@(ListReplace path2 key2 old2 new2)
  | (path1 == path2) && (key1 == key2) = Right (ListReplace path1 key1 new2 new1, Identity)

transformDouble sd1@(StringDelete {}) sd2@(StringDelete {}) |
  sd1 == sd2 = Right (Identity, Identity)

-- ListMove/ListMove: fall back to special logic
transformDouble op1@(ListMove path1 otherFrom otherTo) op2@(ListMove path2 from to) | path1 == path2
  = Right (transformListMove LeftSide op2 op1, transformListMove RightSide op1 op2)

-- The right default behavior for transformDouble is to transform the two sides independently,
-- for the cases where the transformations don't depend on each other
transformDouble x y = (, ) <$> transformRight y x <*> transformRight x y










------------------------------------------------------------------------------------------------------

data Side = LeftSide | RightSide deriving (Show, Eq)

-- ListMove/ListMove, where we're transforming the right one
-- Made by directly copying the logic in json0.js
transformListMove side op1@(ListMove path1 otherFrom otherTo) op2@(ListMove path2 from to) | otherFrom == otherTo = op2

-- Where did my thing go? Someone already moved it and we're the right: tiebreak to a no-op
transformListMove side (ListMove path1 otherFrom otherTo) (ListMove path2 from to) | ((from == otherFrom) && (side == RightSide)) = Identity

-- Where did my thing go? Someone already moved it and we're the left: tiebreak to a valid op
transformListMove side (ListMove path1 otherFrom otherTo) (ListMove path2 from to) | ((from == otherFrom) && (side == LeftSide) && (from == to)) = ListMove path2 otherTo otherTo -- Ugh special case
transformListMove side (ListMove path1 otherFrom otherTo) (ListMove path2 from to) | ((from == otherFrom) && (side == LeftSide)) = ListMove path2 otherTo to

-- Mimic the imperative JS code from json0.js exactly
-- (This was too tricky to do otherwise)
-- TODO: use ST monad or something to make this less error-prone
transformListMove side (ListMove path1 otherFrom otherTo) (ListMove path2 from to) = ListMove path2 newFrom newTo where
  newFrom' = if (from > otherFrom) then from - 1 else from
  newFrom'' = if (from > otherTo) then newFrom' + 1 else newFrom'
  newFrom''' = if ((from == otherTo) && (otherFrom > otherTo)) then newFrom'' + 1 else newFrom''
  newFrom = newFrom'''

  newTo1 = if ((from == otherTo) && (otherFrom > otherTo) && (from == to)) then to + 1 else to
  newTo2 = if (to > otherFrom) then newTo1 - 1 else newTo1
  newTo3 = if ((to == otherFrom) && (to > from)) then newTo2 - 1 else newTo2
  newTo4 = if (to > otherTo) then newTo3 + 1 else newTo3
  newTo5 = if ((to == otherTo) && ((otherTo > otherFrom && to > from) || (otherTo < otherFrom && to < from)) && (side == RightSide)) then newTo4 + 1 else newTo4
  newTo6 = if ((to == otherTo) && (not ((otherTo > otherFrom && to > from) || (otherTo < otherFrom && to < from))) && (to > from)) then newTo5 + 1 else newTo5
  newTo7 = if ((to == otherTo) && (not ((otherTo > otherFrom && to > from) || (otherTo < otherFrom && to < from))) && (not (to > from)) && (to == otherFrom)) then newTo6 - 1 else newTo6
  newTo = newTo7

transformListMove side op1 op2 = error [i|Invalid arguments to transformListMove: #{side}, #{op1}, #{op2}|]
