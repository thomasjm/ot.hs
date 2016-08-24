{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Control.OperationalTransformation.JSON
  (
  -- * JSON operations
  JSONOperation (..)
  , invertOperation
  ) where


import Control.OperationalTransformation
import Control.OperationalTransformation.JSON.Types
import Data.List

invertOperation = undefined

commonPath :: Path -> Path -> Path
commonPath = undefined


inc :: PathSegment -> PathSegment
inc (Pos x) = Pos (x + 1)
inc (Prop _) = error "Tried to increment a prop"

rev (a, b) = (b, a)

-- TODO: implement these operations
instance OTOperation JSONOperation where
  -- An InsertString can't affect a ListInsert at all
  -- The only way a ListInsert can affect an InsertString is if the InsertString is at a point in the tree where it's
  -- in the list after the edited position (or the child of some such element)
  transform op1@(InsertString path pos s) op2@(ListInsert listPath pos2 val) | ((listPath `isPrefixOf` path) && (x >= pos2)) = Right (InsertString path' pos s, op2) where
                                                                                 (beginning, rest) = splitAt ((length listPath) + 1) path
                                                                                 listPos@(Pos x) = last beginning
                                                                                 path' = (init beginning) ++ [inc listPos] ++ rest
  transform op1@(InsertString {}) op2@(ListInsert listPath pos2 val) = Right (op1, op2)
  -- Reversed
  transform op1@(ListInsert {}) op2@(InsertString {}) = rev <$> transform op2 op1


  -- An ApplySubtypeOperation is just like an insertString
  transform op1@(ApplySubtypeOperation path t op) op2@(ListInsert listPath pos2 val) | ((listPath `isPrefixOf` path) && (x >= pos2)) = Right (ApplySubtypeOperation path' t op, op2) where
                                                                                         (beginning, rest) = splitAt ((length listPath) + 1) path
                                                                                         listPos@(Pos x) = last beginning
                                                                                         path' = (init beginning) ++ [inc listPos] ++ rest
  transform op1@(ApplySubtypeOperation {}) op2@(ListInsert listPath pos2 val) = Right (op1, op2)
  -- Reversed
  transform op1@(ListInsert {}) op2@(ApplySubtypeOperation {}) = rev <$> transform op2 op1

  transform op1@(ListInsert path1 i1 value1) op2@(ListInsert path2 i2 value2)
    | (path2 == path1) && (i1 >  i2) = rev <$> transform op2 op1 -- WLOG
    | (path2 == path1) && (i1 <= i2) = Right (op1, ListInsert path2 (succ i2) value2)
    | (path2 `isPrefixOf` path1) = rev <$> transform op2 op1 -- WLOG
    | (path1 `isPrefixOf` path2) = Right (op1, op2) -- TODO: increment the appropriate part of path2
    | otherwise = Right (op1, op2)
    --  (ApplySubtypeOperation path' t op, op2)
    --where
    --  (beginning, rest) = splitAt ((length listPath) + 1) path
    --  listPos@(Pos x) = last beginning
    --  path' = (init beginning) ++ [inc listPos] ++ rest

  transform op1@(Add path1 operand1) op2@(Add path2 operand2) = undefined
  transform op1@(Add aPath aOperand) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(Add aPath aOperand) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(Add aPath aOperand) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(Add aPath aOperand) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(Add aPath aOperand) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(Add aPath aOperand) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(Add aPath aOperand) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(Add aPath aOperand) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(Add aPath aOperand) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(Add aPath aOperand) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(Add aPath aOperand) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ListInsert liPath liI liValue) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ListInsert liPath liI liValue) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(Add aPath aOperand) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ListDelete path1 i1 value1) op2@(ListDelete path2 i2 value2) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ListDelete ldPath ldI ldValue) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ListDelete ldPath ldI ldValue) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(Add aPath aOperand) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ListReplace path1 i1 old1 new1) op2@(ListReplace path2 i2 old2 new2) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ListReplace lrPath lrI lrOld lrNew) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(Add aPath aOperand) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ListMove path1 src1 dst1) op2@(ListMove path2 src2 dst2) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ListMove lmPath lmSrc lmDst) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ListMove lmPath lmSrc lmDst) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(Add aPath aOperand) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ObjectInsert path1 key1 value1) op2@(ObjectInsert path2 key2 value2) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ObjectInsert oiPath oiKey oiValue) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(Add aPath aOperand) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ObjectDelete path1 key1 value1) op2@(ObjectDelete path2 key2 value2) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ObjectDelete odPath odKey odValue) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ObjectDelete odPath odKey odValue) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(Add aPath aOperand) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(ObjectReplace path1 key1 old1 new1) op2@(ObjectReplace path2 key2 old2 new2) = undefined
  -- transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(ObjectReplace orPath orKey orOld orNew) op2@(DeleteString dsPath dsI dsStr) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(Add aPath aOperand) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ListInsert liPath liI liValue) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ListDelete ldPath ldI ldValue) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ListMove lmPath lmSrc lmDst) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ObjectDelete odPath odKey odValue) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(ApplySubtypeOperation path1 op1) op2@(ApplySubtypeOperation path2 op2) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(InsertString isPath isI isStr) = undefined
  -- transform op1@(ApplySubtypeOperation asoPath asoOp) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(Add aPath aOperand) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(InsertString isPath isI isStr) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(InsertString path1 i1 str1) op2@(InsertString path2 i2 str2) = undefined
  transform op1@(InsertString isPath isI isStr) op2@(DeleteString dsPath dsI dsStr) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(Add aPath aOperand) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ListInsert liPath liI liValue) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ListDelete ldPath ldI ldValue) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ListReplace lrPath lrI lrOld lrNew) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ListMove lmPath lmSrc lmDst) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ObjectInsert oiPath oiKey oiValue) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ObjectDelete odPath odKey odValue) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform op1@(DeleteString dsPath dsI dsStr) op2@(ApplySubtypeOperation asoPath asoOp) = undefined
  transform op1@(DeleteString dsPath dsI dsStr) op2@(InsertString isPath isI isStr) = undefined
  transform op1@(DeleteString path1 i1 str1) op2@(DeleteString path2 i2 str2) = undefined
