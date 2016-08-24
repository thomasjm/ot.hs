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


  transform (Add path1 operand1) (Add path2 operand2) = undefined
  transform (Add aPath aOperand) (ListInsert liPath liI liValue) = undefined
  transform (Add aPath aOperand) (ListDelete ldPath ldI ldValue) = undefined
  transform (Add aPath aOperand) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (Add aPath aOperand) (ListMove lmPath lmSrc lmDst) = undefined
  transform (Add aPath aOperand) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (Add aPath aOperand) (ObjectDelete odPath odKey odValue) = undefined
  transform (Add aPath aOperand) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (Add aPath aOperand) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (Add aPath aOperand) (InsertString isPath isI isStr) = undefined
  transform (Add aPath aOperand) (DeleteString dsPath dsI dsStr) = undefined
  transform (ListInsert liPath liI liValue) (Add aPath aOperand) = undefined
  transform (ListInsert path1 i1 value1) (ListInsert path2 i2 value2) = undefined
  transform (ListInsert liPath liI liValue) (ListDelete ldPath ldI ldValue) = undefined
  transform (ListInsert liPath liI liValue) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ListInsert liPath liI liValue) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ListInsert liPath liI liValue) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ListInsert liPath liI liValue) (ObjectDelete odPath odKey odValue) = undefined
  transform (ListInsert liPath liI liValue) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ListInsert liPath liI liValue) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ListInsert liPath liI liValue) (InsertString isPath isI isStr) = undefined
  transform (ListInsert liPath liI liValue) (DeleteString dsPath dsI dsStr) = undefined
  transform (ListDelete ldPath ldI ldValue) (Add aPath aOperand) = undefined
  transform (ListDelete ldPath ldI ldValue) (ListInsert liPath liI liValue) = undefined
  transform (ListDelete path1 i1 value1) (ListDelete path2 i2 value2) = undefined
  transform (ListDelete ldPath ldI ldValue) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ListDelete ldPath ldI ldValue) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ListDelete ldPath ldI ldValue) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ListDelete ldPath ldI ldValue) (ObjectDelete odPath odKey odValue) = undefined
  transform (ListDelete ldPath ldI ldValue) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ListDelete ldPath ldI ldValue) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ListDelete ldPath ldI ldValue) (InsertString isPath isI isStr) = undefined
  transform (ListDelete ldPath ldI ldValue) (DeleteString dsPath dsI dsStr) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (Add aPath aOperand) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ListInsert liPath liI liValue) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ListDelete ldPath ldI ldValue) = undefined
  transform (ListReplace path1 i1 old1 new1) (ListReplace path2 i2 old2 new2) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ObjectDelete odPath odKey odValue) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ListReplace lrPath lrI lrOld lrNew) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (InsertString isPath isI isStr) = undefined
  transform (ListReplace lrPath lrI lrOld lrNew) (DeleteString dsPath dsI dsStr) = undefined
  transform (ListMove lmPath lmSrc lmDst) (Add aPath aOperand) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ListInsert liPath liI liValue) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ListDelete ldPath ldI ldValue) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ListMove path1 src1 dst1) (ListMove path2 src2 dst2) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ObjectDelete odPath odKey odValue) = undefined
  transform (ListMove lmPath lmSrc lmDst) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ListMove lmPath lmSrc lmDst) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ListMove lmPath lmSrc lmDst) (InsertString isPath isI isStr) = undefined
  transform (ListMove lmPath lmSrc lmDst) (DeleteString dsPath dsI dsStr) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (Add aPath aOperand) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ListInsert liPath liI liValue) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ListDelete ldPath ldI ldValue) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ObjectInsert path1 key1 value1) (ObjectInsert path2 key2 value2) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ObjectDelete odPath odKey odValue) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ObjectInsert oiPath oiKey oiValue) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (InsertString isPath isI isStr) = undefined
  transform (ObjectInsert oiPath oiKey oiValue) (DeleteString dsPath dsI dsStr) = undefined
  transform (ObjectDelete odPath odKey odValue) (Add aPath aOperand) = undefined
  transform (ObjectDelete odPath odKey odValue) (ListInsert liPath liI liValue) = undefined
  transform (ObjectDelete odPath odKey odValue) (ListDelete ldPath ldI ldValue) = undefined
  transform (ObjectDelete odPath odKey odValue) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ObjectDelete odPath odKey odValue) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ObjectDelete odPath odKey odValue) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ObjectDelete path1 key1 value1) (ObjectDelete path2 key2 value2) = undefined
  transform (ObjectDelete odPath odKey odValue) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ObjectDelete odPath odKey odValue) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ObjectDelete odPath odKey odValue) (InsertString isPath isI isStr) = undefined
  transform (ObjectDelete odPath odKey odValue) (DeleteString dsPath dsI dsStr) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (Add aPath aOperand) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ListInsert liPath liI liValue) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ListDelete ldPath ldI ldValue) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ListMove lmPath lmSrc lmDst) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (ObjectDelete odPath odKey odValue) = undefined
  transform (ObjectReplace path1 key1 old1 new1) (ObjectReplace path2 key2 old2 new2) = undefined
  -- transform (ObjectReplace orPath orKey orOld orNew) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (InsertString isPath isI isStr) = undefined
  transform (ObjectReplace orPath orKey orOld orNew) (DeleteString dsPath dsI dsStr) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (Add aPath aOperand) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ListInsert liPath liI liValue) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ListDelete ldPath ldI ldValue) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ListReplace lrPath lrI lrOld lrNew) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ListMove lmPath lmSrc lmDst) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ObjectInsert oiPath oiKey oiValue) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ObjectDelete odPath odKey odValue) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (ApplySubtypeOperation path1 op1) (ApplySubtypeOperation path2 op2) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (InsertString isPath isI isStr) = undefined
  -- transform (ApplySubtypeOperation asoPath asoOp) (DeleteString dsPath dsI dsStr) = undefined
  transform (InsertString isPath isI isStr) (Add aPath aOperand) = undefined
  transform (InsertString isPath isI isStr) (ListDelete ldPath ldI ldValue) = undefined
  transform (InsertString isPath isI isStr) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (InsertString isPath isI isStr) (ListMove lmPath lmSrc lmDst) = undefined
  transform (InsertString isPath isI isStr) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (InsertString isPath isI isStr) (ObjectDelete odPath odKey odValue) = undefined
  transform (InsertString isPath isI isStr) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (InsertString isPath isI isStr) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (InsertString path1 i1 str1) (InsertString path2 i2 str2) = undefined
  transform (InsertString isPath isI isStr) (DeleteString dsPath dsI dsStr) = undefined
  transform (DeleteString dsPath dsI dsStr) (Add aPath aOperand) = undefined
  transform (DeleteString dsPath dsI dsStr) (ListInsert liPath liI liValue) = undefined
  transform (DeleteString dsPath dsI dsStr) (ListDelete ldPath ldI ldValue) = undefined
  transform (DeleteString dsPath dsI dsStr) (ListReplace lrPath lrI lrOld lrNew) = undefined
  transform (DeleteString dsPath dsI dsStr) (ListMove lmPath lmSrc lmDst) = undefined
  transform (DeleteString dsPath dsI dsStr) (ObjectInsert oiPath oiKey oiValue) = undefined
  transform (DeleteString dsPath dsI dsStr) (ObjectDelete odPath odKey odValue) = undefined
  transform (DeleteString dsPath dsI dsStr) (ObjectReplace orPath orKey orOld orNew) = undefined
  -- transform (DeleteString dsPath dsI dsStr) (ApplySubtypeOperation asoPath asoOp) = undefined
  transform (DeleteString dsPath dsI dsStr) (InsertString isPath isI isStr) = undefined
  transform (DeleteString path1 i1 str1) (DeleteString path2 i2 str2) = undefined
