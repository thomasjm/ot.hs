{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Compose where

import Control.OperationalTransformation.JSON.Types
import Control.OperationalTransformation.JSON.Util
import Control.OperationalTransformation.Text0
import Data.Monoid
import qualified Data.Text as T



compose :: JSONOperation -> JSONOperation -> Either String JSONOperation

compose (JSONOperation [op1]) (JSONOperation [op2]) = case compose' op1 op2 of
  Left err -> Right $ JSONOperation [op1, op2]
  Right ops -> Right $ JSONOperation ops
compose (JSONOperation ops1) (JSONOperation ops2) = Right $ JSONOperation $ ops1 ++ ops2


compose' (Add path1 n1) (Add path2 n2) | path1 == path2 = Right [Add path1 (n1 + n2)]

-- List insert and delete cancel out
compose' (ListInsert path1 index1 obj1) (ListDelete path2 index2 obj2)
  | path1 == path2, index1 == index2, obj1 == obj2
  = Right [Identity]
compose' (ListDelete path1 index1 obj1) (ListInsert path2 index2 obj2)
  | path1 == path2, index1 == index2, obj1 == obj2
  = Right [Identity]

-- List replace and delete get mashed togeter if possible
compose' (ListReplace path1 index1 obj11 obj12) (ListDelete path2 index2 obj2)
  | path1 == path2, index1 == index2, obj12 == obj2
  = Right [ListDelete path1 index1 obj11]

-- String inserts get mashed together if possible
compose' (StringInsert path1 pos1 text1) (StringInsert path2 pos2 text2)
  | path1 == path2, pos2 == pos1 + (T.length text1)
  = Right [StringInsert path1 pos1 (text1 <> text2)]
compose' (ApplySubtypeOperation path1 "text0" (T0 [TextInsert pos1 text1]))
         (ApplySubtypeOperation path2 "text0" (T0 [TextInsert pos2 text2]))
  | path1 == path2, pos2 == pos1 + (T.length text1)
  = Right [ApplySubtypeOperation path1 "text0" (T0 [TextInsert pos1 (text1 <> text2)])]

-- Object replacements compose
compose' op1@(ObjectReplace path key from1 to1) op2@(ObjectReplace _ _ from2 to2)
  | (getFullPath op1) == (getFullPath op2) = Right [ObjectReplace path key from1 to2]

-- Object delete + insert becomes replace
compose' op1@(ObjectDelete path key val1) op2@(ObjectInsert _ _ val2)
  | (getFullPath op1) == (getFullPath op2) = Right [ObjectReplace path key val1 val2]

compose' op1 op2 = Left "Don't know how to compose these"
