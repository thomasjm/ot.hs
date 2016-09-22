{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, ViewPatterns, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.Text0
  (
  -- * JSON operations
  Text0Operation (..)
  ) where


import Control.OperationalTransformation
import Data.Aeson as A
import Data.Convertible
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T

data SingleText0Operation = TextInsert Int T.Text
                          | TextDelete Int T.Text
                          | Text0Identity deriving (Eq, Show)


type Text0Operation = [SingleText0Operation]


invertOperation = undefined


instance OTOperation SingleText0Operation where
  transform op1@(TextInsert p1 s1) op2@(TextInsert p2 s2) | p1 > p2 = rev <$> transform' op2 op1
  transform op1@(TextInsert p1 s1) op2@(TextInsert p2 s2) = transform' op1 op2

  transform op1@(TextDelete p1 s1) op2@(TextInsert p2 s2) | p1 > p2 = rev <$> transform' op2 op1
  transform op1@(TextDelete p1 s1) op2@(TextInsert p2 s2) = transform' op1 op2

  transform op1@(TextInsert p1 s1) op2@(TextDelete p2 s2) | p1 > p2 = rev <$> transform' op2 op1
  transform op1@(TextInsert p1 s1) op2@(TextDelete p2 s2) = transform' op1 op2

  transform op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) | p1 > p2 = rev <$> transform' op2 op1
  transform op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) = transform' op1 op2


rev (a, b) = (b, a)

wrapList (a, b) = ([a], [b])


-- In transform', p1 <= p2 guaranteed
transform' op1@(TextInsert p1 s1) op2@(TextInsert p2 s2) | p1 + (T.length s1) <= p2 = Right (op1, TextInsert (p1 + p2) s2)
transform' op1@(TextDelete p1 s1) op2@(TextInsert p2 s2) = error "Not implemented"
transform' op1@(TextInsert p1 s1) op2@(TextDelete p2 s2) = error "Not implemented"
transform' op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) = error "Not implemented"


instance OTSystem T.Text SingleText0Operation where
  apply ops txt = Right $ applySingle txt ops
    --Right $ foldl applySingle txt ops

applySingle txt (TextInsert p t) = convert $ begin ++ (convert t) ++ end
  where (begin, end) = L.splitAt p ((convert txt) :: String)
applySingle txt (TextDelete p t) = convert $ begin ++ (L.drop (L.length ((convert t) :: String)) end)
  where (begin, end) = L.splitAt p ((convert txt) :: String)

instance FromJSON SingleText0Operation where
  parseJSON (A.Object x) | "i" `elem` HM.keys x = do
    p <- x .: "p"
    i <- x .: "i"
    return $ TextInsert p i
  parseJSON (A.Object x) | "d" `elem` HM.keys x = do
    p <- x .: "p"
    d :: T.Text <- x .: "d"
    return $ TextDelete p d
  parseJSON (A.Object x) | HM.null x = return Text0Identity
  parseJSON _ = fail "Couldn't parse Text0Operation"

instance ToJSON SingleText0Operation where
  toJSON (TextInsert p s) = object [("p", A.Number $ fromIntegral p), ("i", A.String s)]
  toJSON (TextDelete p d) = object [("p", A.Number $ fromIntegral p), ("d", A.String d)]
