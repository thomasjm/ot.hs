{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections, ViewPatterns, MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.Text0
  (
  -- * JSON operations
  Text0Operation (..),
  SingleText0Operation(..)
  ) where


import Control.DeepSeq
import Control.OperationalTransformation
import Data.Aeson as A
import Data.Convertible
import Data.Data
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift

data SingleText0Operation = TextInsert Int T.Text
                          | TextDelete Int T.Text deriving (Eq, Show, Typeable, Data, Generic, NFData)

$(deriveLift ''SingleText0Operation)

newtype Text0Operation = T0 [SingleText0Operation] deriving (Eq, Show, Typeable, Data, Generic, NFData)

$(deriveLift ''Text0Operation)

invertOperation = undefined

isBlank (TextInsert _ s) = s == ""
isBlank (TextDelete _ s) = s == ""


instance OTOperation Text0Operation where
  transform (T0 []) x = Right (T0 [], x)
  transform x (T0 []) = Right (x, T0 [])

  transform (T0 [op1]) (T0 [op2]) | isBlank op1 = Right (T0 [], T0 [op2])
  transform (T0 [op1]) (T0 [op2]) | isBlank op2 = Right (T0 [op1], T0 [])
  transform (T0 [op1]) (T0 [op2]) | isBlank op2 && isBlank op2 = Right (T0 [], T0 [])

  transform (T0 [op1@(TextInsert p1 s1)]) (T0 [op2@(TextInsert p2 s2)]) | p1 > p2 = rev <$> transform' op2 op1
  transform (T0 [op1@(TextInsert p1 s1)]) (T0 [op2@(TextInsert p2 s2)]) = transform' op1 op2

  transform (T0 [op1@(TextDelete p1 s1)]) (T0 [op2@(TextInsert p2 s2)]) | p1 > p2 = rev <$> transform' op2 op1
  transform (T0 [op1@(TextDelete p1 s1)]) (T0 [op2@(TextInsert p2 s2)]) = transform' op1 op2

  transform (T0 [op1@(TextInsert p1 s1)]) (T0 [op2@(TextDelete p2 s2)]) | p1 > p2 = rev <$> transform' op2 op1
  transform (T0 [op1@(TextInsert p1 s1)]) (T0 [op2@(TextDelete p2 s2)]) = transform' op1 op2

  transform (T0 [op1@(TextDelete p1 s1)]) (T0 [op2@(TextDelete p2 s2)]) | p1 > p2 = rev <$> transform' op2 op1
  transform (T0 [op1@(TextDelete p1 s1)]) (T0 [op2@(TextDelete p2 s2)]) = transform' op1 op2


rev (a, b) = (b, a)

wrapList (a, b) = ([a], [b])

len = T.length

-- In transform', p1 <= p2 guaranteed
transform' op1@(TextInsert p1 s1) op2@(TextInsert p2 s2) | p1 == p2
  = Right (T0 [op1], T0 [TextInsert (p2 + (len s1)) s2])
-- Default behavior
transform' op1@(TextInsert p1 s1) op2@(TextInsert p2 s2)
  = Right (T0 [op1], T0 [TextInsert (p1 + p2) s2])

-- Insert is entirely within delete: split the delete
-- Note that the second delete is shifted back because it is applied after the first delete
transform' op1@(TextDelete p1 s1) op2@(TextInsert p2 s2) | p2 < p1 + len s1
  = Right (T0 [TextDelete p1 (T.take (p2 - p1) s1), TextDelete (p1 + (len s1) - (p2 - p1)) (T.drop (p2 - p1) s1)],
           T0 [TextInsert p1 s2])
-- Delete and insert at the same place: delete shifts forward
transform' op1@(TextDelete p1 s1) op2@(TextInsert p2 s2) | p1 == p2
  = Right (T0 [TextDelete (p1 + (len s2)) s1], T0 [op2])
-- (Default) Delete that comes strictly before insert just causes insert to shift back
transform' op1@(TextDelete p1 s1) op2@(TextInsert p2 s2)
  = Right (T0 [op1], T0 [TextInsert (p2 - (len s1)) s2])


-- TODO: insert before delete probably needs more cases
transform' op1@(TextInsert p1 s1) op2@(TextDelete p2 s2) | p1 == p2
  = Right (T0 [op1], T0 [TextDelete (p2 + (len s1)) s2])
transform' op1@(TextInsert p1 s1) op2@(TextDelete p2 s2)
  = Right (T0 [op1], T0 [TextDelete (p2 + (len s1)) s2])

-- Second delete exactly matches first delete
transform' op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) | p1 == p2, s1 == s2
  = Right (T0 [], T0 [])
-- Second delete is entirely within the first delete
transform' op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) | p2 < p1 + len s1
  = Right (T0 [TextDelete p1 ((T.take (p2 - p1) s1) <> (T.drop (p2 - p1 + (len s2)) s1))], T0 [])
-- (Default) Second delete comes after
transform' op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) | p1 + (len s1) < p2
  = Right (T0 [op1], T0 [TextDelete (p2 - (len s1)) s2])

transform' op1@(TextDelete p1 s1) op2@(TextDelete p2 s2) = error "Not implemented"


instance OTSystem T.Text Text0Operation where
  apply (T0 ops) txt = Right $ foldl applySingle txt ops
    -- Right $ applySingle txt ops
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
  parseJSON _ = fail "Couldn't parse Text0Operation"

instance ToJSON SingleText0Operation where
  toJSON (TextInsert p s) = object [("p", A.Number $ fromIntegral p), ("i", A.String s)]
  toJSON (TextDelete p d) = object [("p", A.Number $ fromIntegral p), ("d", A.String d)]


instance OTComposableOperation Text0Operation where
  compose (T0 ops1) (T0 ops2) = Right $ T0 (ops1 ++ ops2)


-- |Force parse an operation. Just for REPL testing.
parseOp :: A.Value -> Text0Operation
parseOp x = case fromJSON x of
  Success op -> T0 [op]
  Error err -> error err
