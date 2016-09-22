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

data Text0Operation = TextInsert Int T.Text
                    | TextDelete Int T.Text deriving (Eq, Show)


invertOperation = undefined


instance OTOperation Text0Operation where
  transform op1 op2 = error "Text0Operation transform not defined"


instance OTSystem T.Text Text0Operation where
  apply (TextInsert p t) txt = Right $ convert $ begin ++ (convert t) ++ end
    where (begin, end) = L.splitAt p ((convert txt) :: String)

  apply (TextDelete p t) txt = Right $ convert $ begin ++ (L.drop (L.length ((convert t) :: String)) end)
    where (begin, end) = L.splitAt p ((convert txt) :: String)


instance FromJSON Text0Operation where
  parseJSON (A.Object x) | "i" `elem` HM.keys x = do
    p <- x .: "p"
    i <- x .: "i"
    return $ TextInsert p i
  parseJSON (A.Object x) | "d" `elem` HM.keys x = do
    p <- x .: "p"
    d :: T.Text <- x .: "d"
    return $ TextDelete p d

instance ToJSON Text0Operation where
  toJSON (TextInsert p s) = object [("p", A.Number $ fromIntegral p), ("s", A.String s)]
  toJSON (TextDelete p d) = object [("p", A.Number $ fromIntegral p), ("s", A.String d)]
