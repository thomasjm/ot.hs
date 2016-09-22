{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.OperationalTransformation.JSON.Types
( JSONOperation(..)
  , Path
  , PathSegment(..)
) where

import Control.Monad
import Control.OperationalTransformation.Text0
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V


-- * PathSegment and Path

data PathSegment
  = Prop T.Text
  | Pos Int
  deriving (Show, Eq)

type Path = [PathSegment]

instance FromJSON PathSegment where
  parseJSON (String x) = return $ Prop x
  parseJSON (Number x) = return $ Pos $ round x
  parseJSON _ = fail "Invalid Pathsegment"

instance ToJSON PathSegment where
  toJSON (Pos x) = A.Number $ fromIntegral x
  toJSON (Prop x) = A.String x

-- parseAction :: A.Value -> A.Parser [Action]
-- parseAction (A.Object x) | "i" `elem` HM.keys x = do
--   p <- x .: "p"
--   i <- x .: "i"
--   return $ [Retain p, Insert i]
-- parseAction (A.Object x) | "d" `elem` HM.keys x = do
--   p <- x .: "p"
--   d :: T.Text <- x .: "d"
--   return $ [Retain p, Delete (T.length d)]
-- parseAction x = error [i|Failed to parse: #{x}|]



-- * JSONOperation

-- Based on the "Summary of operations" at https://github.com/ottypes/json0
-- See also https://github.com/josephg/ShareJS/blob/master/lib/types/json-api.js
data JSONOperation

  = Identity

  -- * Numbers

  -- adds the number to the number at [path]
  | Add Path Int

  -- * Lists

  -- inserts the object obj before the item at idx in the list at [path]
  | ListInsert Path Int A.Value
  -- deletes the object obj from the index idx in the list at [path]
  | ListDelete Path Int A.Value
  -- replaces the object before at the index in the list at [path] with the object after
  | ListReplace Path Int A.Value A.Value
  -- moves the object at idx1 such that the object will be at index idx2 in the list at [path]
  | ListMove Path Int Int

  -- * Objects

  -- inserts the object obj into the object at [path] with key key
  -- The key may be Nothing, which is a special case essentially meaning we're doing an
  -- ObjectReplace of `null`. But the JS spec says
  -- (transformRight [j|{p:[],od:[""],oi:{}}|] [j|{p:[],od:[""]}|]) `shouldBe` [j|{p:[],oi:{}}|]
  -- , so we have to define this this way.
  | ObjectInsert Path (Maybe T.Text) A.Value
  -- deletes the object obj with key key from the object at [path]
  -- The key may be Nothing, which is a special case meaning we should delete the entire object
  | ObjectDelete Path (Maybe T.Text) A.Value
  -- replaces the object before with the object after at key key in the object at [path]
  -- The key may be Nothing, which is a special case meaning we should replace the entire object
  | ObjectReplace Path (Maybe T.Text) A.Value A.Value

  -- * Subtypes

  -- applies the subtype op o of type t to the object at [path]
  | ApplySubtypeOperation Path T.Text Text0Operation
  -- inserts the string s at offset offset into the string at [path] (uses subtypes internally)
  | StringInsert Path Int T.Text
  -- deletes the string s at offset offset from the string at [path] (uses subtypes internally)
  | StringDelete Path Int T.Text

  deriving (Eq, Show)

instance ToJSON JSONOperation where
  toJSON Identity = object []
  toJSON (Add path operand) = object [("p", toJSON path), ("na", A.Number $ fromIntegral operand)]
  toJSON (ListInsert path i value)    = object [("p", toJSON (path ++ [Pos i])), ("li", value)]
  toJSON (ListDelete path i value)    = object [("p", toJSON (path ++ [Pos i])), ("ld", value)]
  toJSON (ListReplace path i old new) = object [("p", toJSON (path ++ [Pos i])), ("ld", old), ("li", new)]
  toJSON (ListMove path src dst) = object [("p", toJSON (path ++ [Pos src])), ("lm", toJSON (Pos dst))]
  toJSON (ObjectInsert path (Just key) value)    = object [("p", toJSON (path ++ [Prop key])), ("oi", value)]
  toJSON (ObjectInsert path Nothing value)    = object [("p", toJSON path), ("oi", value)]
  toJSON (ObjectDelete path (Just key) value)    = object [("p", toJSON (path ++ [Prop key])), ("od", value)]
  toJSON (ObjectDelete path Nothing value)    = object [("p", toJSON path), ("od", value)]

  toJSON (ObjectReplace path (Just key) old new) = object [("p", toJSON (path ++ [Prop key])), ("od", old), ("oi", new)]
  toJSON (ObjectReplace path Nothing old new) = object [("p", toJSON path), ("od", old), ("oi", new)]

  toJSON (ApplySubtypeOperation path t ops) = object [("p", toJSON path), ("t", A.String t), ("o", A.Array $ V.fromList (fmap toJSON ops))]
  toJSON (StringInsert path i s) = object [("p", toJSON (path ++ [Pos i])), ("si", A.String s)]
  toJSON (StringDelete path i s) = object [("p", toJSON (path ++ [Pos i])), ("sd", A.String s)]

instance FromJSON JSONOperation where
  parseJSON (A.Object v) | HM.null v = return Identity

  -- Numbers
  parseJSON (A.Object v) | "na" `elem` (HM.keys v) = Add <$> v .: "p" <*> v .: "na"

  -- Lists
  parseJSON (A.Object v) | "ld" `elem` (HM.keys v) && "li" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             before <- v .: "ld"
                             after <- v .: "li"
                             return $ ListReplace path index before after
  parseJSON (A.Object v) | "li" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             obj <- v .: "li"
                             return $ ListInsert path index obj
  parseJSON (A.Object v) | "ld" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             obj <- v .: "ld"
                             return $ ListDelete path index obj
  parseJSON (A.Object v) | "lm" `elem` (HM.keys v) = do
                             (path, index1) <- parsePathAndIndex v
                             index2 <- v .: "lm"
                             return $ ListMove path index1 index2

  -- Objects
  parseJSON (A.Object v) | "od" `elem` (HM.keys v) && "oi" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             before <- v .: "od"
                             after <- v .: "oi"
                             return $ ObjectReplace path prop before after
  parseJSON (A.Object v) | "oi" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             when (isNothing prop) $ fail "Missing key on object insert"
                             obj <- v .: "oi"
                             return $ ObjectInsert path prop obj
  parseJSON (A.Object v) | "od" `elem` (HM.keys v) = do
                             (path, prop) <- parsePathAndProp v
                             obj <- v .: "od"
                             return $ ObjectDelete path prop obj

  -- Subtypes
  parseJSON (A.Object v) | "o" `elem` (HM.keys v) = do
                             path <- v .: "p"
                             typ <- v .: "t"
                             case typ of
                               "text0" -> do
                                 ops :: Text0Operation <- v .: "o"
                                 return $ ApplySubtypeOperation path typ ops
                               _ -> error "Unrecognized subtype operation"

  parseJSON (A.Object v) | "si" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             str <- v .: "si"
                             return $ if T.null str
                               then Identity
                               else StringInsert path index str
  parseJSON (A.Object v) | "sd" `elem` (HM.keys v) = do
                             (path, index) <- parsePathAndIndex v
                             str <- v .: "sd"
                             return $ if T.null str
                               then Identity
                               else StringDelete path index str
  parseJSON _ = fail "Failed to parse operation"

parsePathAndIndex :: A.Object -> A.Parser ([PathSegment], Int)
parsePathAndIndex v = do
  pathAndIndex :: [PathSegment]  <- v .: "p"
  if null pathAndIndex then do
      fail "Can't parse empty list index"
    else do
      let path = init pathAndIndex
      let (Pos index) = last pathAndIndex
      return (path, index)

parsePathAndProp :: A.Object -> A.Parser ([PathSegment], Maybe T.Text)
parsePathAndProp v = do
  pathAndProp :: [PathSegment]  <- v .: "p"
  if null pathAndProp then
    return ([], Nothing)
    else do
      let path = init pathAndProp
      let (Prop prop) = last pathAndProp
      return (path, Just prop)
