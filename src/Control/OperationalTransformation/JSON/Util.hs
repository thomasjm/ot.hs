{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Util where

import Control.OperationalTransformation.JSON.Types
import Data.Aeson as A
import Data.String.Interpolate.IsString

inc :: PathSegment -> PathSegment
inc = add 1

dec :: PathSegment -> PathSegment
dec = add (-1)

add :: Int -> PathSegment -> PathSegment
add y (Pos x) = Pos (x + y)
add _ (Prop _) = error "Tried to add to a prop"

rev (a, b) = (b, a)

unPos (Pos x) = x
unPos _ = error "unPos called on prop"

unProp (Prop x) = x
unProp _ = error "unProp called on pos"

-- |Force parse an operation. Just for REPL testing.
parseOp :: A.Value -> JSONOperation
parseOp x = case fromJSON x of
  Success op -> op
  Error err -> error err


-- |A generic way to set/get the path from a JSONOperation
-- TODO: replace this with lenses or something
class HasPath a where
  getPath :: a -> Path
  setPath :: Path -> a -> a
instance HasPath JSONOperation where
  getPath (StringInsert path _ _) = path
  getPath (StringDelete path _ _) = path

  getPath (ObjectInsert path _ _) = path
  getPath (ObjectDelete path _ _) = path
  getPath (ObjectReplace path _ _ _ ) = path

  getPath (ListInsert path i _) = path
  getPath (ListDelete path _ _) = path
  getPath (ListReplace path _ _ _) = path
  getPath (ListMove path _ _) = path

  getPath (Add path _) = path

  getPath (ApplySubtypeOperation path _ _) = path
  getPath x = error $ "getPath undefined for: " ++ show x

  setPath path' (StringInsert _ x y) = (StringInsert path' x y)
  setPath path' (StringDelete _ x y) = (StringInsert path' x y)

  setPath path' (ObjectInsert _ x y) = (ObjectInsert path' x y)
  setPath path' (ObjectDelete _ x y) = (ObjectDelete path' x y)
  setPath path' (ObjectReplace _ x y z) = (ObjectReplace path' x y z)

  setPath path' (ListInsert _ x y) = (ListInsert path' x y)
  setPath path' (ListDelete _ x y) = (ListDelete path' x y)
  setPath path' (ListReplace _ x y z) = (ListReplace path' x y z)
  setPath path' (ListMove _ x y) = (ListMove path' x y)

  setPath path' (ApplySubtypeOperation _ x y) = ApplySubtypeOperation path' x y

  setPath path' (Add _ k) = Add path' k

  setPath _ x = error [i|setPath undefined for #{x}|]


-- |A "full path" is different from a "path" because it contains the list
-- index or object key, if present. This is sometimes more useful.
class HasFullPath a where
  getFullPath :: a -> Path
  setFullPath :: Path -> a -> a

instance HasFullPath JSONOperation where
  getFullPath (StringInsert path _ _) = path
  getFullPath (StringDelete path _ _) = path

  getFullPath (ObjectInsert path (Just key) _) = path ++ [Prop key]
  getFullPath (ObjectDelete path (Just key) _) = path ++ [Prop key]
  getFullPath (ObjectReplace path (Just key) _ _ ) = path ++ [Prop key]

  getFullPath (ObjectInsert path Nothing _) = path
  getFullPath (ObjectDelete path Nothing _) = path
  getFullPath (ObjectReplace path Nothing _ _ ) = path

  getFullPath (ListInsert path i _) = path ++ [Pos i]
  getFullPath (ListDelete path i _) = path ++ [Pos i]
  getFullPath (ListReplace path i _ _) = path ++ [Pos i]
  getFullPath (ListMove path i _) = path ++ [Pos i]

  getFullPath (Add path _) = path

  getFullPath (ApplySubtypeOperation path _ _) = path
  getFullPath x = error $ "getFullPath undefined for: " ++ show x

  setFullPath path' (StringInsert _ x y) = StringInsert path' x y
  setFullPath path' (StringDelete _ x y) = StringInsert path' x y
  setFullPath path' (ApplySubtypeOperation _ x y) = ApplySubtypeOperation path' x y

  setFullPath path' (ObjectInsert _ _ y) = ObjectInsert (init path') (Just $ unProp $ last path') y
  setFullPath path' (ObjectDelete _ _ y) = ObjectDelete (init path') (Just $ unProp $ last path') y
  setFullPath path' (ObjectReplace _ _ y z) = ObjectReplace (init path') (Just $ unProp $ last path') y z

  setFullPath path' (ListInsert _ _ y) = ListInsert (init path') (unPos $ last path') y
  setFullPath path' (ListDelete _ _ y) = ListDelete (init path') (unPos $ last path') y
  setFullPath path' (ListReplace _ _ y z) = ListReplace (init path') (unPos $ last path') y z
  setFullPath path' (ListMove _ _ y) = ListMove (init path') (unPos $ last path') y

  setFullPath _ x = error [i|setFullPath undefined for #{x}|]


isListInsert (ListInsert {}) = True
isListInsert _ = False

-- | Get the path and key for an object. Useful for simplifying the "affects" function.
getObjectPathAndKey (ObjectInsert path key _) = Just (path, key)
getObjectPathAndKey (ObjectDelete path key _) = Just (path, key)
getObjectPathAndKey (ObjectReplace path key _ _) = Just (path, key)
getObjectPathAndKey _ = Nothing
