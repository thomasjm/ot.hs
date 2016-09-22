
module Control.OperationalTransformation.JSON.Util where

import Control.OperationalTransformation.JSON.Types
import Data.Aeson as A

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

  getPath (ListInsert path _ _) = path
  getPath (ListDelete path _ _) = path

  getPath (ApplySubtypeOperation path _ _) = path
  getPath x = error $ "getPath undefined for: " ++ show x

  setPath path' (StringInsert _ x y) = (StringInsert path' x y)
  setPath path' (StringDelete _ x y) = (StringInsert path' x y)
  setPath path' (ListInsert _ x y) = (ListInsert path' x y)
  setPath path' (ListDelete _ x y) = (ListDelete path' x y)
  setPath path' (ApplySubtypeOperation _ x y) = ApplySubtypeOperation path' x y
  setPath _ _ = error "setPath undefined"
