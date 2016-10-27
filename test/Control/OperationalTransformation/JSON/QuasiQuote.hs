{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.JSON.QuasiQuote (v, s, l) where

import Control.OperationalTransformation.JSON.Types
import Data.Aeson
import Data.Aeson.QQ
import qualified Data.ByteString.Lazy.Char8 as BS8
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)


v :: QuasiQuoter
v = aesonQQ

------------------------------------------------------------------------

s :: QuasiQuoter
s = QuasiQuoter {
  quoteExp = jsonOpExp,
  quotePat = const $ error "No quotePat defined for JSONOp quasiquoter",
  quoteType = const $ error "No quoteType defined for JSONOp quasiquoter",
  quoteDec = const $ error "No quoteDec defined for JSONOp quasiquoter"
}

jsonOpExp :: String -> ExpQ
jsonOpExp txt =
  case eitherDecode $ BS8.pack txt of
    Left err -> error $ "Error in aesonExp: " ++ show err
    Right !(val :: JSONOp) -> lift $ JSONOperation [val]

------------------------------------------------------------------------

l :: QuasiQuoter
l = QuasiQuoter {
  quoteExp = jsonOperationExp,
  quotePat = const $ error "No quotePat defined for JSONOperation quasiquoter",
  quoteType = const $ error "No quoteType defined for JSONOperation quasiquoter",
  quoteDec = const $ error "No quoteDec defined for JSONOperation quasiquoter"
}

jsonOperationExp :: String -> ExpQ
jsonOperationExp txt =
  case eitherDecode $ BS8.pack txt of
    Left err -> error $ "Error in aesonExp: " ++ show err
    Right (val :: JSONOperation) -> lift val
