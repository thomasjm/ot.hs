
{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}

module Control.OperationalTransformation.JSON.QuasiQuote (j) where

import Data.Aeson.QQ
import Language.Haskell.TH.Quote

j :: QuasiQuoter
j = aesonQQ
