{-# LANGUAGE TupleSections, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards, QuasiQuotes #-}

module Control.OperationalTransformation.JSON.Compose where


import Control.OperationalTransformation.JSON.Types


compose :: JSONOperation -> JSONOperation -> Either String JSONOperation
compose (Add path1 n1) (Add path2 n2) | path1 == path2 = Right $ Add path1 (n1 + n2)
