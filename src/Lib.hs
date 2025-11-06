{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

-- import qualified Language.Kotlin.AST as KT
import Data.Aeson
import Data.Aeson.TH
import Data.Data
data Loc = Loc {
    line   :: Int,
    column :: Int
} deriving (Show, Eq, Ord, Data)

deriveJSON defaultOptions ''Loc

data TsNode = TsNode {
    kind        :: String,
    -- kindNumber  :: Int,
    children    :: Maybe [TsNode],
    content     :: Maybe String,
    pos         :: Loc
} deriving (Show, Eq, Ord, Data)

deriveJSON defaultOptions ''TsNode