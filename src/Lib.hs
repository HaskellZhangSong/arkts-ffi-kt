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

decl_list :: [String]
decl_list = [
    "ClassDeclaration",
    "FunctionDeclaration",
    "PropertyDeclaration"
    ]

filterSourceFile :: TsNode -> TsNode
filterSourceFile (TsNode "SourceFile" (Just (x:xs)) cont p) = 
    TsNode "SourceFile" (Just $ filterSyntax x: xs) cont p

filterSourceFile _ = error "Not a SourceFile node"

filterSyntax :: TsNode -> TsNode
filterSyntax (TsNode "SyntaxList" (Just chn) cont p) = 
    TsNode "SyntaxList" (Just $ filter (\child -> kind child `elem` decl_list) chn) cont p
filterSyntax _ = error "Not a SyntaxList node"