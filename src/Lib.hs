{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-# LANGUAGE TemplateHaskell #-}

module Lib where

-- import qualified Language.Kotlin.AST as KT
import qualified Language.TypeScript.AST as TS
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Data.Data
import GHC.Generics
import Text.Pretty.Simple
import System.IO.Unsafe
import qualified Data.ByteString.Lazy as BS
-- example :: BS.ByteString
example1 = unsafePerformIO $ BS.readFile "example/cases/case5_class_field_func_global_func.json"

data Loc = Loc {
    line   :: Int,
    column :: Int
} deriving (Show, Eq, Ord, Data, Generic)

deriveJSON defaultOptions ''Loc

data TsNode = TsNode {
    kind        :: String,
    -- kindNumber  :: Int,
    children    :: Maybe [TsNode],
    content     :: Maybe String,
    pos         :: Loc
} deriving (Show, Eq, Ord, Data, Generic)

deriveJSON defaultOptions ''TsNode

ts = fromJust (decode example1 :: Maybe TsNode)

foo :: IO ()
foo = do 
    let Just ts = decode example1 :: Maybe TsNode
    pPrint ts
