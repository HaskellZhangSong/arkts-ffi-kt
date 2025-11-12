{-# LANGUAGE DeriveDataTypeable #-}
module Options where

import Data.Typeable
import System.Console.CmdArgs
import Data.Data

data CmdOptions = CmdOptions {
    input_file :: FilePath,
    output_file :: FilePath,
    dump_json :: Bool,
    dump_ts_node :: Bool,
    dump_ts_ast :: Bool,
    dump_kotlin_ast :: Bool,
    keep_json :: Bool,
    package_name :: String
} deriving (Show, Eq, Data, Typeable)

options = CmdOptions {
    input_file = def &= args &= typ "FILES",
    output_file = "a.out.kt" &= help "output file" &= typFile,
    dump_json = False &= help "dump intermediate JSON from tsp",
    dump_ts_node = False &= help "dump TypeScript TsNode AST",
    dump_ts_ast = False &= help "dump TypeScript AST",
    dump_kotlin_ast = False &= help "dump generated Kotlin AST",
    keep_json = False &= help "keep intermediate JSON file",
    package_name = "ffi.demo" &= help "Kotlin package name"
}


