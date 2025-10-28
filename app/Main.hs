
{-#LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main (main) where

import Lib
import Data.Aeson
import System.Console.CmdArgs
import System.Environment
import Language.TypeScript.Par
import Language.TypeScript.AST
import Language.TypeScript.ParserCombinators
import Language.Kotlin.AST
import qualified Data.ByteString.Lazy as BS
import Text.Pretty.Simple
import Generator
import Prettyprinter
import Generator (convertSourceFile)

data CmdOptions = CmdOptions {
    inputFile :: FilePath,
    outputFile :: FilePath
} deriving (Show, Eq, Data, Typeable)

main :: IO ()
main = do
    a <- getArgs
    opts <- cmdArgs mode
    input <- BS.readFile (inputFile opts)
    let ts_node = decode input :: Maybe TsNode
    ast <- case ts_node of
        Nothing -> error "Failed to parse input JSON."
        Just ts -> do
            let parseResult =  par pSourceFile [ts]
            case parseResult of
                Right ts_ast -> return ts_ast
                Left err -> error $ "Parsing failed" ++ show err
                
    let kt_asts = convertSourceFile ast
    pPrint kt_asts
    let res = pretty kt_asts
    writeFile (outputFile opts) (show res)
    where
        mode = CmdOptions {
            inputFile = def &= help "input file" &= typFile,
            outputFile = def &= help "output file" &= typFile
        }
