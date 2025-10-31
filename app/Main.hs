
{-#LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main (main) where

import Lib
import Control.Monad
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
import System.Exit
import System.FilePath
import System.Process
import Data.List
data CmdOptions = CmdOptions {
    input_file :: FilePath,
    output_file :: FilePath,
    dump_json :: Bool,
    dump_ts_node :: Bool,
    dump_ts_ast :: Bool,
    dump_kotlin_ast :: Bool,
    keep_json :: Bool
} deriving (Show, Eq, Data, Typeable)

options = CmdOptions {
    input_file = def &= args &= typ "FILES",
    output_file = "a.out.kt" &= help "output file" &= typFile,
    dump_json = False &= help "dump intermediate JSON from tsp",
    dump_ts_node = False &= help "dump TypeScript TsNode AST",
    dump_ts_ast = False &= help "dump TypeScript AST",
    dump_kotlin_ast = False &= help "dump generated Kotlin AST",
    keep_json = False &= help "keep intermediate JSON file"
}

create_temp_file :: CmdOptions -> IO FilePath
create_temp_file _ = do
    temp_file <- readCreateProcess (proc "mktemp" ["-t", "arkts-ffi.json"]) ""
    return $ init $ temp_file -- drop \n at the end

commandExists :: String -> IO Bool
commandExists cmd = do
    (exitCode, _, _) <- readProcessWithExitCode "which" [cmd] ""
    return (exitCode == ExitSuccess)

compile_to_json :: CmdOptions -> FilePath -> FilePath -> IO ()
compile_to_json _ input_ts temp_file = do
    tsp <- readProcessWithExitCode "tsp" [input_ts, "-o", temp_file] ""
    case tsp of
        (ExitSuccess, _, _) -> return ()
        (ExitFailure code, err, _) -> do
            putStrLn $ "tsp failed: " ++ err
            putStrLn $ "json output file: " ++ temp_file
            exitWith (ExitFailure code)

get_ts_node :: CmdOptions -> FilePath -> IO TsNode
get_ts_node opts json_file = do
    input <- BS.readFile json_file
    let ts_node = decode input :: Maybe TsNode
    case ts_node of
        Nothing -> do
            putStrLn $ "Failed to decode TypeScript AST from JSON for file: " ++ json_file
            exitFailure
        Just ts -> do
            when (dump_json opts) $ do
                putStrLn $ replicate 80 '='
                putStrLn "Dumped TypeScript JSON AST:"
                putStrLn $ replicate 80 '='
                pPrint ts
            when (dump_ts_node opts) $ do
                putStrLn $ replicate 80 '='
                putStrLn "Dumped TypeScript TsNode AST:"
                putStrLn $ replicate 80 '='
                pPrint ts
            return ts

compile_to_ts :: CmdOptions -> FilePath -> TsNode -> IO SourceFile
compile_to_ts opts json_file ts_node = do
            let parseResult =  par pSourceFile [ts]
            case parseResult of
                Right ts_ast -> do
                    when (dump_ts_ast opts) $ do
                        putStrLn $ replicate 80 '='
                        putStrLn "Dumped TypeScript AST:"
                        pPrint ts_ast
                        putStrLn $ replicate 80 '='
                    return ts_ast
                Left err -> do 
                    putStrLn $ "Parsing failed" ++ show err
                    putStrLn $ "Input JSON: " ++ show json_file
                    exitFailure

data InputType = InputTypeJSON | InputTypeTS

get_input_type :: CmdOptions -> InputType
get_input_type opt =
    if isSuffixOf ".json" (input_file opt)
        then InputTypeJSON
        else InputTypeTS

main :: IO ()
main = do
    exists <- commandExists "tsp"
    case exists of
        True -> return ()
        False -> do
            putStrLn "Error: 'tsp' command not found."
            putStrLn "Please install TypeScript Parser (tsp)."
            exitFailure
    opts <- cmdArgs $ modes [options]    

    let input_file_path = input_file opts
    let input_type = get_input_type opts
    json_file <- case input_type of
        InputTypeJSON -> do
            return input_file_path
        InputTypeTS -> do 
            tmp_file <- create_temp_file opts
            compile_to_json opts input_file_path tmp_file
            return tmp_file

    ts_node <- get_ts_node opts json_file
    ast <- compile_to_ts opts json_file ts_node
    -- pPrint ast
    let kt_asts = convertSourceFile ast
    when (dump_kotlin_ast opts) $ do
        putStrLn $ replicate 80 '='
        putStrLn "Dumped Kotlin AST:"
        pPrint kt_asts
        putStrLn $ replicate 80 '='
    -- pPrint kt_asts
    let res = pretty kt_asts
    writeFile (output_file opts) (show res)
    -- remove json file
    case input_type of
        InputTypeJSON -> return ()
        InputTypeTS -> do
            unless (keep_json opts) $ do
                void $ readProcess "rm" [json_file] ""
            when (keep_json opts) $ do
                putStrLn $ "Intermediate JSON file kept at: " ++ json_file
