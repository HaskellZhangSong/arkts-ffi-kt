
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
import System.Process
import System.Exit
import System.Console.CmdArgs (opt)

data CmdOptions = CmdOptions {
    inputFile :: FilePath,
    outputFile :: FilePath
} deriving (Show, Eq, Data, Typeable)

options = CmdOptions {
    inputFile = def &= args &= typ "FILES",
    outputFile = def &= help "output file" &= typFile
}

create_temp_file :: IO FilePath
create_temp_file = do
    temp_file <- readCreateProcess (proc "mktemp" ["-t", "arkts-ffi.json"]) ""
    return $ init $ temp_file -- drop \n at the end

commandExists :: String -> IO Bool
commandExists cmd = do
  (exitCode, _, _) <- readProcessWithExitCode "which" [cmd] ""
  return (exitCode == ExitSuccess)

main :: IO ()
main = do
    exists <- commandExists "tsp"
    case exists of
        True -> return ()
        False -> do
            putStrLn "Error: 'tsp' command not found. Please install TypeScript Parser (tsp) to proceed."
            exitFailure
    a <- getArgs
    opts <- cmdArgs $ modes [options]    
    -- use tsp to parse input file
    temp_file <- create_temp_file
    tsp <- readProcessWithExitCode "tsp" [(inputFile opts), "-o", temp_file] ""
    case tsp of
        (ExitSuccess, _, _) -> return ()
        (ExitFailure code, err, _) -> do
            putStrLn $ "tsp failed: " ++ err
            putStrLn $ "json output file: " ++ temp_file
            exitWith (ExitFailure code)
    input <- BS.readFile temp_file
    let ts_node = decode input :: Maybe TsNode
    ast <- case ts_node of
        Nothing -> do
            putStrLn $ "Failed to decode TypeScript AST from JSON for file: " ++ temp_file
            exitFailure
        Just ts -> do
            let parseResult =  par pSourceFile [ts]
            case parseResult of
                Right ts_ast -> return ts_ast
                Left err -> do 
                    putStrLn $ "Parsing failed" ++ show err
                    putStrLn $ "Input JSON: " ++ show temp_file
                    exitFailure
    -- pPrint ast
    let kt_asts = convertSourceFile ast
    -- pPrint kt_asts
    let res = pretty kt_asts
    writeFile (outputFile opts) (show res)
    -- remove json file
    _ <- readProcess "rm" [temp_file] ""
    return ()
