import Test.HUnit
import System.IO
import System.Directory
import System.IO.Unsafe
import Data.List
import Data.Aeson
import Language.TypeScript.Par
import Language.TypeScript.AST
import Lib
import Control.Monad.State
import Language.TypeScript.ParserCombinators
import Data.Attoparsec.Combinator
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import Data.Either

json_files :: [FilePath]
json_files = unsafePerformIO $ do
    all_files <- listDirectory "example/cases"
    let jsons = filter (\f -> isSuffixOf ".json" f) all_files
    return $ map (\f -> "example/cases/" ++ f) jsons

json_strings :: [BS.ByteString]
json_strings = unsafePerformIO $ mapM BS.readFile json_files

parsed_jsons :: [Maybe TsNode]
parsed_jsons = map decode json_strings



parsed_ts_decls :: [Either TsNode (SourceFile, [TsNode])]
parsed_ts_decls = map (\n -> runStateT pSourceFile [n]) (map fromJust parsed_jsons)

json_parse_ts_node_test = TestCase (assertBool "all parsed into TsNode" (all isJust parsed_jsons))

parsed_ts_decls_all_right = TestCase (assertBool "all parsed into TypeScript AST" (all isRight parsed_ts_decls))

left_tokens = map (snd . fromRight (error "should not left any tokens")) parsed_ts_decls

parsed_ts_decls_all_no_token_left = TestCase (assertBool 
            "all parsed into TypeScript AST" (all null left_tokens))

test_cases = TestList [
        TestLabel "JSON parsed into TsNode" json_parse_ts_node_test,
        TestLabel "TsNode parsed into TypeScript AST" parsed_ts_decls_all_right,
        TestLabel "No token left after parsing" parsed_ts_decls_all_no_token_left
    ]

main :: IO ()
main = do 
    runTestTT test_cases
    return ()
