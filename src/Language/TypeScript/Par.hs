{-# LANGUAGE OverloadedStrings #-}
module Language.TypeScript.Par where

import Language.TypeScript.AST
import Lib
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (find)
import qualified Data.Text as T
import Data.Maybe
import Debug.Trace
import Data.Either
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Language.TypeScript.ParserCombinators
import Data.Attoparsec.Combinator
import Text.Pretty.Simple
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import GHC.Stack
import GHC.RTS.Flags (ProfFlags(retainerSelector))

pSourceFile :: HasCallStack => Parser SourceFile
pSourceFile = do
    pushKindChildren "SourceFile"
    pushKindChildren "SyntaxList"
    -- ignore import declarations
    -- _ <- many (skipKind "ImportDeclaration")
    decs <- pDecls
    skipKind "EndOfFileToken"
    return $ SourceFile $ decs

pDecls :: HasCallStack => Parser [Decl]
pDecls = many pDecl

pDecl :: HasCallStack => Parser Decl
pDecl = do
    n <- peek
    case kind n of
        "ClassDeclaration" -> do
            class_decl <- pClassDecl
            return $ ClassDecl class_decl
        "FunctionDeclaration" -> do
            func_decl <- pFuncDecl
            return $ FuncDecl func_decl
        "PropertyDeclaration" -> do
            var_decl <- pVarDecl
            return $ VarDecl var_decl
        "InterfaceDeclaration" -> do
            interface_decl <- pInterfaceDecl
            return $ ClassDecl interface_decl
        "MethodDeclaration" -> do
            method_decl <- pMethodDecl
            return $ FuncDecl method_decl
        "Constructor" -> do
            constr_decl <- pConstrDecl
            return $ FuncDecl constr_decl
        _ -> do
            lift $ Left n

has_decorator :: HasCallStack => Parser Bool
has_decorator = do 
    is_syntax_list <- peekKind "SyntaxList"
    if is_syntax_list
        then do
            n <- peek
            let maybe_children = children n
            case maybe_children of
                Nothing -> error $ "Expected children for SyntaxList, but got: " ++ show n
                Just chs -> do
                    let has_deco = any (\c -> kind c == "Decorator") chs
                    return has_deco
        else return False

pClassDecl :: HasCallStack => Parser ClassD
pClassDecl = do
    ch <- pKindChildren "ClassDeclaration"
    push ch
    hd <- has_decorator
    decos <- if hd
                then do 
                    pushKindChildren "SyntaxList"
                    a <- many pDecorator
                    void $ many $ eat "export"
                    return a
                else do
                    void $ many $ eat "export"
                    return []
    eat "class"
    ident <- pKindContent "Identifier"
    -- TODO parse super classes
    eat "{"
    class_body <- pKindNode "SyntaxList"
    class_members <- case children class_body of
                        Nothing -> return []
                        Just class_members -> do
                                        push class_members
                                        mem_decls <- pDecls
                                        return mem_decls
    eat "}"
    return $ ClassD {
        className = ident,
        classDecorators = decos,
        classMembers = class_members,
        classSuperClasses = []
    }

pDecorator :: HasCallStack => Parser Decorator
pDecorator = do
    pushKindChildren "Decorator"
    eat "@"
    isIdent <- peekKind "Identifier"
    if isIdent
        then do
            ident <- pKindContent "Identifier"
            return $ Decorator ident
        else do
            pushKindChildren "CallExpression"
            ident <- pKindContent "Identifier"
            eat "("
            sl <- peek
            case children sl of
                Nothing -> do
                    skip
                    eat ")"
                    return $ DecoratorPara ident []
                Just _ -> do
                    pushKindChildren "SyntaxList"
                    pushKindChildren "ObjectLiteralExpression"
                    eat "{"
                    pushKindChildren "SyntaxList"
                    params <- sepBy pDecoratorParam (eat ",")
                    eat "}"
                    eat ")"
                    return $ DecoratorPara ident params

pDecoratorParam :: HasCallStack => Parser (String, String)
pDecoratorParam = do
    pushKindChildren "PropertyAssignment"
    key <- pKindContent "StringLiteral"
    eat ":"
    value <- pKindContent "StringLiteral"
    return (filter (/= '\"') key, filter (/= '\"') value)
    

pDecorators :: HasCallStack => Parser [Decorator]
pDecorators = do
    hd <- has_decorator
    decos <- if hd
                then do 
                    pushKindChildren "SyntaxList"
                    many pDecorator
                else return []
    return decos

pConstrDecl :: HasCallStack => Parser FuncD
pConstrDecl = do
    pushKindChildren "Constructor"
    eat "constructor"
    eat "("
    pushKindChildren "SyntaxList"
    params <- sepBy pFuncParam (eat ",")
    eat ")"
    skipKind "Block"
    return $ FuncD {
        funcType = Constr,
        funcName = "constructor",
        funcDecorators = [],
        funcParams = params ,
        -- Not useful, just dummy
        funcReturnType = TyRef "Unit"
    }

pFuncParam :: HasCallStack => Parser (String, Type)
pFuncParam = do
    pushKindChildren "Parameter"
    ident <- pKindContent "Identifier"
    eat ":"
    ty <- pType
    return (ident, ty)

pType :: HasCallStack => Parser Type
pType = do
    n <- pop
    case kind n of
        "NumberKeyword" -> return $ TyRef "number"
        "StringKeyword" -> return $ TyRef "string"
        "BooleanKeyword" -> return $ TyRef "boolean"
        "BigIntKeyword" -> return $ TyRef "bigint"
        "ArrayType" -> return $ TyArray (TyRef "any")  -- TODO: parse element type
        "TypeReference" -> do
            let ident = fromJust $ content $ head $ fromJust $ children n
            return $ TyRef ident
        _ -> error $ "Unsupported type kind: " ++ show n

pModifiers :: HasCallStack => Parser [Modifier]
pModifiers = do
    mods <- many $ do
        mod_kind <- peek
        case kind mod_kind of
            "PublicKeyword" -> eat "public" >> return Public
            "PrivateKeyword" -> eat "private" >> return Private
            "ProtectedKeyword" -> eat "protected" >> return Protected
            "StaticKeyword" -> eat "static" >> return Static
            "ReadonlyKeyword" -> eat "readonly" >> return Readonly
            _ -> lift $ Left (error $ "Unexpected modifier: " ++ show mod_kind)
    return mods

pVarDecl :: HasCallStack => Parser VarD
pVarDecl = do
    pushKindChildren "PropertyDeclaration"
    decos <- pDecorators
    md <- peek
    let is_syntax_list = kind md == "SyntaxList"
    mods <- if is_syntax_list 
                then do
                    pushKindChildren "SyntaxList"
                    pModifiers
                else return []

    ident <- pKindContent "Identifier"
    i <- peek
    let nullable = kind i == "QuestionToken"
    when nullable (eat "?")
    eat ":"
    
    ty <- pType
    eq <- peek
    when (kind eq == "FirstAssignment") $ do 
                                    eat "="
                                    -- skip expression of var
                                    skip
    void $ many (eat ";")
    -- TODO parse initializer
    return $ VarD {
        varDecorators = decos,
        varModifier = mods,
        varName = ident,
        varType = if nullable then TyNullable ty else ty
    }

pAbsFuncPart :: HasCallStack => FuncType -> [Decorator] -> Parser FuncD
pAbsFuncPart ft decos = do
    ident <- pKindContent "Identifier"
    eat "("
    pushKindChildren "SyntaxList"
    params <- sepBy pFuncParam (eat ",")
    eat ")"
    eat ":"
    ret_type <- pType
    skipKind "Block"
    return $ FuncD {
        funcType = ft,
        funcName = ident,
        funcDecorators = decos,
        funcParams = params,
        funcReturnType = ret_type
    }

pMethodDecl :: HasCallStack => Parser FuncD
pMethodDecl = do
    pushKindChildren "MethodDeclaration"
    decos <- pDecorators
    pAbsFuncPart Method decos

pFuncDecl :: HasCallStack => Parser FuncD
pFuncDecl = do
    pushKindChildren "FunctionDeclaration"
    decos <- pDecorators
    eat "function"
    
    pAbsFuncPart Func decos

pInterfaceDecl = undefined

parJson :: HasCallStack => BS.ByteString -> Either TsNode SourceFile
parJson input =
    case decode input :: Maybe TsNode of
        Nothing -> Left (error "Failed to decode TypeScript AST from JSON")
        Just ts -> par pSourceFile [ts]

parJsonFile :: HasCallStack => FilePath -> IO (Either TsNode SourceFile)
parJsonFile json_file = do
    input <- BS.readFile json_file
    return $ parJson input