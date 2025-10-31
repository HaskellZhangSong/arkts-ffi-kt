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
import Control.Monad.State
import Control.Applicative
import Language.TypeScript.ParserCombinators
import Data.Attoparsec.Combinator
import Text.Pretty.Simple
pSourceFile :: Parser SourceFile
pSourceFile = do
    pushKindChildren "SourceFile"
    pushKindChildren "SyntaxList"
    -- ignore import declarations
    _ <- many (skipKind "ImportDeclaration")
    decs <- pDecls
    traceM $ "Parsed declarations: " ++ (show $ length decs)
    -- _ <- pop -- pop Eof
    skipKind "EndOfFileToken"
    nodes <- get

    return $ SourceFile $ decs

pDecls :: Parser [Decl]
pDecls = many pDecl

pDecl :: Parser Decl
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

pClassDecl :: Parser ClassD
pClassDecl = do
    ch <- pKindChildren "ClassDeclaration"
    push ch
    has_decorator <- peekKind "SyntaxList"
    decos <- if has_decorator
                then do 
                    pushKindChildren "SyntaxList"
                    many pDecorator
                else return []
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

pDecorator :: Parser Decorator
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
            pushKindChildren "SyntaxList"
            params <- sepBy pDecoratorParam (eat ",")
            eat ")"
            return $ DecoratorPara ident params

pDecoratorParam :: Parser (String, String)
pDecoratorParam = do
    pushKindChildren "BinaryExpression"
    key <- pKindContent "Identifier"
    eat "="
    value <- pKindContent "StringLiteral"
    return (key, value)

pDecorators :: Parser [Decorator]
pDecorators = do
    has_decorator <- peekKind "SyntaxList"
    decos <- if has_decorator
                then do 
                    pushKindChildren "SyntaxList"
                    many pDecorator
                else return []
    return decos

pConstrDecl :: Parser FuncD
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

pFuncParam :: Parser (String, Type)
pFuncParam = do
    pushKindChildren "Parameter"
    ident <- pKindContent "Identifier"
    eat ":"
    ty <- pType
    return (ident, ty)

pType :: Parser Type
pType = do
    n <- pop
    case kind n of
        "NumberKeyword" -> return $ TyRef "number"
        "StringKeyword" -> return $ TyRef "string"
        "BooleanKeyword" -> return $ TyRef "boolean"
        "ArrayType" -> return $ TyArray (TyRef "any")  -- TODO: parse element type
        "TypeReference" -> do
            let ident = fromJust $ content $ head $ fromJust $ children n
            return $ TyRef ident
        _ -> error $ "Unsupported type kind: " ++ show n

pVarDecl :: Parser VarD
pVarDecl = do
    ch <- pKindChildren "PropertyDeclaration"
    push ch
    decos <- pDecorators
    ident <- pKindContent "Identifier"
    eat ":"
    ty <- pType
    -- TODO parse initializer
    return $ VarD {
        varName = ident,
        varType = ty,
        varDecorators = decos
    }

pAbsFuncPart :: FuncType -> [Decorator] -> Parser FuncD
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

pMethodDecl :: Parser FuncD
pMethodDecl = do
    pushKindChildren "MethodDeclaration"
    decos <- pDecorators
    pAbsFuncPart Method decos

pFuncDecl :: Parser FuncD
pFuncDecl = do
    pushKindChildren "FunctionDeclaration"
    decos <- pDecorators
    eat "function"
    pAbsFuncPart Func decos

pInterfaceDecl = undefined
