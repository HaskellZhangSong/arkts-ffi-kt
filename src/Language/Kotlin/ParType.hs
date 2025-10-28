module Language.Kotlin.ParType where


import Language.Kotlin.AST
-- import Control.Applicative

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Combinator
import Data.Maybe (fromJust)
-- import qualified Data.Text.Prettyprint.Doc as Tok
import Debug.Trace
lexer = Tok.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = Tok.lexeme lexer

identifier = Tok.identifier lexer

operator = Tok.operator lexer

angle = Tok.angles lexer

braces = Tok.braces lexer

comma = Tok.comma lexer

parens = Tok.parens lexer

pKotlinType :: Parsec String () KotlinType
pKotlinType = choice [try pGenericType, 
                      try pNullableType, 
                      try pQualifedType, 
                      try pFunctionType, 
                      try pTupleType, 
                      try (RefType <$> identifier)]

pGenericType :: Parsec String () KotlinType
pGenericType = do
    typeName <- identifier
    typeParams <- angle (lexeme (pKotlinType `sepBy` comma))
    return $ GenericType typeName typeParams

pNullableType :: Parsec String () KotlinType
pNullableType = do
    baseType <- choice [try pGenericType, 
                        try pQualifedType,
                        try pFunctionType, 
                        try pTupleType, 
                        (RefType <$> identifier)]
    nullable <- optionMaybe (lexeme (char '?'))
    case nullable of
        Just _  -> return $ NullableType baseType
        Nothing -> return baseType

pQualifedType :: Parsec String () KotlinType
pQualifedType = do    
    quals <- many1 (try (do
        i <- identifier
        _ <- lexeme (char '.')
        return i))
    baseType <- choice [try pGenericType, 
                        try pFunctionType, 
                        try pTupleType, 
                        (RefType <$> identifier)]
    return $ QualifiedType quals baseType

pTupleType :: Parsec String () KotlinType
pTupleType = do
    types <- parens (lexeme (pKotlinType `sepBy` comma))
    return $ TupleType types

pFunctionType :: Parsec String () KotlinType
pFunctionType = do
    paramTypes <- parens (lexeme (pKotlinType `sepBy` comma))
    _ <- lexeme (string "->")
    retType <- pKotlinType
    return $ FunctionType paramTypes retType

pKtType :: String -> KotlinType
pKtType s = let remove_quote = filter (/= '\"') s
            in case parse pKotlinType "" remove_quote of
                Left err -> error $ "Failed to parse Kotlin type: " ++ show err ++ " " ++ show s
                Right kt -> kt