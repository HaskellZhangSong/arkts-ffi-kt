{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.TypeScript.AST where
import Data.Derive.IsDataCon
data Decorator = Decorator {
    name :: String
} | DecoratorPara {
    name :: String,
    para :: [(String, String)]
} deriving (Show, Eq, Ord)

data SourceFile = SourceFile [Decl]
    deriving (Show, Eq, Ord)

data Type = 
      TyRef { typeName :: String }
    | TyApp String [Type]
    | TyFun [Type] Type
    | TyNullable Type
    | TyArray Type
    | TyUnion [Type]
    | TyIntersection [Type]
    | TyTuple [Type]
    deriving (Show, Eq, Ord)

isPrimType :: Type -> Bool
isPrimType (TyRef "number") = True
isPrimType (TyRef "string") = True
isPrimType (TyRef "boolean") = True
isPrimType _ = False



data Decl = ClassDecl ClassD
          | FuncDecl FuncD
          | VarDecl VarD
          deriving (Show, Eq, Ord)

data ClassD = ClassD {
    classDecorators :: [Decorator],
    className :: String,
    classSuperClasses :: [Type],
    classMembers :: [Decl]
} deriving (Show, Eq, Ord)

data FuncType = Func | Method | Constr
    deriving (Show, Eq, Ord)

data FuncD = FuncD {
    funcType :: FuncType,
    funcName :: String,
    funcDecorators :: [Decorator],
    funcParams :: [(String, Type)],
    funcReturnType :: Type
} deriving (Show, Eq, Ord)

data Modifier = Public | Private | Protected | Static | Readonly
    deriving (Show, Eq, Ord)

-- I do not need exprs
data VarD = VarD {
    varDecorators :: [Decorator],
    varModifier :: [Modifier],
    varName :: String,
    varType :: Type
} deriving (Show, Eq, Ord)

derive_is ''Type