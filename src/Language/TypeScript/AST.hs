{-# LANGUAGE DeriveGeneric #-}
module Language.TypeScript.AST where
import GHC.Generics (Generic)
data Decorator = Decorator {
    name :: String
} | DecoratorPara {
    name :: String,
    para :: [(String, String)]
} deriving (Show, Eq, Ord, Generic)

data SourceFile = SourceFile [Decl]
    deriving (Show, Eq, Ord, Generic)

data Type = 
      TyRef { typeName :: String }
    | TyApp String [Type]
    | TyFun [Type] Type
    | TyNullable Type
    | TyArray Type
    | TyUnion [Type]
    | TyIntersection [Type]
    | TyTuple [Type]
    deriving (Show, Eq, Ord, Generic)

isPrimType :: Type -> Bool
isPrimType (TyRef "number") = True
isPrimType (TyRef "string") = True
isPrimType (TyRef "boolean") = True
isPrimType _ = False

data Decl = ClassDecl ClassD
          | FuncDecl FuncD
          | VarDecl VarD
          deriving (Show, Eq, Ord, Generic)

data ClassD = ClassD {
    classDecorators :: [Decorator],
    className :: String,
    classSuperClasses :: [Type],
    classMembers :: [Decl]
} deriving (Show, Eq, Ord, Generic)

data FuncType = Func | Method | Constr
    deriving (Show, Eq, Ord, Generic)

data FuncD = FuncD {
    funcType :: FuncType,
    funcName :: String,
    funcDecorators :: [Decorator],
    funcParams :: [(String, Type)],
    funcReturnType :: Type
} deriving (Show, Eq, Ord, Generic)


-- I do not need exprs
data VarD = VarD {
    varDecorators :: [Decorator],
    varName :: String,
    varType :: Type
} deriving (Show, Eq, Ord, Generic)

