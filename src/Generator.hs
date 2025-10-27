{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable#-}
{-# OPTIONS_GHC -ddump-splices #-}
module Generator where
import qualified Language.Kotlin.AST as Kt
import Data.Coerce
import Data.Derive.IsDataCon
import Data.Derive.TopDown
import Data.Data
import qualified Data.Text.Prettyprint.Doc as Kt
import Language.TypeScript.AST
import Language.Kotlin.AST
import qualified Language.TypeScript.AST as Ts
import Control.Exception (Deadlock)

convertDecl :: Decl -> Kt.KotlinDeclaration
convertDecl (Ts.FuncDecl f) = Kt.FunctionDecl $ convertFunc f
convertDecl (Ts.VarDecl v) = Kt.PropertyDecl $ convertVar v
convertDecl (Ts.ClassDecl c) = Kt.ClassDecl $ convertClass c

convertClassMember :: Decl -> Kt.ClassMember
convertClassMember (Ts.FuncDecl f) = Kt.ClassFunction $ convertFunc f
convertClassMember (Ts.VarDecl v) = Kt.ClassProperty $ convertVar v
convertClassMember (Ts.ClassDecl v) = error "Nested classes not supported"
convertClassMember _ = error "Unsupported class member"

convertClass :: ClassD -> Kt.Class
convertClass (ClassD decos name superclasses members) = 
    Kt.Class
        { Kt.className = name ++ "Proxy"
        , Kt.classModifiers = []
        , primaryConstructor = Nothing
        , superTypes = []
        , typeParameters = []
        , classBody = map convertClassMember members
        }


convertFunc :: FuncD -> Kt.Function
convertFunc f@(FuncD ty name decos params retTy) =
    Kt.Function
        { Kt.functionName = name
        , Kt.functionModifiers = []
        , functionTypeParameters = []
        , Kt.functionParameters = map (convertParameter decos) params
        , Kt.functionReturnType = Just $ convertType decos retTy
        , Kt.functionBody = Just $ convertFunctionBody f
        }

convertFunctionBody :: FuncD -> Kt.FunctionBody
convertFunctionBody (FuncD funTy name decos params retTy) =
    case funTy of 
        Method -> Kt.BlockBody [ReturnStmt (Just $ CallExpr (IdentifierExpr "BarProxy")  
                                                    [RefType "Reference"]
                                                    [LiteralString name])]
                                                    
convertParameter :: [Decorator] -> (String, Type) -> Kt.Parameter
convertParameter ds (name, ty) =
    Kt.Parameter
        { Kt.parameterName = name
        , Kt.parameterType = convertType ds ty
        , parameterDefaultValue = Nothing
        , parameterModifiers = []
        }

convertType :: [Decorator] -> Ts.Type -> Kt.KotlinType
convertType d (TyRef name) = Kt.RefType name
convertType d (TyNullable ty) = NullableType (convertType d ty)
convertType d _ = error "not implemented"

convertVar :: Ts.VarD -> Kt.Property
convertVar (VarD decos name ty) =
    Kt.Property
        { propertyName = name
        , propertyModifiers = [Kt.M_Var]
        , propertyType = Just $ convertType decos ty
        , propertyInitializer = Nothing
        , propertyGetter = Just $ Kt.PropertyAccessor {
            accessorModifiers = [],
            accessorBody = Just $ ExpressionBody $ CallExpr (IdentifierExpr "getInt") [] [LiteralString name]
        }
        , propertySetter = Just $ Kt.PropertyAccessor {
            accessorModifiers = [],
            accessorBody = Just $ ExpressionBody $ CallExpr (IdentifierExpr "setInt") [] [LiteralString name, IdentifierExpr "value"]
        }
    }