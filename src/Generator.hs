{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, BangPatterns#-}
{-# OPTIONS_GHC -ddump-splices #-}
module Generator where
import qualified Language.Kotlin.AST as Kt
import Language.TypeScript.AST
import Language.Kotlin.AST
import Language.Kotlin.ParType
import qualified Language.TypeScript.AST as Ts
import qualified Data.List as L (lookup, find)
import Data.Maybe (fromJust)
import Debug.Trace


convertSourceFile :: SourceFile -> Kt.KotlinFile
convertSourceFile (SourceFile decls) =
    Kt.KotlinFile
        { Kt.packageDecl = ["arkts", "ffi"]
        , Kt.imports = []
        , Kt.declarations = map convertDecl decls
        }

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
        , Kt.fieldParameters = [Parameter "ref" 
                    (RefType "ArkObjectSaafeReference")  Nothing []]
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
convertType [DecoratorPara n p] (TyRef name) = case L.lookup "type" p of
                                                Nothing -> RefType name
                                                Just t -> pKtType (trace (show t) t)

convertType _ (TyRef "number") = RefType "Double"
convertType _ (TyRef "string") = RefType "String"
convertType _ (TyRef "boolean") = RefType "Boolean"
convertType _ (TyRef "any") = RefType "Any"
convertType d (TyArray ty) = ArrayType (convertType d ty)
convertType d (TyNullable ty) = NullableType (convertType d ty)
convertType d ty = error $ show d ++ " " ++ show ty ++ "not implemented"


defaultType :: Ts.Type -> Kt.KotlinType
defaultType (TyRef "number") = RefType "Double"
defaultType (TyRef "string") = RefType "String"
defaultType (TyRef "boolean") = RefType "Boolean"
defaultType (TyRef "any") = RefType "Any"
defaultType (TyNullable ty) = NullableType (defaultType ty)
defaultType ty = error $ "Unsupported default type for " ++ show ty

convertVar :: Ts.VarD -> Kt.Property
convertVar (VarD decos n ty) =
    let deco_ty = L.find (\d -> case d of
                                DecoratorPara name _ -> name == "KotlinExportField"
                                _ -> False) (trace (show decos) decos)
        kt_ty = case deco_ty of
                    Just (DecoratorPara _ p) -> case L.lookup "type" p of
                                                    Just t -> pKtType t
                                                    Nothing -> error $ "KotlinExportField " ++
                                                                    show p ++ 
                                                                    " missing type parameter"
                    _ -> defaultType ty
        get_fun = if Kt.isPrimType kt_ty 
                    then let fun =  "get" ++ Kt.refTypeName kt_ty 
                            in CallExpr (IdentifierExpr fun)
                                [] [LiteralString n]
                    else CallExpr (IdentifierExpr (refTypeName kt_ty))
                                [] [(CallExpr (IdentifierExpr "getProperty") [] [(LiteralString n)])]

        set_fun = if Kt.isPrimType kt_ty
                    then let fun = "set" ++ Kt.refTypeName kt_ty
                            in CallExpr (IdentifierExpr fun)
                                [] [LiteralString n, IdentifierExpr n]
                    else CallExpr (IdentifierExpr ("setProperty"))
                                [] [LiteralString n, IdentifierExpr "value"]

        getter = ExpressionBody $ get_fun
        setter = ExpressionBody $ set_fun
                                in
    Kt.Property
        { propertyName = n
        , propertyModifiers = [Kt.M_Var]
        , propertyType = Just $ convertType decos ty
        , propertyInitializer = Nothing
        , propertyGetter = Just $ Kt.PropertyAccessor {
            accessorModifiers = [],
            accessorBody = Just $ getter
        }
        , propertySetter = Just $ Kt.PropertyAccessor {
            accessorModifiers = [],
            accessorBody = Just $ setter
        }
    }