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
convertClassMember (Ts.ClassDecl v) = error $ "Nested classes not supported" ++ show v

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
convertFunc f@(FuncD _ name decos params ret_ty) =
    let deco_fun = findDecorator "KotlinExportFunction" decos
        in case deco_fun of
            Just (DecoratorPara _ ps) -> 
                if length ps /= length params + 1
                    then error $ "KotlinExportFunction decorator parameter length "
                                 ++ show (length ps)
                                 ++ " does not match function parameter + return type length "
                                 ++ show (length params)
                    else
                Kt.Function
                { Kt.functionName = name
                , Kt.functionModifiers = []
                , functionTypeParameters = []
                , Kt.functionParameters = map (\(ts, kt) -> constructParam ts kt) (zip params ps)
                , Kt.functionReturnType = Just $ let (_, kt) = last ps 
                                                     parsed_kt_ty = pKtType kt
                                                in  parsed_kt_ty
                , Kt.functionBody = Just $ convertFunctionBody f
                }
            Nothing -> Kt.Function
                { Kt.functionName = name
                , Kt.functionModifiers = []
                , functionTypeParameters = []
                , Kt.functionParameters = map (convertParameter decos) params
                , Kt.functionReturnType = Just $ defaultType ret_ty
                , Kt.functionBody = Just $ convertFunctionBody f
                }
            _ -> error $ "KotlinExportFunction decorator missing parameters"

convertFunctionBody :: FuncD -> Kt.FunctionBody
convertFunctionBody (FuncD fun_ty name decos params ret_ty) =
    let deco_ret_kt_ty = findDecorator "KotlinExportFunction" decos
        para_kt_nm_tys = case deco_ret_kt_ty of
                            Just (DecoratorPara _ ps) -> map (\((_, deco_t), (n, _)) -> (n, pKtType deco_t)) (zip (init ps) params)
                            _ -> map (\(n,t) -> (n, defaultType t)) params
        ret_kt_ty = case deco_ret_kt_ty of
                        Just (DecoratorPara _ ps) -> let (_, kt) = last ps 
                                                        in  pKtType kt
                        _ -> defaultType ret_ty
    in
    case fun_ty of
        Method -> if Kt.isPrimType ret_kt_ty
                    then Kt.BlockBody $ [ExpressionStmt $ CallExpr (IdentifierExpr "callMethod")
                                                    [RefType (refTypeName ret_kt_ty)]
                                                    -- if Argument is primitive, pass value directly
                                                    -- if Argument is reference, pass x.ref
                                                    (LiteralString name : map (\(param_nm, kt_ty) -> 
                                                        if Kt.isPrimType kt_ty
                                                            then IdentifierExpr param_nm
                                                            else MemberExpr (IdentifierExpr param_nm) "ref") 
                                                        para_kt_nm_tys)]
                    else Kt.BlockBody $ [ExpressionStmt $ 
                                                    CallExpr (IdentifierExpr (refTypeName ret_kt_ty ++ "Proxy")) []  
                                                    [(CallExpr (IdentifierExpr "callMethod")
                                                    [RefType "ArkObjectSafeReference"]
                                                    (LiteralString name : 
                                                        map (\(param_nm, kt_ty) -> 
                                                        if Kt.isPrimType kt_ty
                                                            then IdentifierExpr param_nm
                                                            else MemberExpr (IdentifierExpr param_nm) "ref") 
                                                        para_kt_nm_tys
                                                    
                                                    ))]]
        _ -> error $ "Only Method function type is supported in function body generation"

constructParam :: (String, Ts.Type) -> (String, String) -> Kt.Parameter
constructParam (name, _) (_, kt_ty) = 
    let parsed_kt_ty = pKtType kt_ty in
    Kt.Parameter
        { Kt.parameterName = name
        , Kt.parameterType = parsed_kt_ty
        , parameterDefaultValue = Nothing
        , parameterModifiers = []
        }

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
                                                Just t -> pKtType t

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
defaultType (TyRef ref) = RefType $ ref ++ "Proxy"
defaultType (TyNullable ty) = NullableType (defaultType ty)
defaultType ty = error $ "Unsupported default type for " ++ show ty

findDecorator :: String -> [Decorator] -> Maybe Decorator
findDecorator name decos =
    L.find (\d -> case d of
                    Decorator n -> n == name
                    DecoratorPara n _ -> n == name
                    _ -> False) decos

convertVar :: Ts.VarD -> Kt.Property
convertVar (VarD decos n ty) =
    let deco_ty = findDecorator "KotlinExportField" decos
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