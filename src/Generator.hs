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
import Options
import System.Console.CmdArgs (CmdArgs)
import Debug.Trace
import Language.Kotlin.AST (getRefTypeName)

-- Should check if decorator is corectly
hasDecorator :: Decl -> Bool
hasDecorator (Ts.FuncDecl (Ts.FuncD _ _ [] _ _)) = False
hasDecorator (Ts.VarDecl (Ts.VarD [] [] _ _)) = False
hasDecorator (Ts.ClassDecl (Ts.ClassD [] _ _ _)) = False
hasDecorator _ = True

convertSourceFile :: CmdOptions -> SourceFile -> Kt.KotlinFile
convertSourceFile opts (SourceFile decls) =
    Kt.KotlinFile
        { Kt.annotations = ["@file:OptIn(ExperimentalForeignApi::class)"]
        , Kt.packageDecl = package_name opts
        , Kt.imports = [Kt.Import "com.bytedance.kmp.ohos_ffi.types.FFIProxy"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.types.ArkObjectSafeReference"
                       , Kt.Import "platform.ohos.napi.*"
                       , Kt.Import "kotlinx.cinterop.*"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.annotation.ArkTsExportClass"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.annotation.ArkTsExportCustomTransform"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.transform.ArkTsExportCustomTransformer"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.types.ArkModule"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.types.transformer.IntTypeTransformer"
                       , Kt.Import "com.bytedance.kmp.ohos_ffi.types.transformer.ListTypeTransformer"
                       ]
        , Kt.declarations = concatMap convertDecl (filter hasDecorator decls)
        }

convertDecl :: Decl -> [Kt.KotlinDeclaration]
convertDecl (Ts.FuncDecl f) = [Kt.FunctionDecl $ convertFunc f]
convertDecl (Ts.VarDecl v) = [Kt.PropertyDecl $ convertVar v]
convertDecl (Ts.ClassDecl c) = [Kt.ClassDecl $ convertClass c] ++
                                [Kt.ObjectDecl $ generateProxyTransformer c]

convertClassMember :: Decl -> Kt.ClassMember
convertClassMember (Ts.FuncDecl f) = Kt.ClassFunction $ convertFunc f
convertClassMember (Ts.VarDecl v) = Kt.ClassProperty $ convertVar v
convertClassMember (Ts.ClassDecl v) = error $ "Nested classes not supported" ++ show v

generateClassProxyDefaultConstructor :: ClassD -> Kt.ClassMember
generateClassProxyDefaultConstructor (ClassD decos name _ _) =
    let module_path = getArkModulePath decos
    in 
    Kt.ClassConstructor $ Kt.Constructor
        { Kt.constructorParameters = []
        , Kt.constructorBody = Just $ Kt.BlockBody [
            Kt.DeclarationAssignmentStmt Kt.M_Var "module" (Kt.CallExpr (Kt.IdentifierExpr "ArkModule") [] [Kt.LiteralString module_path]),
            Kt.DeclarationAssignmentStmt Kt.M_Var "clazz" (Kt.CallExpr (Kt.MemberExpr (Kt.IdentifierExpr "module") "getExportClass") [] [Kt.LiteralString name]),
            Kt.DeclarationAssignmentStmt Kt.M_Var "instance" (Kt.CallExpr (Kt.MemberExpr (Kt.IdentifierExpr "clazz") "newInstance") [] []),
            Kt.AssignmentStmt (Kt.MemberExpr Kt.ThisExpr "ref") (Kt.CallExpr (Kt.IdentifierExpr "ArkObjectSafeReference") []
                                                [Kt.CallExpr (Kt.MemberExpr (Kt.IdentifierExpr "instance") "getNapiValue") [] []])
        ]}

generateClassProxyConstructor :: ClassD -> Kt.ClassMember
generateClassProxyConstructor (ClassD _ name _ _) = 
    Kt.ClassConstructor $ Kt.Constructor
        { Kt.constructorParameters = [
            Kt.Parameter
                { Kt.parameterName = "ref"
                , Kt.parameterType = Kt.RefType "ArkObjectSafeReference"
                , Kt.parameterDefaultValue = Nothing
                , Kt.parameterModifiers = []
                }
            ]
        , Kt.constructorBody = Just $ Kt.BlockBody [
            Kt.AssignmentStmt (Kt.MemberExpr Kt.ThisExpr "ref") (Kt.IdentifierExpr "ref")
        ]}

convertClass :: ClassD -> Kt.Class
convertClass c@(ClassD decos name superclasses members) =
    let ref_var_decl = Kt.Property
            { Kt.propertyName = "ref"
            , Kt.propertyModifiers = [Kt.M_Val]
            , Kt.propertyType = Just $ Kt.RefType "ArkObjectSafeReference"
            , Kt.propertyInitializer = Nothing
            , Kt.propertyGetter = Nothing
            , Kt.propertySetter = Nothing
            }
    in Kt.Class
        { Kt.classAnnotations = [Annotation "ArkTsExportClass" ["customTransform = true"]]
        , Kt.className = name ++ "Proxy"
        , Kt.classModifiers = []
        , Kt.classFieldParameters = []
        , classSuperTypes = []
        , classTypeParameters = []
        , classBody = [ClassProperty ref_var_decl,
                       generateClassProxyDefaultConstructor c,
                       generateClassProxyConstructor c] ++ map convertClassMember (filter (not . isStaticDecl) members)
        }

generateProxyTransformer :: Ts.ClassD -> Kt.Object
generateProxyTransformer (Ts.ClassD _ name _ _) =
    let class_proxy_name = name ++ "Proxy"
        object = Kt.Object {
            Kt.objectAnnotations = [Annotation "ArkTsExportCustomTransform" 
                                        [class_proxy_name ++ "::class"]],
            Kt.objectName = class_proxy_name ++ "Transformer",
            Kt.objectModifiers = [],
            Kt.objectParameters = [],
            Kt.objectSuperTypes = [Kt.AppType
                                        (Kt.RefType "ArkTsExportCustomTransformer")
                                        [Kt.RefType class_proxy_name]],
            Kt.objectMembers = [
                Kt.ClassFunction $ Kt.Function
                    { Kt.functionName = "fromJsObject"
                    , Kt.functionModifiers = [Kt.Override]
                    , Kt.functionTypeParameters = []
                    , Kt.functionParameters = [Kt.Parameter
                                                { Kt.parameterName = "obj"
                                                , Kt.parameterType = Kt.RefType "napi_value"
                                                , Kt.parameterDefaultValue = Nothing
                                                , Kt.parameterModifiers = []
                                                }]
                    , Kt.functionReturnType = Just $ Kt.RefType class_proxy_name
                    , Kt.functionBody = Just $ Kt.BlockBody
                        [ Kt.ReturnStmt (Just $
                            Kt.CallExpr (Kt.IdentifierExpr class_proxy_name) []
                                [Kt.CallExpr (Kt.IdentifierExpr "ArkObjectSafeReference") []
                                    [Kt.IdentifierExpr "obj"]])
                        ]
                    },
                Kt.ClassFunction $ Kt.Function
                    { Kt.functionName = "toJsObject"
                    , Kt.functionModifiers = [Kt.Override]
                    , Kt.functionTypeParameters = []
                    , Kt.functionParameters = [Kt.Parameter
                                                { Kt.parameterName = "obj"
                                                , Kt.parameterType = Kt.RefType class_proxy_name
                                                , Kt.parameterDefaultValue = Nothing
                                                , Kt.parameterModifiers = []
                                                }]
                    , Kt.functionReturnType = Just $ Kt.RefType "napi_value"
                    , Kt.functionBody = Just $ Kt.BlockBody
                        [ Kt.ReturnStmt (Just $
                            Kt.MemberExpr
                                (Kt.MemberExpr (Kt.IdentifierExpr "obj") "ref")
                                "handle")
                        ]
                    }
                ]
        }
        in object

convertFunc :: FuncD -> Kt.Function
convertFunc f@(FuncD _ name decos params ret_ty) =
    let deco_fun = findDecorator "ExportKotlinFunction" decos
        in case deco_fun of
            Just d ->
                Kt.Function
                { Kt.functionName = name
                , Kt.functionModifiers = []
                , functionTypeParameters = []
                , Kt.functionParameters = map (convParam d) params
                , Kt.functionReturnType = Just $ if null decos
                                                    then defaultType ret_ty
                                                    else convReturnType (head decos) ret_ty
                , Kt.functionBody = Just $ convertFunctionBody f
                }
            t -> error $ "ExportKotlinFunction decorator missing parameters " ++ show t

convertFunctionBody :: FuncD -> Kt.FunctionBody
convertFunctionBody f@(FuncD fun_ty _ _ _ _) =
    case fun_ty of
        Method -> convertMethodBody f
        Func -> convertGlobalFuncBody f
        _ -> error $ "Only Func and Method function types are supported in function body generation"

wrapBangBang :: Kt.Expression -> Kt.Expression
wrapBangBang expr = Kt.OpPostfix expr Kt.BangBang

convertMethodBody :: FuncD -> Kt.FunctionBody
convertMethodBody f@(FuncD Method name decos params ret_ty) =
        let deco_ret_kt_ty = findDecorator "ExportKotlinFunction" decos
            para_kt_nm_tys = case deco_ret_kt_ty of
                            Just (DecoratorPara _ ps) -> map (\((_, deco_t), (n, _)) -> (n, pKtType deco_t)) (zip (init ps) params)
                            _ -> map (\(n,t) -> (n, defaultType t)) params
            ret_kt_ty = let t = convReturnType (head decos) ret_ty in t
            common_method_expr = CallExpr (MemberExpr (IdentifierExpr "ref") "callMethod")
                                                    [RefType (getRefTypeName ret_kt_ty)]
                                                    -- if Argument is primitive, pass value directly
                                                    -- if Argument is reference, pass x.ref
                                                    (LiteralString name : map (\(param_nm, kt_ty) ->
                                                        if Kt.isPrimType kt_ty
                                                            then IdentifierExpr param_nm
                                                            else MemberExpr (IdentifierExpr param_nm) "ref")
                                                        para_kt_nm_tys)
            is_nullable = isTyNullable ret_ty
            conmmon_method_expr_nullable = if is_nullable
                                            then common_method_expr
                                            else wrapBangBang common_method_expr
                    in if (Kt.isPrimType ret_kt_ty)
                    then Kt.BlockBody $ [ReturnStmt $ Just $ conmmon_method_expr_nullable]
                    else Kt.BlockBody $ [ReturnStmt $ Just $
                                                    CallExpr (IdentifierExpr (getRefTypeName ret_kt_ty)) []
                                                    [conmmon_method_expr_nullable]]
convertMethodBody _ = error $ "Only Method function type is supported in method function body generation"

getArkModulePath :: [Decorator] -> String
getArkModulePath (d:ds) =
    case d of
        (DecoratorPara _ p) ->
            case L.lookup "ark_module_path" p of
                Just path -> path
                Nothing -> "<no_module_path>"
        _ -> "<no_module_path>"


convertGlobalFuncBody :: FuncD -> Kt.FunctionBody
convertGlobalFuncBody (FuncD Func name decos params ret_ty) =
    let ark_module_path = getArkModulePath decos
        common_body = [
                DeclarationAssignmentStmt M_Val "mod" (CallExpr (IdentifierExpr "ArkModule")  [] [(LiteralString ark_module_path)]),
                DeclarationAssignmentStmt M_Val "entry" (CallExpr (IdentifierExpr "ArkObjectSafeReference") []
                                                [CallExpr (MemberExpr (IdentifierExpr "mod") "getNapiValue") [] []]),
                DeclarationAssignmentStmt M_Val "func" (CallExpr (MemberExpr (IdentifierExpr "entry") "get")  [] [LiteralString name])]
        return_type = (defaultType ret_ty)
        -- (func!!.invoke<ArkObjectSafeReference>())!!
        ret_stmt = (OpPostfix
                            (CallExpr
                                (MemberExpr
                                    (OpPostfix (IdentifierExpr "func") BangBang)
                                    "invoke")
                                [RefType "ArkObjectSafeReference"]
                                (map (\(param_nm, kt_ty) ->
                                        if Kt.isPrimType kt_ty
                                            then IdentifierExpr param_nm
                                            else MemberExpr (IdentifierExpr param_nm) "ref")
                                    (map (\(n,t) -> (n, defaultType t)) params)))
                            BangBang)

        in if Kt.isPrimType return_type
            then Kt.BlockBody $ common_body ++ [ReturnStmt (Just ret_stmt)]
            else Kt.BlockBody $ common_body ++ [
                    ExpressionStmt $
                        CallExpr (IdentifierExpr (getRefTypeName return_type ++ "Proxy")) [] 
                        [ret_stmt]]
convertGlobalFuncBody _ = error $ "Only Func function type is supported in global function body generation"

convReturnType :: Decorator -> Ts.Type -> Kt.KotlinType
convReturnType (DecoratorPara "ExportKotlinFunction" p) ts_type =
    case L.lookup "return_" p of
        Just kt_ty ->
            let parsed_kt_ty = pKtType kt_ty in
            parsed_kt_ty
        Nothing -> defaultType ts_type
convReturnType _ ty = defaultType ty


convParam :: Decorator -> (String, Ts.Type) -> Kt.Parameter
convParam (DecoratorPara "ExportKotlinFunction" p) (ts_name, ts_type) =
    case L.lookup ts_name p of
        Just kt_ty ->
            let parsed_kt_ty = pKtType kt_ty in
            Kt.Parameter
                { Kt.parameterName = ts_name
                , Kt.parameterType = parsed_kt_ty
                , parameterDefaultValue = Nothing
                , parameterModifiers = []
                }
        Nothing -> Kt.Parameter
                { Kt.parameterName = ts_name
                , Kt.parameterType = defaultType ts_type
                , parameterDefaultValue = Nothing
                , parameterModifiers = []
                }
convParam _ (ts_name, ty) = Kt.Parameter
                { Kt.parameterName = ts_name
                , Kt.parameterType = defaultType ty
                , parameterDefaultValue = Nothing
                , parameterModifiers = []
                }

convertType :: [Decorator] -> Ts.Type -> Kt.KotlinType
convertType [DecoratorPara _ p] (TyRef nm) = case L.lookup "type" p of
                                                Nothing -> RefType nm
                                                Just t -> pKtType t

convertType _ (TyRef "number") = RefType "Double"
convertType _ (TyRef "bigint") = RefType "Long"
convertType _ (TyRef "string") = RefType "String"
convertType _ (TyRef "boolean") = RefType "Boolean"
convertType _ (TyRef "any") = RefType "Any"
convertType _ (TyRef r) = RefType $ r ++ "Proxy"
convertType d (TyArray ty) = ArrayType (convertType d ty)
convertType d (TyNullable ty) = NullableType (convertType d ty)
convertType d ty = error $ show d ++ " " ++ show ty ++ "not implemented"

defaultType :: Ts.Type -> Kt.KotlinType
defaultType (TyRef "number") = RefType "Double"
defaultType (TyRef "bigint") = RefType "Long"
defaultType (TyRef "string") = RefType "String"
defaultType (TyRef "boolean") = RefType "Boolean"
defaultType (TyRef "any") = RefType "Any"
defaultType (TyRef ref) = RefType $ ref ++ "Proxy"
defaultType (TyNullable ty) = NullableType (defaultType ty)
defaultType ty = error $ "Unsupported default type for " ++ show ty

findDecorator :: String -> [Decorator] -> Maybe Decorator
findDecorator nm decos =
    L.find (\d -> case d of
                    Decorator n -> n == nm
                    DecoratorPara n _ -> n == nm) 
                    decos

convertVar :: Ts.VarD -> Kt.Property
convertVar (VarD decos _ n ty) =
    let deco_ty = findDecorator "ExportKotlinField" decos
        kt_ty = case deco_ty of
                    Just (DecoratorPara _ p) -> case L.lookup "type" p of
                                                    Just t -> pKtType t
                                                    Nothing -> error $ "ExportKotlinField " ++
                                                                    show p ++
                                                                    " missing type parameter"
                    _ -> defaultType ty
        get_fun = if Kt.isPrimType kt_ty
                    then let fun = ("get" ++ Kt.getRefTypeName kt_ty)
                            in CallExpr (MemberExpr (IdentifierExpr "ref") fun)
                                [] [LiteralString n]
                    else CallExpr (IdentifierExpr (getRefTypeName kt_ty))
                                [] [(CallExpr (IdentifierExpr "getProperty") [] [(LiteralString n)])]

        set_fun = if Kt.isPrimType kt_ty
                    then let fun = "set" ++ Kt.getRefTypeName kt_ty
                            in ExpressionStmt $ CallExpr (MemberExpr (IdentifierExpr "ref") fun)
                                [] [LiteralString n, IdentifierExpr "value"]
                    else ExpressionStmt $ CallExpr (IdentifierExpr ("setProperty"))
                                [] [LiteralString n, IdentifierExpr "value"]

        getter = ExpressionBody $ if isTyNullable ty then get_fun else wrapBangBang get_fun
        setter = BlockBody [set_fun]
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