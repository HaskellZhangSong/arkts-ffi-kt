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

-- Should check if decorator is corectly
hasDecorator :: Decl -> Bool
hasDecorator (Ts.FuncDecl (Ts.FuncD _ _ [] _ _)) = False
hasDecorator (Ts.VarDecl (Ts.VarD [] _ _)) = False
hasDecorator (Ts.ClassDecl (Ts.ClassD [] _ _ _)) = False
hasDecorator _ = True

convertSourceFile :: SourceFile -> Kt.KotlinFile
convertSourceFile (SourceFile decls) =
    Kt.KotlinFile
        { Kt.packageDecl = ["arkts", "ffi"]
        , Kt.imports = []
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

convertClass :: ClassD -> Kt.Class
convertClass (ClassD decos name superclasses members) =
    Kt.Class
        { Kt.className = name ++ "Proxy"
        , Kt.classModifiers = []
        , Kt.classFieldParameters = [Parameter "ref"
                    (RefType "ArkObjectSafeReference")  Nothing []]
        , primaryConstructor = Nothing
        , classSuperTypes = []
        , classTypeParameters = []
        , classBody = map convertClassMember members
        }

generateProxyTransformer :: Ts.ClassD -> Kt.Object
generateProxyTransformer (Ts.ClassD _ name _ _) =
{-
object BarProxyTransformer : ArkTsExportCustomTransformer<BarProxy> {
    override fun fromJsObject(obj: napi_value): BarProxy {
        return BarProxy(ArkObjectSafeReference(obj))
    }

    override fun toJsObject(obj: BarProxy): napi_value {
        return obj.ref.handle!!
    }
}
-}
    let class_proxy_name = name ++ "Proxy"
        object = Kt.Object {
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
                                "handle" `Kt.OpPostfix` Kt.BangBang)
                        ]
                    }
                ]
        }
        in object

convertFunc :: FuncD -> Kt.Function
convertFunc f@(FuncD _ name decos params ret_ty) =
    let deco_fun = findDecorator "ExportKotlinFunction" decos
        in case deco_fun of
            Just d@(DecoratorPara _ _) ->
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
            Just d@(Decorator _) -> Kt.Function
                { Kt.functionName = name
                , Kt.functionModifiers = []
                , functionTypeParameters = []
                , Kt.functionParameters = map (convParam d) params
                , Kt.functionReturnType = Just $  if null decos
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

convertMethodBody :: FuncD -> Kt.FunctionBody
convertMethodBody f@(FuncD Method name decos params ret_ty) =
        let deco_ret_kt_ty = findDecorator "ExportKotlinFunction" decos
            para_kt_nm_tys = case deco_ret_kt_ty of
                            Just (DecoratorPara _ ps) -> map (\((_, deco_t), (n, _)) -> (n, pKtType deco_t)) (zip (init ps) params)
                            _ -> map (\(n,t) -> (n, defaultType t)) params
            ret_kt_ty = case deco_ret_kt_ty of
                        Just (DecoratorPara _ ps) -> let (_, kt) = last ps
                                                        in  pKtType kt
                        _ -> defaultType ret_ty
            common_method_expr = CallExpr (MemberExpr (IdentifierExpr "ref") name)
                                                    [RefType (refTypeName ret_kt_ty)]
                                                    -- if Argument is primitive, pass value directly
                                                    -- if Argument is reference, pass x.ref
                                                    (LiteralString name : map (\(param_nm, kt_ty) ->
                                                        if Kt.isPrimType kt_ty
                                                            then IdentifierExpr param_nm
                                                            else MemberExpr (IdentifierExpr param_nm) "ref")
                                                        para_kt_nm_tys)
                    in if Kt.isPrimType ret_kt_ty
                    then Kt.BlockBody $ [ExpressionStmt $ common_method_expr]
                    else Kt.BlockBody $ [ExpressionStmt $
                                                    CallExpr (IdentifierExpr (refTypeName ret_kt_ty ++ "Proxy")) []
                                                    [common_method_expr]]
convertMethodBody _ = error $ "Only Method function type is supported in method function body generation"

getArkModulePath :: [Decorator] -> String
getArkModulePath decos =
    case findDecorator "ExportKotlinFunction" decos of
        Just (DecoratorPara _ p) ->
            case L.lookup "ark_module_path" p of
                Just path -> path
                Nothing -> "<no_module_path>"
        _ -> "<no_module_path>"


convertGlobalFuncBody :: FuncD -> Kt.FunctionBody
convertGlobalFuncBody (FuncD Func name decos params ret_ty) =
    let ark_module_path = getArkModulePath decos
        common_body = [
                AssignmentStmt M_Val "mod" (CallExpr (IdentifierExpr "ArkModule")  [] [(LiteralString ark_module_path)]),
                AssignmentStmt M_Val "entry" (CallExpr (IdentifierExpr "ArkObjectSafeReference") []
                                                [CallExpr (MemberExpr (IdentifierExpr "mod") "getNapiValue") [] []]),
                AssignmentStmt M_Val "func" (CallExpr (MemberExpr (IdentifierExpr "entry") "get")  [] [LiteralString name])]
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
                        CallExpr (IdentifierExpr (refTypeName return_type ++ "Proxy")) [] 
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
    let deco_ty = findDecorator "ExportKotlinField" decos
        kt_ty = case deco_ty of
                    Just (DecoratorPara _ p) -> case L.lookup "type" p of
                                                    Just t -> pKtType t
                                                    Nothing -> error $ "ExportKotlinField " ++
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