{-# LANGUAGE OverloadedStrings #-}
module Language.Kotlin.AST where

import Prettyprinter
import Data.List (intercalate)
-- | Main Kotlin compilation unit
data KotlinFile = KotlinFile 
  { packageDecl :: Maybe String
  , imports :: [Import]
  , declarations :: [KotlinDeclaration]
  } deriving (Eq, Show)

-- | Import declarations
data Import = Import 
  { importPath :: String
  , importAlias :: Maybe String
  } deriving (Eq, Show)

-- | Top-level Kotlin declarations
data KotlinDeclaration
  = ClassDecl Class
  | InterfaceDecl Interface
  | VarDecl Var
  | FunctionDecl Function
  | PropertyDecl Property
  | ObjectDecl Object
  | DataClassDecl DataClass
  | EnumDecl KotlinEnum
  deriving (Eq, Show)

data Var = Var
  { varModifier :: ParameterModifier
  , varName :: String
  , varType :: Maybe KotlinType
  , varInitializer :: Maybe Expression
  } deriving (Eq, Show)

-- | Class declaration
data Class = Class
  { className :: String
  , classModifiers :: [Modifier]
  , typeParameters :: [TypeParameter]
  , superTypes :: [KotlinType]
  , primaryConstructor :: Maybe Constructor
  , classBody :: [ClassMember]
  } deriving (Eq, Show)

-- | Data class declaration (special case of class)
data DataClass = DataClass
  { dataClassName :: String
  , dataClassModifiers :: [Modifier]
  , dataClassTypeParameters :: [TypeParameter]
  , dataClassProperties :: [Property]
  } deriving (Eq, Show)

-- | Interface declaration
data Interface = Interface
  { interfaceName :: String
  , interfaceModifiers :: [Modifier]
  , interfaceTypeParameters :: [TypeParameter]
  , interfaceSuperTypes :: [KotlinType]
  , interfaceMembers :: [InterfaceMember]
  } deriving (Eq, Show)

-- | Object declaration (singleton)
data Object = Object
  { objectName :: String
  , objectModifiers :: [Modifier]
  , objectSuperTypes :: [KotlinType]
  , objectMembers :: [ClassMember]
  } deriving (Eq, Show)

-- | Enum declaration
data KotlinEnum = KotlinEnum
  { enumName :: String
  , enumModifiers :: [Modifier]
  , enumEntries :: [EnumEntry]
  , enumMembers :: [ClassMember]
  } deriving (Eq, Show)

data EnumEntry = EnumEntry
  { entryName :: String
  , entryArguments :: [Expression]
  , entryMembers :: [ClassMember]
  } deriving (Eq, Show)

-- | Function declaration
data Function = Function
  { functionName :: String
  , functionModifiers :: [Modifier]
  , functionTypeParameters :: [TypeParameter]
  , functionParameters :: [Parameter]
  , functionReturnType :: Maybe KotlinType
  , functionBody :: Maybe FunctionBody
  } deriving (Eq, Show)

-- | Property declaration
data Property = Property
  { propertyName :: String
  , propertyModifiers :: [ParameterModifier]
  , propertyType :: Maybe KotlinType
  , propertyInitializer :: Maybe Expression
  , propertyGetter :: Maybe PropertyAccessor
  , propertySetter :: Maybe PropertyAccessor
  } deriving (Eq, Show)

-- | Class members
data ClassMember
  = ClassProperty Property
  | ClassFunction Function
  | ClassConstructor Constructor
  | ClassClass Class
  | ClassObject Object
  | ClassInterface Interface
  deriving (Eq, Show)

-- | Interface members
data InterfaceMember
  = InterfaceProperty Property
  | InterfaceFunction Function
  | InterfaceClass Class
  | InterfaceInterface Interface
  deriving (Eq, Show)

-- | Constructor declaration
data Constructor = Constructor
  { constructorModifiers :: [Modifier]
  , constructorParameters :: [Parameter]
  , constructorBody :: Maybe ConstructorBody
  } deriving (Eq, Show)

-- | Parameter declaration
data Parameter = Parameter
  { parameterName :: String
  , parameterType :: KotlinType
  , parameterDefaultValue :: Maybe Expression
  , parameterModifiers :: [ParameterModifier]
  } deriving (Eq, Show)

-- | Type parameter (generics)
data TypeParameter = TypeParameter
  { typeParameterName :: String
  , typeParameterBounds :: [KotlinType]
  , typeParameterVariance :: Maybe Variance
  } deriving (Eq, Show)

-- | Kotlin type system
data KotlinType
  = RefType String                          -- Int, String, etc.
  | GenericType String [KotlinType]           -- List<String>
  | NullableType KotlinType                   -- String?
  | FunctionType [KotlinType] KotlinType      -- (Int, String) -> Boolean
  | TupleType [KotlinType]                    -- (T1, T2,...)
  | QualifiedType [String] KotlinType         -- QualifiedName
  deriving (Eq, Show)

-- | Modifiers
data Modifier
  = Public | Private | Protected | Internal
  | Abstract | Final | Open | Override
  | Static | Suspend | Inline | Infix
  | Operator | Data | Sealed | Inner
  | Companion | Const | Lateinit
  | Vararg | NoInline | CrossInline
  deriving (Eq, Show)

-- | Parameter modifiers
data ParameterModifier
  = M_Val | M_Var | M_Vararg
  deriving (Eq, Show)

-- | Variance annotations
data Variance = In | Out
  deriving (Eq, Show)

-- | Function body
data FunctionBody
  = ExpressionBody Expression
  | BlockBody [Statement]
  deriving (Eq, Show)

-- | Constructor body
data ConstructorBody
  = DelegatingConstructor [Expression]
  | ConstructorBlock [Statement]
  deriving (Eq, Show)

-- | Property accessor (getter/setter)
data PropertyAccessor = PropertyAccessor
  { accessorModifiers :: [Modifier]
  , accessorBody :: Maybe FunctionBody
  } deriving (Eq, Show)

-- | Expressions (simplified for FFI generation)
data Expression
  = LiteralString String
  | IdentifierExpr String
  | CallExpr Expression [KotlinType] [Expression]
  | MemberExpr Expression String
  | ThisExpr
  | SuperExpr
  deriving (Eq, Show)

-- | Statements (simplified for FFI generation)
data Statement
  = ExpressionStmt Expression
  | ReturnStmt (Maybe Expression)
  | AssignmentStmt String Expression
  deriving (Eq, Show)

-- Pretty Printer Instances

instance Pretty KotlinFile where
  pretty (KotlinFile pkg imps decls) = 
    vsep $ catMaybes $
      [ fmap (\p -> "package" <+> pretty p <> line) pkg
      , if null imps then Nothing else Just (vsep (map pretty imps) <> line)
      , if null decls then Nothing else Just (vsep $ punctuate (line <> line) $ map pretty decls)
      ]
    where
      catMaybes [] = []
      catMaybes (Nothing : xs) = catMaybes xs
      catMaybes (Just x : xs) = x : catMaybes xs

instance Pretty Import where
  pretty (Import path alias) = 
    "import" <+> pretty path <> maybe mempty (\a -> " as " <> pretty a) alias

instance Pretty KotlinDeclaration where
  pretty (ClassDecl cls) = pretty cls
  pretty (InterfaceDecl iface) = pretty iface
  pretty (FunctionDecl func) = pretty func
  pretty (PropertyDecl prop) = pretty prop
  pretty (ObjectDecl obj) = pretty obj
  pretty (DataClassDecl datacls) = pretty datacls
  pretty (EnumDecl enum) = pretty enum

instance Pretty Class where
  pretty (Class name mods tyParams supers ctor body) =
    (if null mods then "" else hsep (map pretty mods) <+> "") <>
    "class" <+> pretty name <>
    prettyTypeParams tyParams <>
    maybe mempty pretty ctor <>
    prettySuperTypes supers <+>
    prettyClassBody body

instance Pretty DataClass where
  pretty (DataClass name mods tyParams props) =
    hsep (map pretty mods) <+>
    "data class" <+> pretty name <>
    prettyTypeParams tyParams <>
    parens (hsep $ punctuate comma $ map prettyDataProperty props)
    where
      prettyDataProperty prop = 
        hsep (map pretty (propertyModifiers prop)) <+>
        pretty (propertyName prop) <> ":" <+> 
        maybe ("Any") pretty (propertyType prop)

instance Pretty Interface where
  pretty (Interface name mods tyParams supers members) =
    hsep (map pretty mods) <+>
    "interface" <+> pretty name <>
    prettyTypeParams tyParams <>
    prettySuperTypes supers <+>
    prettyInterfaceBody members

instance Pretty Object where
  pretty (Object name mods supers members) =
    hsep (map pretty mods) <+>
    "object" <+> pretty name <>
    prettySuperTypes supers <+>
    prettyClassBody members

instance Pretty KotlinEnum where
  pretty (KotlinEnum name mods entries members) =
    hsep (map pretty mods) <+>
    "enum class" <+> pretty name <+> "{" <> line <>
    indent 2 (vsep $ punctuate comma $ map pretty entries) <>
    (if null members then mempty else ";" <> line <> indent 2 (vsep $ map pretty members)) <>
    line <> "}"

instance Pretty EnumEntry where
  pretty (EnumEntry name args members) =
    pretty name <>
    (if null args then mempty else parens (hsep $ punctuate comma $ map pretty args)) <>
    (if null members then mempty else " {" <> line <> indent 2 (vsep $ map pretty members) <> line <> "}")

instance Pretty Function where
  pretty (Function name mods tyParams params retType body) =
    (if null mods then "" else hsep (map pretty mods) <+> "") <>
    "fun" <>
    prettyTypeParams tyParams <+>
    pretty name <>
    parens (hsep $ punctuate comma $ map pretty params) <>
    maybe mempty (\t -> ": " <> pretty t) retType <>
    maybe mempty (\b -> pretty b) body

instance Pretty Property where
  pretty (Property name mods propType initializer getter setter) =
    (if null mods then "" else hsep (map pretty mods) <+> "") <>
    pretty name <>
    maybe mempty (\t -> ": " <> pretty t) propType <> 
    maybe mempty (\i -> " = " <> pretty i) initializer <>
    maybe mempty (\g -> line <> indent 2 ("get()" <+> maybe mempty pretty (accessorBody g))) getter <>
    maybe mempty (\s -> line <> indent 2 ("set(value)" <+> maybe mempty pretty (accessorBody s))) setter

instance Pretty ClassMember where
  pretty (ClassProperty prop) = pretty prop
  pretty (ClassFunction func) = pretty func
  pretty (ClassConstructor ctor) = pretty ctor
  pretty (ClassClass cls) = pretty cls
  pretty (ClassObject obj) = pretty obj
  pretty (ClassInterface iface) = pretty iface

instance Pretty InterfaceMember where
  pretty (InterfaceProperty prop) = pretty prop
  pretty (InterfaceFunction func) = pretty func
  pretty (InterfaceClass cls) = pretty cls
  pretty (InterfaceInterface iface) = pretty iface

instance Pretty Constructor where
  pretty (Constructor mods params body) =
    hsep (map pretty mods) <+>
    "constructor" <>
    parens (hsep $ punctuate comma $ map pretty params) <>
    maybe mempty (\b -> pretty b) body

instance Pretty Parameter where
  pretty (Parameter name paramType defaultVal paramMods) =
    (if null paramMods then "" else hsep (map pretty paramMods) <+> "") <>
    pretty name <> ":" <+> pretty paramType <>
    maybe mempty (\v -> " = " <> pretty v) defaultVal

instance Pretty TypeParameter where
  pretty (TypeParameter name bounds variance) =
    maybe mempty (\v -> pretty v <> " ") variance <>
    pretty name <>
    (if null bounds then mempty else " : " <> hsep (punctuate (" & ") $ map pretty bounds))

instance Pretty KotlinType where
  pretty (RefType name) = pretty name
  pretty (GenericType name args) = pretty name <> "<" <> hsep (punctuate comma $ map pretty args) <> ">"
  pretty (NullableType typ) = pretty typ <> "?"
  pretty (FunctionType params ret) = 
    parens (hsep $ punctuate comma $ map pretty params) <+> "->" <+> pretty ret
  pretty (TupleType types) = 
    parens (hsep $ punctuate comma $ map pretty types)
  pretty (QualifiedType ns ty) = pretty (intercalate "." ns) <> "." <> pretty ty


instance Pretty Modifier where
  pretty Public = "public"
  pretty Private = "private"
  pretty Protected = "protected"
  pretty Internal = "internal"
  pretty Abstract = "abstract"
  pretty Final = "final"
  pretty Open = "open"
  pretty Override = "override"
  pretty Static = "static"
  pretty Suspend = "suspend"
  pretty Inline = "inline"
  pretty Infix = "infix"
  pretty Operator = "operator"
  pretty Data = "data"
  pretty Sealed = "sealed"
  pretty Inner = "inner"
  pretty Companion = "companion"
  pretty Const = "const"
  pretty Lateinit = "lateinit"
  pretty Vararg = "vararg"
  pretty NoInline = "noinline"
  pretty CrossInline = "crossinline"

instance Pretty ParameterModifier where
  pretty M_Val = "val"
  pretty M_Var = "var"
  pretty M_Vararg = "vararg"

instance Pretty Variance where
  pretty In = "in"
  pretty Out = "out"

instance Pretty FunctionBody where
  pretty (ExpressionBody expr) = "=" <+> pretty expr
  pretty (BlockBody stmts) = "{" <> line <> indent 2 (vsep $ map pretty stmts) <> line <> "}"

instance Pretty ConstructorBody where
  pretty (DelegatingConstructor args) = ":" <+> "this" <> parens (hsep $ punctuate comma $ map pretty args)
  pretty (ConstructorBlock stmts) = "{" <> line <> indent 2 (vsep $ map pretty stmts) <> line <> "}"

instance Pretty PropertyAccessor where
  pretty (PropertyAccessor mods body) =
    hsep (map pretty mods) <> maybe mempty pretty body

instance Pretty Expression where
  pretty (LiteralString lit) = dquotes $ pretty lit
  pretty (IdentifierExpr name) = pretty name
  pretty (CallExpr func ty args) = pretty func <>
                                      if null ty then mempty else "<" <> hsep (punctuate comma $ map pretty ty) <> ">" <>
                                      parens (hsep $ punctuate comma $ map pretty args)
  pretty (MemberExpr obj member) = pretty obj <> "." <> pretty member
  pretty ThisExpr = "this"
  pretty SuperExpr = "super"

instance Pretty Statement where
  pretty (ExpressionStmt expr) = pretty expr
  pretty (ReturnStmt Nothing) = "return"
  pretty (ReturnStmt (Just expr)) = "return" <+> pretty expr
  pretty (AssignmentStmt var expr) = pretty var <+> "=" <+> pretty expr

-- Helper functions for pretty printing

prettyTypeParams :: [TypeParameter] -> Doc ann
prettyTypeParams [] = mempty
prettyTypeParams tyParams = "<" <> hsep (punctuate comma $ map pretty tyParams) <> ">"

prettySuperTypes :: [KotlinType] -> Doc ann
prettySuperTypes [] = mempty
prettySuperTypes supers = ": " <+> hsep (punctuate comma $ map pretty supers)

prettyClassBody :: [ClassMember] -> Doc ann
prettyClassBody [] = "{}"
prettyClassBody members = "{" <> line <> indent 2 (vsep $ map pretty members) <> line <> "}"

prettyInterfaceBody :: [InterfaceMember] -> Doc ann
prettyInterfaceBody [] = "{}"
prettyInterfaceBody members = "{" <> line <> indent 2 (vsep $ map pretty members) <> line <> "}"