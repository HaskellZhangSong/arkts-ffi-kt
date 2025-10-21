{-# LANGUAGE DeriveGeneric #-}
module Language.TypeScript.AST where
import GHC.Generics (Generic)

data SyntaxKind = 
      SkUnknown
    | SkEndOfFileToken
    | SkSingleLineCommentTrivia
    | SkMultiLineCommentTrivia
    | SkNewLineTrivia
    | SkWhitespaceTrivia
    | SkShebangTrivia
    | SkConflictMarkerTrivia
    | SkNonTextFileMarkerTrivia
    | SkNumericLiteral
    | SkBigIntLiteral
    | SkStringLiteral
    | SkJsxText
    | SkJsxTextAllWhiteSpaces
    | SkRegularExpressionLiteral
    | SkNoSubstitutionTemplateLiteral
    | SkTemplateHead
    | SkTemplateMiddle
    | SkTemplateTail
    | SkOpenBraceToken
    | SkCloseBraceToken
    | SkOpenParenToken
    | SkCloseParenToken
    | SkOpenBracketToken
    | SkCloseBracketToken
    | SkDotToken
    | SkDotDotDotToken
    | SkSemicolonToken
    | SkCommaToken
    | SkQuestionDotToken
    | SkLessThanToken
    | SkLessThanSlashToken
    | SkGreaterThanToken
    | SkLessThanEqualsToken
    | SkGreaterThanEqualsToken
    | SkEqualsEqualsToken
    | SkExclamationEqualsToken
    | SkEqualsEqualsEqualsToken
    | SkExclamationEqualsEqualsToken
    | SkEqualsGreaterThanToken
    | SkPlusToken
    | SkMinusToken
    | SkAsteriskToken
    | SkAsteriskAsteriskToken
    | SkSlashToken
    | SkPercentToken
    | SkPlusPlusToken
    | SkMinusMinusToken
    | SkLessThanLessThanToken
    | SkGreaterThanGreaterThanToken
    | SkGreaterThanGreaterThanGreaterThanToken
    | SkAmpersandToken
    | SkBarToken
    | SkCaretToken
    | SkExclamationToken
    | SkTildeToken
    | SkAmpersandAmpersandToken
    | SkBarBarToken
    | SkQuestionToken
    | SkColonToken
    | SkAtToken
    | SkQuestionQuestionToken
    | SkBacktickToken
    | SkHashToken
    | SkEqualsToken
    | SkPlusEqualsToken
    | SkMinusEqualsToken
    | SkAsteriskEqualsToken
    | SkAsteriskAsteriskEqualsToken
    | SkSlashEqualsToken
    | SkPercentEqualsToken
    | SkLessThanLessThanEqualsToken
    | SkGreaterThanGreaterThanEqualsToken
    | SkGreaterThanGreaterThanGreaterThanEqualsToken
    | SkAmpersandEqualsToken
    | SkBarEqualsToken
    | SkBarBarEqualsToken
    | SkAmpersandAmpersandEqualsToken
    | SkQuestionQuestionEqualsToken
    | SkCaretEqualsToken
    | SkIdentifier
    | SkPrivateIdentifier
    | SkJSDocCommentTextToken
    | SkBreakKeyword
    | SkCaseKeyword
    | SkCatchKeyword
    | SkClassKeyword
    | SkConstKeyword
    | SkContinueKeyword
    | SkDebuggerKeyword
    | SkDefaultKeyword
    | SkDeleteKeyword
    | SkDoKeyword
    | SkElseKeyword
    | SkEnumKeyword
    | SkExportKeyword
    | SkExtendsKeyword
    | SkFalseKeyword
    | SkFinallyKeyword
    | SkForKeyword
    | SkFunctionKeyword
    | SkIfKeyword
    | SkImportKeyword
    | SkInKeyword
    | SkInstanceOfKeyword
    | SkNewKeyword
    | SkNullKeyword
    | SkReturnKeyword
    | SkSuperKeyword
    | SkSwitchKeyword
    | SkThisKeyword
    | SkThrowKeyword
    | SkTrueKeyword
    | SkTryKeyword
    | SkTypeOfKeyword
    | SkVarKeyword
    | SkVoidKeyword
    | SkWhileKeyword
    | SkWithKeyword
    | SkImplementsKeyword
    | SkInterfaceKeyword
    | SkLetKeyword
    | SkPackageKeyword
    | SkPrivateKeyword
    | SkProtectedKeyword
    | SkPublicKeyword
    | SkStaticKeyword
    | SkYieldKeyword
    | SkAbstractKeyword
    | SkAccessorKeyword
    | SkAsKeyword
    | SkAssertsKeyword
    | SkAssertKeyword
    | SkAnyKeyword
    | SkAsyncKeyword
    | SkAwaitKeyword
    | SkBooleanKeyword
    | SkConstructorKeyword
    | SkDeclareKeyword
    | SkGetKeyword
    | SkInferKeyword
    | SkIntrinsicKeyword
    | SkIsKeyword
    | SkKeyOfKeyword
    | SkModuleKeyword
    | SkNamespaceKeyword
    | SkNeverKeyword
    | SkOutKeyword
    | SkReadonlyKeyword
    | SkRequireKeyword
    | SkNumberKeyword
    | SkObjectKeyword
    | SkSatisfiesKeyword
    | SkSetKeyword
    | SkStringKeyword
    | SkSymbolKeyword
    | SkTypeKeyword
    | SkUndefinedKeyword
    | SkUniqueKeyword
    | SkUnknownKeyword
    | SkUsingKeyword
    | SkFromKeyword
    | SkGlobalKeyword
    | SkBigIntKeyword
    | SkOverrideKeyword
    | SkOfKeyword
    | SkDeferKeyword 
    | SkQualifiedName
    | SkComputedPropertyName
    | SkTypeParameter
    | SkParameter
    | SkDecorator
    | SkPropertySignature
    | SkPropertyDeclaration
    | SkMethodSignature
    | SkMethodDeclaration
    | SkClassStaticBlockDeclaration
    | SkConstructor
    | SkGetAccessor
    | SkSetAccessor
    | SkCallSignature
    | SkConstructSignature
    | SkIndexSignature
    | SkTypePredicate
    | SkTypeReference
    | SkFunctionType
    | SkConstructorType
    | SkTypeQuery
    | SkTypeLiteral
    | SkArrayType
    | SkTupleType
    | SkOptionalType
    | SkRestType
    | SkUnionType
    | SkIntersectionType
    | SkConditionalType
    | SkInferType
    | SkParenthesizedType
    | SkThisType
    | SkTypeOperator
    | SkIndexedAccessType
    | SkMappedType
    | SkLiteralType
    | SkNamedTupleMember
    | SkTemplateLiteralType
    | SkTemplateLiteralTypeSpan
    | SkImportType
    | SkObjectBindingPattern
    | SkArrayBindingPattern
    | SkBindingElement
    | SkArrayLiteralExpression
    | SkObjectLiteralExpression
    | SkPropertyAccessExpression
    | SkElementAccessExpression
    | SkCallExpression
    | SkNewExpression
    | SkTaggedTemplateExpression
    | SkTypeAssertionExpression
    | SkParenthesizedExpression
    | SkFunctionExpression
    | SkArrowFunction
    | SkDeleteExpression
    | SkTypeOfExpression
    | SkVoidExpression
    | SkAwaitExpression
    | SkPrefixUnaryExpression
    | SkPostfixUnaryExpression
    | SkBinaryExpression
    | SkConditionalExpression
    | SkTemplateExpression
    | SkYieldExpression
    | SkSpreadElement
    | SkClassExpression
    | SkOmittedExpression
    | SkExpressionWithTypeArguments
    | SkAsExpression
    | SkNonNullExpression
    | SkMetaProperty
    | SkSyntheticExpression
    | SkSatisfiesExpression
    | SkTemplateSpan
    | SkSemicolonClassElement
    | SkBlock
    | SkEmptyStatement
    | SkVariableStatement
    | SkExpressionStatement
    | SkIfStatement
    | SkDoStatement
    | SkWhileStatement
    | SkForStatement
    | SkForInStatement
    | SkForOfStatement
    | SkContinueStatement
    | SkBreakStatement
    | SkReturnStatement
    | SkWithStatement
    | SkSwitchStatement
    | SkLabeledStatement
    | SkThrowStatement
    | SkTryStatement
    | SkDebuggerStatement
    | SkVariableDeclaration
    | SkVariableDeclarationList
    | SkFunctionDeclaration
    | SkClassDeclaration
    | SkInterfaceDeclaration
    | SkTypeAliasDeclaration
    | SkEnumDeclaration
    | SkModuleDeclaration
    | SkModuleBlock
    | SkCaseBlock
    | SkNamespaceExportDeclaration
    | SkImportEqualsDeclaration
    | SkImportDeclaration
    | SkImportClause
    | SkNamespaceImport
    | SkNamedImports
    | SkImportSpecifier
    | SkExportAssignment
    | SkExportDeclaration
    | SkNamedExports
    | SkNamespaceExport
    | SkExportSpecifier
    | SkMissingDeclaration
    | SkExternalModuleReference
    | SkJsxElement
    | SkJsxSelfClosingElement
    | SkJsxOpeningElement
    | SkJsxClosingElement
    | SkJsxFragment
    | SkJsxOpeningFragment
    | SkJsxClosingFragment
    | SkJsxAttribute
    | SkJsxAttributes
    | SkJsxSpreadAttribute
    | SkJsxExpression
    | SkJsxNamespacedName
    | SkCaseClause
    | SkDefaultClause
    | SkHeritageClause
    | SkCatchClause
    | SkImportAttributes
    | SkImportAttribute
    | SkImportTypeAssertionContainer
    | SkPropertyAssignment
    | SkShorthandPropertyAssignment
    | SkSpreadAssignment
    | SkEnumMember
    | SkSourceFile
    | SkBundle
    | SkJSDocTypeExpression
    | SkJSDocNameReference
    | SkJSDocMemberName 
    | SkJSDocAllType 
    | SkJSDocUnknownType 
    | SkJSDocNullableType
    | SkJSDocNonNullableType
    | SkJSDocOptionalType
    | SkJSDocFunctionType
    | SkJSDocVariadicType
    | SkJSDocNamepathType 
    | SkJSDoc
    | SkJSDocText
    | SkJSDocTypeLiteral
    | SkJSDocSignature
    | SkJSDocLink
    | SkJSDocLinkCode
    | SkJSDocLinkPlain
    | SkJSDocTag
    | SkJSDocAugmentsTag
    | SkJSDocImplementsTag
    | SkJSDocAuthorTag
    | SkJSDocDeprecatedTag
    | SkJSDocClassTag
    | SkJSDocPublicTag
    | SkJSDocPrivateTag
    | SkJSDocProtectedTag
    | SkJSDocReadonlyTag
    | SkJSDocOverrideTag
    | SkJSDocCallbackTag
    | SkJSDocOverloadTag
    | SkJSDocEnumTag
    | SkJSDocParameterTag
    | SkJSDocReturnTag
    | SkJSDocThisTag
    | SkJSDocTypeTag
    | SkJSDocTemplateTag
    | SkJSDocTypedefTag
    | SkJSDocSeeTag
    | SkJSDocPropertyTag
    | SkJSDocThrowsTag
    | SkJSDocSatisfiesTag
    | SkJSDocImportTag
    | SkSyntaxList
    | SkNotEmittedStatement
    | SkNotEmittedTypeElement
    | SkPartiallyEmittedExpression
    | SkCommaListExpression
    | SkSyntheticReferenceExpression
    | SkCount
    --Sk -- | JSDocComment = JSDoc
    --Sk // FirstAssignment = EqualsToken
    --Sk // LastAssignment = CaretEqualsToken
    --Sk // FirstCompoundAssignment = PlusEqualsToken
    --Sk // LastCompoundAssignment = CaretEqualsToken
    -- // FirstReservedWord = BreakKeyword
    -- // LastReservedWord = WithKeyword
    -- // FirstKeyword = BreakKeyword
    -- // LastKeyword = DeferKeyword
    -- // FirstFutureReservedWord = ImplementsKeyword
    -- // LastFutureReservedWord = YieldKeyword
    -- // FirstTypeNode = TypePredicate
    -- // LastTypeNode = ImportType
    -- // FirstPunctuation = OpenBraceToken
    -- // LastPunctuation = CaretEqualsToken
    -- // FirstToken = Unknown
    -- // LastToken = LastKeyword
    -- // FirstTriviaToken = SingleLineCommentTrivia
    -- // LastTriviaToken = ConflictMarkerTrivia
    -- // FirstLiteralToken = NumericLiteral
    -- // LastLiteralToken = NoSubstitutionTemplateLiteral
    -- // FirstTemplateToken = NoSubstitutionTemplateLiteral
    -- // LastTemplateToken = TemplateTail
    -- // FirstBinaryOperator = LessThanToken
    -- // LastBinaryOperator = CaretEqualsToken
    -- // FirstStatement = VariableStatement
    -- // LastStatement = DebuggerStatement
    -- // FirstNode = QualifiedName
    -- // FirstJSDocNode = JSDocTypeExpression
    -- // LastJSDocNode = JSDocImportTag
    -- // FirstJSDocTagNode = JSDocTag
    -- // LastJSDocTagNode = JSDocImportTag
    -- // FirstContextualKeyword = AbstractKeyword
    -- // LastContextualKeyword = LastKeyword
    -- | SkAssertClause = ImportAttributes
    -- | SkAssertEntry = ImportAttribute
        deriving (Show, Eq, Ord, Generic, Enum)

data Decorator = Decorator {
    name :: String
} | DecoratorPara {
    name :: String,
    para :: [(String, String)]
} deriving (Show, Eq, Ord, Generic)

data SourceFile = SourceFile [Decl]
    deriving (Show, Eq, Ord, Generic)

data Type = 
      TyRef String
    | TyApp String [Type]
    | TyFun [Type] Type
    | TyNullable Type
    | TyArray Type
    | TyUnion [Type]
    | TyIntersection [Type]
    deriving (Show, Eq, Ord, Generic)

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

