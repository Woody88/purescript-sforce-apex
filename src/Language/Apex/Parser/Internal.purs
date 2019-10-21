module Language.Apex.Parser.Internal where

-- import Control.Applicative ((<$>), (*>), (<*), (<*>), pure)
-- import Control.Monad (replicateM)
-- import qualified Data.Set as S hiding (map)
-- import Data.Maybe (fromMaybe)

-- import Text.Parsec.Combinator
-- import Text.Parsec.Prim
-- import Text.Parsec.Expr

-- import Language.Java.AST
-- import Language.Java.Parser.Core

-- javaProgram :: JParser CompilationUnit
-- javaProgram = compilationUnit

-- -- |
-- -- = Productions from 3 __(Lexical Structures)__.

-- ident :: JParser Ident
-- ident = Ident <$> (getSS <$> satisfy isIdentifier)

-- literal :: JParser Primary
-- literal = Literal <$> do
--        tok <- getT
--        case tok of
--         TokInt t    -> return $ IntegerLiteral t
--         TokFloat s  -> return $ FloatingPointLiteral s
--         TokDouble s -> return $ FloatingPointLiteral s
--         TokLong t   -> return $ IntegerLiteral t
--         TokChar s   -> return $ CharacterLiteral s
--         TokString s -> return $ StringLiteral s
--         TokBool s   -> return $ BooleanLiteral s
--         TokNull     -> return NullLiteral
--         s           -> unexpected (show s)
--        <?> "literal"

-- -- | Java Name
-- typeName :: JParser TypeName
-- typeName = ident `sepBy1` dot

-- -- | Java name ended by dot, for parsing such as "this.class"
-- typeNameDot :: JParser TypeName
-- typeNameDot = ident `sepEndBy1` dot

-- -- |
-- -- = Productions from §4 __(Types, Values, and Variables)__.

-- type_ :: JParser Type
-- type_ = try (RefType <$> refType) <|> (PrimType <$> primType)

-- -- | Integral and floating point types are merged into one production.
-- primType :: JParser PrimType
-- primType = (do
--     let isPrimType x = isKeyword x && S.member (getSS x) primitiveTypes
--     tok <- satisfy isPrimType
--     return $ case getSS tok of
--       "byte"    -> ByteT
--       "short"   -> ShortT
--       "int"     -> IntT
--       "long"    -> LongT
--       "char"    -> CharT
--       "float"   -> FloatT
--       "double"  -> DoubleT
--       _         -> BooleanT)
--     <?> "primitive type"

-- refType :: JParser RefType
-- refType = choice (map try
--         [ ArrayType    <$> arrayType
--         , classOrInterfaceType
--         , TypeVariable <$> typeVariable
--         ]) <?> "reference type"

-- classOrInterfaceType :: JParser RefType
-- classOrInterfaceType = ClassOrInterfaceType <$> classType

-- classType :: JParser ClassType
-- classType = ClassType <$> typeName <*> optionMaybe typeArgs

-- interfaceType :: JParser InterfaceType
-- interfaceType = classType

-- dims :: JParser Dims
-- dims = length <$> many1 (lSquare <* rSquare)

-- arrayType :: JParser ArrayType
-- arrayType = choice (map try
--           [ primArrayType
--           , classOrInterfaceArrayType
--           , typeVarArrayType
--           ]) <?> "array types"

-- typeVariable :: JParser TypeVariable
-- typeVariable = ident

-- primArrayType :: JParser ArrayType
-- primArrayType = PrimArrayT <$> primType <*> dims

-- classOrInterfaceArrayType :: JParser ArrayType
-- classOrInterfaceArrayType = ClassOrInterfaceArrayT <$> classType <*> dims

-- typeVarArrayType :: JParser ArrayType
-- typeVarArrayType = TypeVarArrayT <$> typeVariable <*> dims

-- typeParam :: JParser TypeParam
-- typeParam = TypeParam <$> ident <*> optionMaybe typeBound

-- typeBound :: JParser TypeBound
-- typeBound = keyword "extends" *> choice (map try
--           [ ExtendsClassType <$> classType <*> many additionalBound
--           , ExtendsTypeVar   <$> typeVariable
--           ])

-- additionalBound :: JParser AdditionalBound
-- additionalBound = operator "&" *> interfaceType

-- typeArgs :: JParser TypeArgs
-- typeArgs = between lessThan greaterThan typeArgList
--         <?> "type arguments"

-- typeArgList :: JParser TypeArgList
-- typeArgList = typeArg `sepBy1` comma

-- typeArg :: JParser TypeArg
-- typeArg = do
--     let wc x = isOperator x && (x === "?")
--     tok <- try (getSS <$> satisfy wc) <|> pure ""
--     if null tok then ActualType <$> refType
--                 else Wildcard   <$> optionMaybe wildcardBound
--     ; <?> "type argument"

-- typeParams :: JParser [TypeParam]
-- typeParams = between lessThan greaterThan (typeParam `sepBy1` comma)

-- wildcardBound :: JParser WildcardBound
-- wildcardBound = SuperWB <$> (keyword "super" *> refType)
--              <|> ExtendsWB <$> (keyword "extends" *> refType)
--              <?> "wild card bound"

-- --------------------------------------
-- -- | Productions from §7 (Packages)
-- --------------------------------------

-- -- | Compilation unit
-- compilationUnit :: JParser CompilationUnit
-- compilationUnit = CompilationUnit
--         <$> optionMaybe packageDeclaration
--         <*> many importDeclaration
--         <*> many typeDeclaration
--         <?> "compilation unit"

-- -- | Package declaration
-- packageDeclaration :: JParser PackageDeclaration
-- packageDeclaration = PackageDeclaration
--         <$> many packageModifier
--         <*> (keyword "package" *> typeName <* semiColon)
--         <?> "package declaration"

-- -- | Package Modifier = Annotation
-- -- | TODO Change this once Annotation is finished
-- packageModifier :: JParser PackageModifier
-- packageModifier = keyword "wizard" *> return PackageModifier
--                <?> "package modifier"

-- -- | Import declaration
-- importDeclaration :: JParser ImportDeclaration
-- importDeclaration = choice (map try [
--           singleTypeImportDeclaration
--         , typeImportOnDemandDeclaration
--         , singleStaticImportDeclaration
--         , staticImportOnDemandDeclaration
--         ]) <?> "import declaration"

-- singleTypeImportDeclaration :: JParser ImportDeclaration
-- singleTypeImportDeclaration = SingleTypeImportDeclaration
--         <$> (keyword "import" *> typeName)
--         <* semiColon

-- typeImportOnDemandDeclaration :: JParser ImportDeclaration
-- typeImportOnDemandDeclaration = TypeImportOnDemandDeclaration
--         <$> (keyword "import" *> typeNameDot <* star <* semiColon)

-- singleStaticImportDeclaration :: JParser ImportDeclaration
-- singleStaticImportDeclaration = do
--         tn <- keyword "import" *> keyword "static" *> typeNameDot <* semiColon
--         case tn of
--             s@(_:_:_) -> do
--                     let (x,y) = splitAt (length s - 1) s
--                     return $ SingleStaticImportDeclaration x (head y)
--             _ -> unexpected "Cannot import whole class"

-- staticImportOnDemandDeclaration :: JParser ImportDeclaration
-- staticImportOnDemandDeclaration = StaticImportOnDemandDeclaration
--         <$> (keyword "import" *> keyword "static" *> typeNameDot <*
--             (star <* semiColon))

-- -- | Productions from §8 (Classes)

-- -- TODO Add Annotation
-- classDeclaration :: JParser ClassDeclaration
-- classDeclaration = normalClassDeclaration
--                 <|> enumDeclaration
--                 <?> "class declaration"

-- normalClassDeclaration :: JParser ClassDeclaration
-- normalClassDeclaration = Class
--                       <$> classModifiers
--                       <*> ident
--                       <*> optionMaybe typeParams
--                       <*> optionMaybe superClass
--                       <*> optionMaybe superInterfaces
--                       <*> classBody
--                       <?> "normal class declaration"

-- classModifiers :: JParser [ClassModifier]
-- classModifiers = classModifier `manyTill` keyword "class"

-- classModifier :: JParser ClassModifier
-- classModifier = fromModifierTable classModifierTable

-- superClass :: JParser SuperClass
-- superClass = Extends <$> (keyword "extends" *> classType)

-- interfaceTypeList :: JParser InterfaceTypeList
-- interfaceTypeList = classType `sepBy1` comma
--                <?> "interface type list"

-- superInterfaces :: JParser SuperInterfaces
-- superInterfaces = Implements <$> (keyword "implements" *> interfaceTypeList)
--                <?> "super interfaces"

-- classBody :: JParser ClassBody
-- classBody = lBrace *> many classBodyDeclaration <* rBrace

-- classBodyDeclaration :: JParser ClassBodyDeclaration
-- classBodyDeclaration = choice (map try
--                   [ ClassMemberDeclaration <$> classMemberDeclaration
--                   , InstanceInitializer <$> instanceInitializer
--                   , StaticInitializer <$> staticInitializer
--                   , constructorDeclaration
--                   ]) <?> "class body declaration"

-- classMemberDeclaration :: JParser ClassMemberDeclaration
-- classMemberDeclaration = choice (map try
--                  [ fieldDeclaration
--                  , methodDeclaration
--                  , MemberClassDeclaration <$> classDeclaration
--                  , MemberInterfaceDeclaration <$> interfaceDeclaration
--                  , semiColon >> return EmptyClassMember
--                  ]) <?> "class member declaration"

-- fieldDeclaration :: JParser ClassMemberDeclaration
-- fieldDeclaration =  FieldDeclaration
--                 <$> (fieldModifier `manyTill` lookAhead unannType)
--                 <*> unannType
--                 <*> variableDeclaratorList
--                 <*  semiColon

-- fieldModifier :: JParser FieldModifier
-- fieldModifier = fromModifierTable fieldModifierTable

-- variableDeclaratorList :: JParser VariableDeclaratorList
-- variableDeclaratorList = variableDeclarator `sepBy1` comma

-- variableDeclarator :: JParser VariableDeclarator
-- variableDeclarator = VariableDeclarator
--                   <$> variableDeclaratorID
--                   <*> optionMaybe (operator "=" *> variableInitializer)

-- variableDeclaratorID :: JParser VariableDeclID
-- variableDeclaratorID = (,) <$> ident <*> (fromMaybe 0 <$> optionMaybe dims)

-- -- Unannoted type thing
-- -- TODO Finish this section once Annotation is finished.
-- unannType :: JParser UnannType
-- unannType = type_

-- result :: JParser Result
-- result = try (RType <$> unannType) <|> (pure RVoid <* keyword "void")

-- methodDeclaration :: JParser ClassMemberDeclaration
-- methodDeclaration = MethodDeclaration
--                 <$> many methodModifier
--                 <*> methodHeader
--                 <*> methodBody
--                 <?> "method declaration"

-- methodDeclarator :: JParser MethodDeclarator
-- methodDeclarator = MethodDeclarator
--                 <$> ident
--                 <*> (lParen *> optionMaybe formalParameterList <* rParen)
--                 <*> (fromMaybe 0 <$> optionMaybe dims)
--                 <?> "method declarator"

-- methodHeader :: JParser MethodHeader
-- methodHeader = try methodHeaderWithoutTP <|> methodHeaderTP
--             <?> "method header"

-- methodHeaderWithoutTP :: JParser MethodHeader
-- methodHeaderWithoutTP = MethodHeader
--                 <$> result
--                 <*> methodDeclarator
--                 <*> optionMaybe throws

-- methodHeaderTP :: JParser MethodHeader
-- methodHeaderTP = MethodHeaderTP
--                 <$> typeParams
--                 <*> result
--                 <*> methodDeclarator
--                 <*> optionMaybe throws

-- methodModifier :: JParser MethodModifier
-- methodModifier = fromModifierTable methodModifierTable

-- formalParameterList :: JParser FormalParameterList
-- formalParameterList = (++)
--                    <$> formalParameter `sepEndBy1` comma
--                    <*> (try ((:[]) <$> lastFormalParameter)
--                    <|> return [])

-- formalParameters :: JParser [FormalParameter]
-- formalParameters = choice
--             [ (:) <$> formalParameter <*> many (comma *> formalParameter)
--             , (:) <$> receiverParameter <*> many (comma *> formalParameter)]

-- formalParameter :: JParser FormalParameter
-- formalParameter = FormalParameter
--                <$> many variableModifier
--                <*> unannType
--                <*> variableDeclaratorID

-- -- TODO Add annotation
-- variableModifier :: JParser VariableModifier
-- variableModifier = keyword "final" >> return FinalV

-- lastFormalParameter :: JParser FormalParameter
-- lastFormalParameter =  try ellipsisParameter
--                    <|> formalParameter

-- ellipsisParameter :: JParser FormalParameter
-- ellipsisParameter =  EllipsisParameter
--                  <$> many variableModifier
--                  <*> unannType
--                  <*> many variableModifier
--                  <*> (replicateM 3 dot *> variableDeclaratorID)

-- receiverParameter :: JParser FormalParameter
-- receiverParameter =  ReceiverParameter
--                  <$> many variableModifier
--                  <*> unannType
--                  <*> optionMaybe (ident <* dot)
--                  <*  this

-- throws :: JParser Throws
-- throws = Throws <$> (keyword "throws" *> exceptionTypeList)

-- exceptionTypeList :: JParser ExceptionTypeList
-- exceptionTypeList = exceptionType `sepBy1` comma

-- exceptionType :: JParser ExceptionType
-- exceptionType =   try (ClassTypeEx <$> classType)
--              <|> (TypeVariableEx  <$> typeVariable)

-- typeDeclaration :: JParser TypeDeclaration
-- typeDeclaration =  ClassDeclaration      <$> classDeclaration
--                <|> InterfaceDeclaration  <$> interfaceDeclaration
--                <?> "type declaration"

-- methodBody :: JParser MethodBody
-- methodBody =  (semiColon >> return EmptyBody) <|> (MethodBody <$> block)

-- instanceInitializer :: JParser InstanceInitializer
-- instanceInitializer = block

-- staticInitializer :: JParser StaticInitializer
-- staticInitializer = keyword "static" *> block

-- constructorDeclaration :: JParser ClassBodyDeclaration
-- constructorDeclaration = ConstructorDeclaration
--                <$> many constructorModifier
--                <*> constructorDeclarator
--                <*> optionMaybe throws
--                <*> constructorBody
--                <?> "constructor declaration"

-- constructorModifier :: JParser ConstructorModifier
-- constructorModifier = fromModifierTable constructorModifierTable

-- simpleTypeName :: JParser SimpleTypeName
-- simpleTypeName = ident

-- constructorDeclarator :: JParser ConstructorDeclarator
-- constructorDeclarator = ConstructorDeclarator
--               <$> optionMaybe typeParams
--               <*> simpleTypeName
--               <*> (lParen *> optionMaybe formalParameterList <* rParen)
--               <?> "constructor declarator"

-- constructorBody :: JParser ConstructorBody
-- constructorBody = between lBrace rBrace (ConstructorBody
--               <$> optionMaybe explicitConstructorInvocation
--               <*> optionMaybe blockStatements)

-- explicitConstructorInvocation :: JParser ExplicitConstructorInvocation
-- explicitConstructorInvocation = choice (map try [thisECI, superECI,
--                                 nameSuperECI, primarySuperECI])

-- thisECI :: JParser ExplicitConstructorInvocation
-- thisECI = ThisECI
--        <$> (optionMaybe typeArgs <* keyword "this")
--        <*> (lParen *> optionMaybe argList <* rParen <* semiColon)

-- superECI :: JParser ExplicitConstructorInvocation
-- superECI = SuperECI
--        <$> (optionMaybe typeArgs <* keyword "super")
--        <*> (lParen *> optionMaybe argList <* rParen <* semiColon)

-- nameSuperECI :: JParser ExplicitConstructorInvocation
-- nameSuperECI = NameSuperECI
--        <$>  typeNameDot
--        <*> (optionMaybe typeArgs <* keyword "super")
--        <*> (lParen *> optionMaybe argList <* rParen <* semiColon)

-- primarySuperECI :: JParser ExplicitConstructorInvocation
-- primarySuperECI = PrimarySuperECI
--        <$> (primary <* dot)
--        <*> (optionMaybe typeArgs <* keyword "super")
--        <*> (lParen *> optionMaybe argList <* rParen <* semiColon)

-- enumDeclaration :: JParser ClassDeclaration
-- enumDeclaration = Enum
--        <$> many classModifier
--        <*> (keyword "enum" *> ident)
--        <*> optionMaybe superInterfaces
--        <*> enumBody

-- enumBody :: JParser EnumBody
-- enumBody = between lBrace rBrace (EnumBody
--       <$> (optionMaybe enumConstantList <* optionMaybe comma)
--       <*> optionMaybe enumBodyDeclaration)

-- enumConstantList :: JParser EnumConstantList
-- enumConstantList = enumConstant `sepBy1` comma

-- enumConstant :: JParser EnumConstant
-- enumConstant = EnumConstant
--             <$> many enumConstantModifier
--             <*> ident
--             <*> (fromMaybe Nothing <$> optionMaybe (lParen *>
--                                        optionMaybe argList <* rParen))
--             <*> optionMaybe classBody

-- enumConstantModifier :: JParser EnumConstantModifier
-- enumConstantModifier = keyword "annotation" *> return EnumConstantModifier

-- enumBodyDeclaration :: JParser EnumBodyDeclarations
-- enumBodyDeclaration = many classBodyDeclaration

-- -- |
-- -- = Productions from 9 (Interfaces)

-- interfaceDeclaration :: JParser InterfaceDeclaration
-- interfaceDeclaration = normalInterfaceDeclaration
--                     <?> "interface declaration"

-- normalInterfaceDeclaration :: JParser InterfaceDeclaration
-- normalInterfaceDeclaration =  NormalInterface
--                           <$> many interfaceModifier
--                           <*> (keyword "interface" *> ident)
--                           <*> optionMaybe typeParams
--                           <*> optionMaybe extendsInterfaces
--                           <*> interfaceBody

-- interfaceModifier :: JParser InterfaceModifier
-- interfaceModifier = fromModifierTable interfaceModifierTable

-- extendsInterfaces :: JParser ExtendsInterfaces
-- extendsInterfaces = ExtendsInterfaces
--                  <$> (keyword "extends" *> interfaceTypeList)

-- interfaceBody :: JParser InterfaceBody
-- interfaceBody = between lBrace rBrace (many interfaceMemberDeclaration)

-- interfaceMemberDeclaration :: JParser InterfaceMemberDeclaration
-- interfaceMemberDeclaration = choice
--                    [ constantDeclaration
--                    , interfaceMethodDeclaration
--                    , InterfaceClassDeclaration  <$> classDeclaration
--                    , InnerInterfaceDeclaration  <$> interfaceDeclaration
--                    ] <?> "interface member declaration"

-- constantDeclaration :: JParser InterfaceMemberDeclaration
-- constantDeclaration =  ConstantDeclaration
--                    <$> many constantModifier
--                    <*> unannType
--                    <*> variableDeclaratorList
--                    <*  semiColon

-- constantModifier :: JParser ConstantModifier
-- constantModifier = fromModifierTable constantModifierTable

-- interfaceMethodDeclaration :: JParser InterfaceMemberDeclaration
-- interfaceMethodDeclaration =  InterfaceMethodDeclaration
--                           <$> many interfaceMethodModifier
--                           <*> methodHeader
--                           <*> methodBody

-- interfaceMethodModifier :: JParser InterfaceMethodModifier
-- interfaceMethodModifier = fromModifierTable interfaceMethodModifierTable

-- variableInitializer :: JParser VariableInitializer
-- variableInitializer = choice
--                     [ ExpressionInitializer <$> expression
--                     , ArrayInitializer <$> arrayInitializer
--                     ] <?> "variable initializer"

-- arrayInitializer :: JParser ArrayInitializer
-- arrayInitializer = lBrace *> (variableInitializer `sepBy1` comma) <* rBrace

-- -- |
-- -- = Productions from 14 (Blocks and Statements)
-- block :: JParser Block
-- block = do
--         bs <- lBrace *> optionMaybe blockStatements <* rBrace
--         return $ case bs of
--             Just b -> Block b
--             Nothing -> EmptyBlock

-- blockStatements :: JParser BlockStatements
-- blockStatements = many1 blockStatement

-- blockStatement :: JParser BlockStatement
-- blockStatement = choice (map try
--               [ LocalVariableDeclarationStmt <$>
--                 (localVariableDeclaration <* semiColon)
--               , ClassDeclarationStmt <$> classDeclaration
--               , Statement <$> statement
--               ]) <?> "block statement"

-- localVariableDeclaration :: JParser LocalVariableDeclaration
-- localVariableDeclaration = LocalVariableDeclaration
--              <$> many variableModifier
--              <*> unannType
--              <*> variableDeclaratorList
--              <?> "local variable declaration"

-- statement :: JParser Statement
-- statement = choice
--           [ StatementWTS <$> statementWTS
--           , labeledStmt
--           , try ifThenElseStmt
--           , ifThenStmt
--           , whileStmt
--           , ForStmt <$> forStmt
--           ] <?> "statement"

-- statementNSI :: JParser StatementNSI
-- statementNSI = choice
--              [ StatementWTSNSI <$> statementWTS
--              , labeledStmtNSI
--              , ifThenElseStmtNSI
--              , whileStmtNSI
--              , ForStmtNSI <$> forStmtNSI
--              ] <?> "statement no short if"

-- statementWTS :: JParser StatementWTS
-- statementWTS = choice (map try
--              [ BlockStmt <$> block
--              , emptyStatement
--              , expressionStmt
--              , assertStmt
--              , assertLblStmt
--              , switchStmt
--              , doStmt
--              , breakStmt
--              , continueStmt
--              , returnStmt
--              , synchronizedStmt
--              , throwStmt
--              , TryStmt <$> tryStmt
--              ]) <?> "statement without trailing substatement"

-- emptyStatement :: JParser StatementWTS
-- emptyStatement = semiColon >> pure EmptyStmt

-- labeledStmt :: JParser Statement
-- labeledStmt = LabeledStmt <$> ident <*> (operator ":" *> statement)

-- labeledStmtNSI :: JParser StatementNSI
-- labeledStmtNSI = LabeledStmtNSI <$> ident <*> (operator ":" *> statementNSI)

-- expressionStmt :: JParser StatementWTS
-- expressionStmt = ExpressionStmt <$> statementExpression <* semiColon

-- isStatementExpression :: AssignmentExpression -> Bool
-- isStatementExpression expr = case expr of
--         Assignment {} -> True
--         Term (PrefixExpr  (Operator "++") _) -> True
--         Term (PrefixExpr  (Operator "--") _) -> True
--         Term (PostfixExpr (Operator "++") _) -> True
--         Term (PostfixExpr (Operator "--") _) -> True
--         Term (PrimExpr (MethodInvocation _)) -> True
--         Term (PrimExpr (ClassInstanceCreationExpression _)) -> True
--         _ -> False

-- statementExpression :: JParser StatementExpression
-- statementExpression = do
--         aExpr <- assignmentExpression
--         if isStatementExpression aExpr then
--             return aExpr
--         else
--             unexpected "statement expression"

-- ifThenStmt :: JParser Statement
-- ifThenStmt =  keyword "if" *> (IfThenStmt
--           <$> (lParen *> expression <* rParen)
--           <*> statement)

-- ifThenElseStmt :: JParser Statement
-- ifThenElseStmt = keyword "if" *> (IfThenElseStmt
--           <$> (lParen *> expression <* rParen)
--           <*> statementNSI
--           <*> (keyword "else" *> statement))

-- ifThenElseStmtNSI :: JParser StatementNSI
-- ifThenElseStmtNSI = keyword "if" *> (IfThenElseStmtNSI
--           <$> (lParen *> expression <* rParen)
--           <*> statementNSI
--           <*> (keyword "else" *> statementNSI))

-- assertStmt :: JParser StatementWTS
-- assertStmt = AssertStmt <$> (keyword "assert" *> expression <* semiColon)

-- assertLblStmt :: JParser StatementWTS
-- assertLblStmt = AssertLblStmt
--              <$> (keyword "assert" *> expression)
--              <*> (operator ":" *> expression <* semiColon)

-- switchStmt :: JParser StatementWTS
-- switchStmt = keyword "switch" *> (SwitchStmt
--           <$> (lParen *> expression <* rParen)
--           <*> switchBlock)

-- switchBlock :: JParser SwitchBlock
-- switchBlock = lBrace *> (SwitchBlock
--            <$> many switchBlockStmtGrp
--            <*> many switchLabel) <* rBrace

-- switchBlockStmtGrp :: JParser SwitchBlockStmtGrp
-- switchBlockStmtGrp = SwitchBlockStmtGrp <$> switchLabels <*> blockStatements

-- switchLabels :: JParser SwitchLabels
-- switchLabels = many1 switchLabel

-- switchLabel :: JParser SwitchLabel
-- switchLabel = choice
--         [ keyword "case" *> (CaseExpr <$> constantExpression) <* operator ":"
--         , keyword "case" *> (CaseEnum <$> enumConstantName)   <* operator ":"
--         , keyword "default" *> operator ":" *> return CaseDefault
--         ]

-- enumConstantName :: JParser EnumConstantName
-- enumConstantName = ident

-- whileStmt :: JParser Statement
-- whileStmt = keyword "while" *> (WhileStmt
--           <$> (lParen *> expression <* rParen)
--           <*> statement)

-- whileStmtNSI :: JParser StatementNSI
-- whileStmtNSI = keyword "while" *> (WhileStmtNSI
--           <$> (lParen *> expression <* rParen)
--           <*> statementNSI)

-- doStmt :: JParser StatementWTS
-- doStmt = keyword "do" *> (DoStmt
--          <$> statement
--          <*> (keyword "while" *> lParen *>
--                 expression <* rParen <* semiColon))

-- forStmt :: JParser ForStatement
-- forStmt = try basicForStmt <|> enhancedForStmt
--        <?> "for statement"

-- forStmtNSI :: JParser ForStatementNSI
-- forStmtNSI =  try basicForStmtNSI <|> enhancedForStmtNSI
--           <?> "for statement no short if"

-- basicForStmt :: JParser ForStatement
-- basicForStmt =  keyword "for" *> (BasicFor
--             <$> (lParen *> optionMaybe forInit <* semiColon)
--             <*> (optionMaybe expression <* semiColon)
--             <*> (optionMaybe forUpdate <* rParen)
--             <*> statement)

-- basicForStmtNSI :: JParser ForStatementNSI
-- basicForStmtNSI =  keyword "for" *> (BasicForNSI
--                <$> (lParen *> optionMaybe forInit <* semiColon)
--                <*> (optionMaybe expression <* semiColon)
--                <*> (optionMaybe forUpdate <* rParen)
--                <*> statementNSI)

-- enhancedForStmt :: JParser ForStatement
-- enhancedForStmt =  keyword "for" *> (EnhancedFor
--                <$> (lParen *> many variableModifier)
--                <*> unannType
--                <*> variableDeclaratorID
--                <*> (operator ":" *> expression <* rParen)
--                <*> statement)

-- enhancedForStmtNSI :: JParser ForStatementNSI
-- enhancedForStmtNSI =  keyword "for" *> (EnhancedForNSI
--                <$> (lParen *> many variableModifier)
--                <*> unannType
--                <*> variableDeclaratorID
--                <*> (operator ":" *> expression <* rParen)
--                <*> statementNSI)

-- forInit :: JParser ForInit
-- forInit = choice
--         [ ForInitExpr <$> statementExpressionList
--         , ForInitDecl <$> localVariableDeclaration
--         ]

-- forUpdate :: JParser ForUpdate
-- forUpdate = statementExpressionList

-- statementExpressionList :: JParser StatementExpressionList
-- statementExpressionList = statementExpression `sepBy1` comma

-- breakStmt :: JParser StatementWTS
-- breakStmt = BreakStmt <$> (keyword "break" *> optionMaybe ident <* semiColon)

-- continueStmt :: JParser StatementWTS
-- continueStmt = ContinueStmt
--             <$> (keyword "continue" *> optionMaybe ident <* semiColon)

-- returnStmt :: JParser StatementWTS
-- returnStmt = ReturnStmt
--           <$> (keyword "return" *> optionMaybe expression <* semiColon)

-- throwStmt :: JParser StatementWTS
-- throwStmt = ThrowStmt <$> (keyword "throw" *> expression <* semiColon)

-- synchronizedStmt :: JParser StatementWTS
-- synchronizedStmt = keyword "synchronized" *> (SynchronizedStmt
--                 <$> (lParen *> expression <* rParen)
--                 <*> block)

-- tryStmt :: JParser TryStmt
-- tryStmt = choice
--         [ keyword "try" *> (TryCatch <$> block <*> catches)
--         , keyword "try" *> (TryFinally <$> block <*> optionMaybe catches
--                            <*> finally)
--         , tryWithResources
--         ]

-- catches :: JParser Catches
-- catches = many1 catchClause

-- catchClause :: JParser CatchClause
-- catchClause = keyword "catch" *> (CatchClause
--            <$> (lParen *> catchFormalParameter <* rParen)
--            <*> block)

-- catchFormalParameter :: JParser CatchFormalParameter
-- catchFormalParameter = CatchFormalParameter
--                     <$> many variableModifier
--                     <*> catchType
--                     <*> variableDeclaratorID

-- catchType :: JParser CatchType
-- catchType = classType `sepBy1` operator "|"

-- finally :: JParser Finally
-- finally = keyword "finally" *> block

-- tryWithResources :: JParser TryStmt
-- tryWithResources = keyword "try" *> (TryWithResources
--                 <$> resourceSpecification
--                 <*> block
--                 <*> optionMaybe catches
--                 <*> optionMaybe finally)

-- resourceSpecification :: JParser ResourceSpecification
-- resourceSpecification = lParen *> resourceList <* optionMaybe semiColon
--                             <* rParen

-- resourceList :: JParser ResourceList
-- resourceList = resource `sepBy1` semiColon

-- resource :: JParser Resource
-- resource =  Resource
--         <$> many variableModifier
--         <*> unannType
--         <*> variableDeclaratorID
--         <*> (operator "=" *> expression)

-- -- | Java Expressions
-- primary :: JParser Primary
-- primary = choice (map try [
--         try literal
--      ,  this
--      ,  try typeNameDotThis
--      ,  try typeNameDotClass
--      ,  try typeNameArrDotClass
--      ,  voidDotClass
--      ,  expressionParen
--      ,  classInstanceCreationExpression
--      ,  fieldAccess
--      ,  arrayAccess
--      ,  methodInvocation
--      ,  methodReference
--      ,  ArrayCreation <$> arrayCreationExpr
--      ]) |>> primarySuffix
--      <?> "primary expression"

-- primarySuffix :: JParser (Primary -> Primary)
-- primarySuffix = choice (map try [ciceSuffix , aaSuffix , miSuffix , faSuffix])

-- expression :: JParser Expression
-- expression = choice (map try
--            [ lambdaExpression
--            , Expression <$> assignmentExpression
--            ]) <?> "expression"

-- lambdaExpression :: JParser Expression
-- lambdaExpression = LambdaExpression
--                 <$> lambdaParameters
--                 <*> (operator "-" *> operator ">" *> lambdaBody)
--                 <?> "lambda expression"

-- lambdaParameters :: JParser LambdaParameters
-- lambdaParameters = choice (map try
--                  [ LIdent <$> ident
--                  , lParen *> (LFormalParameterList <$> optionMaybe
--                                 formalParameterList) <* rParen
--                  , lParen *> (LInferredFormalParameterList <$>
--                                 inferredFormalParameterList) <* rParen
--                  ]) <?> "lambda parameters"

-- inferredFormalParameterList :: JParser InferredFormalParameterList
-- inferredFormalParameterList = ident `sepBy1` comma

-- constantExpression :: JParser Expression
-- constantExpression = expression

-- lambdaBody :: JParser LambdaBody
-- lambdaBody = try (Lambda <$> expression) <|> (LambdaBlock <$> block)

-- -- | Expression surrounded by parenthesis
-- expressionParen :: JParser Primary
-- expressionParen = lParen *> (Expr <$> expression) <* rParen

-- -- | Java name followed by a .class
-- -- | e.g foo.bar.qux.class
-- typeNameDotClass :: JParser Primary
-- typeNameDotClass = TypeNameDotClass
--         <$> typeNameDot
--         <*  keyword "class"
--         <?> "typename.class"

-- -- | Java array name followed by a .class
-- typeNameArrDotClass :: JParser Primary
-- typeNameArrDotClass = TypeNameArrDotClass
--         <$> typeName
--         <*> dims
--         <*  dot <* keyword "class"
--         <?> "typename[].class"

-- -- | Parses void.class
-- voidDotClass :: JParser Primary
-- voidDotClass = pure VoidDotClass
--         <* keyword "void"
--         <* dot <* keyword "class"
--         <?> "void.class"

-- -- | Java name followed by this
-- -- | e.g foo.bar.this
-- typeNameDotThis :: JParser Primary
-- typeNameDotThis = TypeNameDotThis
--         <$> typeNameDot
--         <* keyword "this"

-- -- | The literal this expression
-- this :: JParser Primary
-- this = pure This <* keyword "this"

-- -- | Class instance creation expression
-- classInstanceCreationExpression :: JParser Primary
-- classInstanceCreationExpression = choice (map try
--     [ ClassInstanceCreationExpression <$> withIdentifier
--     , ClassInstanceCreationExpression <$> withExpressionName
--     ]) <?> "class instance creation"

-- -- | Suffix for Class Instance creation.
-- ciceSuffix :: JParser (Primary -> Primary)
-- ciceSuffix = do
--         ta0 <- dot *> keyword "new" *> optionMaybe typeArgs
--         id0 <- ident
--         tad0 <- optionMaybe typeArgsOrDiamond
--         argList0 <- between lParen rParen (optionMaybe argList)
--         cb0 <- optionMaybe classBody
--         return (\x -> ClassInstanceCreationExpression
--                             (WithPrimary x ta0 id0 tad0 argList0 cb0))

-- withIdentifier :: JParser ClassInstanceCreation
-- withIdentifier = WithIdentifier
--         <$> (keyword "new" *> optionMaybe typeArgs)
--         <*> ident
--         <*> optionMaybe typeArgsOrDiamond
--         <*> between lParen rParen (optionMaybe argList)
--         <*> optionMaybe classBody

-- withExpressionName :: JParser ClassInstanceCreation
-- withExpressionName = WithExpressionName
--         <$> typeName
--         <*> (dot *> keyword "new" *> optionMaybe typeArgs)
--         <*> optionMaybe typeArgsOrDiamond
--         <*> between lParen rParen (optionMaybe argList)
--         <*> optionMaybe classBody

-- typeArgsOrDiamond :: JParser TypeArgsOrDiam
-- typeArgsOrDiamond =  try (TypeArgs <$> typeArgs)
--                 <|> (pure Diamond <* lessThan <* greaterThan)

-- argList :: JParser ArgList
-- argList = ArgList <$> expression `sepBy1` comma

-- dotPrim :: JParser (Primary -> Primary -> Primary)
-- dotPrim = dot *> return Dot

-- fieldAccess :: JParser Primary
-- fieldAccess = FieldAccess <$> choice
--         [ SelfParentFieldAccess <$>  (keyword "super" *> dot *> ident)
--         , SelfParentFieldAccess <$>  (keyword "this" *> dot *> ident)
--         , ParentFieldAccess <$> (typeNameDot <* keyword "super") <*> (dot *> ident)
--         ]

-- faSuffix :: JParser (Primary -> Primary)
-- faSuffix = do
--             i <- dot *> ident
--             return (\x -> FieldAccess (ExprFieldAccess x i))

-- arrayAccess :: JParser Primary
-- arrayAccess = ArrayAccess <$>
--     (NormalArrayAccess <$> typeName
--                        <*> (lSquare *> expression <* rSquare))

-- aaSuffix = do
--             i <- lSquare *> expression <* rSquare
--             return (\x -> ArrayAccess (ExprArrayAccess x i))


-- methodInvocation :: JParser Primary
-- methodInvocation = choice
--         [ MethodInvocation <$> normalMethodInvocation
--         , MethodInvocation <$> nameMethodInvocation
--         , MethodInvocation <$> selfParentMethodInvocation
--         , MethodInvocation <$> parentMethodInvocation
--         ] <?> "method invocation"

-- miSuffix :: JParser (Primary -> Primary)
-- miSuffix = do
--             typeArgs0 <- dot *> optionMaybe typeArgs
--             ident0 <- ident
--             argList0 <- lParen *> optionMaybe argList <* rParen
--             return (\x -> MethodInvocation (
--                             ExprMethodInvocation
--                                 x typeArgs0 ident0 argList0))

-- normalMethodInvocation :: JParser MethodInvocation
-- normalMethodInvocation = NormalMethodInvocation
--         <$> typeName
--         <*> between lParen rParen (optionMaybe argList)

-- nameMethodInvocation :: JParser MethodInvocation
-- nameMethodInvocation = NameMethodInvocation
--         <$> typeNameDot
--         <*> optionMaybe typeArgs
--         <*> ident
--         <*> between lParen rParen (optionMaybe argList)

-- selfParentMethodInvocation :: JParser MethodInvocation
-- selfParentMethodInvocation = SelfParentMethodInvocation
--         <$> (keyword "super" *> dot *> optionMaybe typeArgs)
--         <*> ident
--         <*> between lParen rParen (optionMaybe argList)

-- parentMethodInvocation :: JParser MethodInvocation
-- parentMethodInvocation = ParentMethodInvocation
--         <$> typeNameDot
--         <*> (keyword "super" *> dot *> optionMaybe typeArgs)
--         <*> ident
--         <*> between lParen rParen (optionMaybe argList)

-- -- TODO Resolve this
-- methodReference :: JParser Primary
-- methodReference = MethodReference <$> choice (map try
--         [ nameMethodReference
--         , refTypeMethodReference
--         , selfParentMethodReference
--         , parentMethodReference
--         , classTypeMethodReference
--         , arrayTypeMethodReference
--         ]) <?> "method reference"

-- mrSuffix :: JParser (Primary -> Primary)
-- mrSuffix = do
--             typeArg0 <- dColon *> optionMaybe typeArgs
--             ident0 <- ident
--             return (\x -> MethodReference (ExprMR x typeArg0 ident0))

-- nameMethodReference :: JParser MethodReference
-- nameMethodReference = NameMR
--         <$> typeName
--         <*> (dColon *> optionMaybe typeArgs)
--         <*> ident

-- refTypeMethodReference :: JParser MethodReference
-- refTypeMethodReference = RefTypeMR
--         <$> refType
--         <*> (dColon *> optionMaybe typeArgs)
--         <*> ident

-- exprMethodReference :: JParser MethodReference
-- exprMethodReference = ExprMR
--         <$> (primary `chainl1` dotPrim)
--         <*> (dColon *> optionMaybe typeArgs)
--         <*> ident

-- selfParentMethodReference :: JParser MethodReference
-- selfParentMethodReference = SelfParentMR
--         <$> (keyword "super" *> dColon *> optionMaybe typeArgs)
--         <*> ident

-- parentMethodReference :: JParser MethodReference
-- parentMethodReference = ParentMR
--         <$> typeNameDot
--         <*> (keyword "super" *> dColon *> optionMaybe typeArgs)
--         <*> ident

-- classTypeMethodReference :: JParser MethodReference
-- classTypeMethodReference = ClassTypeMR
--         <$> classType
--         <*> (dColon *> optionMaybe typeArgs <* keyword "new")

-- arrayTypeMethodReference :: JParser MethodReference
-- arrayTypeMethodReference = ArrayTypeMR
--         <$> (arrayType <* dColon <* keyword "new")

-- dimExprs :: JParser [DimExpr]
-- dimExprs = many1 (DimExpr <$> (lSquare *> optionMaybe expression <* rSquare))

-- arrayCreationExpr :: JParser ArrayCreationExpr
-- arrayCreationExpr = choice (map try
--          [  primTypeACE
--          ,  classTypeACE
--          ,  primTypeACEI
--          ,  classTypeACEI
--          ]) <?> "array creation expression"

-- primTypeACE :: JParser ArrayCreationExpr
-- primTypeACE = do
--         pt <- keyword "new" *> primType
--         de <- dimExprs
--         m  <- optionMaybe dims
--         return $ PrimTypeACE pt de (length de + fromMaybe 0 m)

-- classTypeACE :: JParser ArrayCreationExpr
-- classTypeACE = do
--         ct <- keyword "new" *> classType
--         de <- dimExprs
--         m  <- optionMaybe dims
--         return $ ClassTypeACE ct de (length de + fromMaybe 0 m)

-- primTypeACEI :: JParser ArrayCreationExpr
-- primTypeACEI = PrimTypeACEI
--         <$> (keyword "new" *> primType)
--         <*> dims
--         <*> arrayInitializer

-- classTypeACEI :: JParser ArrayCreationExpr
-- classTypeACEI = ClassTypeACEI
--         <$> (keyword "new" *> classType)
--         <*> dims
--         <*> arrayInitializer

-- assignmentOperator :: JParser T
-- assignmentOperator = choice (map operator [
--          "=", "*=", "/=", "%=", "+=", "-=", "<<=",
--          ">>=", ">>>=", "&=","^=", "|="
--         ])
--         <?> "assignment operator"

-- leftHandSide :: JParser LHS
-- leftHandSide = try (LHSExpr <$> choice [ fieldAccess, arrayAccess ])
--         <|> (LHSIdent <$> ident)
--         <?> "lhs"

-- assignmentExpression :: JParser AssignmentExpression
-- assignmentExpression = try assignment <|> (Term <$> term)

-- assignment :: JParser AssignmentExpression
-- assignment = Assignment
--        <$> leftHandSide
--        <*> assignmentOperator
--        <*> expression
--        <?> "assignment"

-- {-
-- castExpr :: JParser CastExpression
-- castExpr = choice
--        [ UnaryToPrim <$> (lParen *> primType <* rParen) <*> unaryExpr
--        , UnaryToRef <$> (lParen *> refType) <*> (many classType <* rParen)
--                     <*> unaryExprNPM
--        , LambdaToRef <$> (lParen *> refType) <*> (many classType <* rParen)
--                     <*> lambdaExpression
--        ] <?> "cast expression"
-- -}

-- term :: JParser Term
-- term = buildExpressionParser table opExpr <?> "term"

-- opExpr :: JParser Term
-- opExpr = choice $ map try [ PrimExpr <$> primary, NameExpr <$> typeName ]

-- -- | Operator precedence table
-- table  = [ postfix <$> [ "++", "--" ]
--          , prefix  <$> [ "++", "--", "+", "-", "~", "!" ]
--          , binary  <$> [ "*", "/", "%" ]
--          , binary  <$> [ "+", "-" ]
--          , binary  <$> [ "<<", ">>", ">>>" ]
--          , (binary <$> [ "<", ">", "<=", ">=" ]) ++ [instanceof]
--          , binary  <$> [ "==", "!=" ]
--          , binary  <$> [ "&" ]
--          , binary  <$> [ "^"]
--          , binary  <$> [ "|" ]
--          , binary  <$> [ "&&" ]
--          , binary  <$> [ "||" ]
--          , [conditionalExpr]
--          ]

-- instanceof = Postfix (InstanceOfExpr <$> (keyword "instanceof" *> refType))
-- binary op = Infix (BinaryExpr <$> operator op) AssocLeft
-- postfix op = Postfix (PostfixExpr <$> operator op)
-- prefix op = Prefix (PrefixExpr <$> operator op)
-- conditionalExpr = Infix (ConditionalExpr <$>
--                             (operator "?" *> expression <* operator ":"))
--                         AssocRight

-- primitiveTypes = S.fromList
--       [ "byte" , "short" , "int" , "long" , "char",
--         "float" , "double" , "boolean"]
