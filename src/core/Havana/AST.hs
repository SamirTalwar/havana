module Havana.AST where

data AST = JavaClass {
         filePath :: FilePath,
         className :: JavaToken,
         classModifiers :: JavaModifiers,
         methods :: [JavaMethod],
         classLineNumber :: LineNumber }
        deriving (Eq, Show)

data JavaType = Void | Int
        deriving (Eq)
instance Show JavaType where
    show Void = "void"
    show Int = "int"

data JavaMethod = JavaMethod {
                methodName :: JavaToken,
                methodModifiers :: JavaModifiers,
                methodParameters :: [JavaParameter],
                returnType :: JavaType,
                methodLineNumber :: LineNumber }
        deriving (Eq, Show)

data JavaParameter = JavaParameter {
                   parameterName :: JavaToken,
                   parameterType :: JavaType }
        deriving (Eq, Show)

data JavaModifiers = JavaModifiers {
                   visibilityModifier :: JavaVisibilityModifier,
                   hierarchyModifier :: JavaHierarchyModifier,
                   staticModifier :: JavaStaticModifier }
        deriving (Eq, Show)

data JavaVisibilityModifier = Public | Protected | Private | DefaultAccess
        deriving (Eq)
instance Show JavaVisibilityModifier where
    show Public = "public"
    show Protected = "protected"
    show Private = "private"
    show DefaultAccess = ""

data JavaHierarchyModifier = NoHierarchy | Abstract | Final
        deriving (Eq)
instance Show JavaHierarchyModifier where
    show NoHierarchy = ""
    show Abstract = "abstract"
    show Final = "final"

type JavaStaticModifier = Bool

exceptHierarchy (JavaModifiers visibilityModifier _ staticModifier)
    = JavaModifiers visibilityModifier NoHierarchy staticModifier

type JavaToken = String

type LineNumber = Int
