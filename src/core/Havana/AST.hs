module Havana.AST where

data AST = JavaClass {
         filePath :: FilePath,
         className :: JavaToken,
         classModifiers :: JavaModifiers,
         methods :: [JavaMethod],
         classLineNumber :: LineNumber }
        deriving (Show)

data JavaType = Void | Int
instance Show JavaType where
    show Void = "void"
    show Int = "int"

data JavaMethod = JavaMethod {
                methodName :: JavaToken,
                methodModifiers :: JavaModifiers,
                methodParameters :: [JavaParameter],
                returnType :: JavaType,
                methodLineNumber :: LineNumber }
        deriving (Show)

data JavaParameter = JavaParameter {
                   parameterName :: JavaToken,
                   parameterType :: JavaType }
        deriving (Show)

data JavaModifiers = JavaModifiers {
                   visibilityModifier :: JavaVisibilityModifier,
                   hierarchyModifier :: JavaHierarchyModifier,
                   staticModifier :: JavaStaticModifier }
        deriving (Show)

data JavaVisibilityModifier = Public | Protected | Private | DefaultAccess
instance Show JavaVisibilityModifier where
    show Public = "public"
    show Protected = "protected"
    show Private = "private"
    show DefaultAccess = ""

data JavaHierarchyModifier = NoHierarchy | Abstract | Final
instance Show JavaHierarchyModifier where
    show NoHierarchy = ""
    show Abstract = "abstract"
    show Final = "final"

type JavaStaticModifier = Bool

exceptHierarchy (JavaModifiers visibilityModifier _ staticModifier)
    = JavaModifiers visibilityModifier NoHierarchy staticModifier

type JavaToken = String

type LineNumber = Int
