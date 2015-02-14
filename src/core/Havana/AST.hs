module Havana.AST where

data AST = JavaClass {
         filePath :: FilePath,
         className :: JavaToken,
         classModifiers :: JavaModifiers,
         methods :: [JavaMethod] }
        deriving (Show)

data JavaType = Void
instance Show JavaType where
    show Void = "void"

data JavaMethod = JavaMethod {
                methodName :: JavaToken,
                methodModifiers :: JavaModifiers,
                returnType :: JavaType }
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
