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
                   staticModifier :: JavaStaticModifier }
        deriving (Show)

type JavaStaticModifier = Bool

data JavaVisibilityModifier = Public | Protected | Private | DefaultAccess
instance Show JavaVisibilityModifier where
    show Public = "public"
    show Protected = "protected"
    show Private = "private"
    show DefaultAccess = ""

type JavaToken = String
