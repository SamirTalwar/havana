module Havana.AST where

data AST = JavaClass {
         filePath :: FilePath,
         className :: JavaToken,
         classAccessModifiers :: JavaAccessModifiers,
         methods :: [JavaMethod] }
        deriving (Show)

data JavaType = Void
instance Show JavaType where
    show Void = "void"

data JavaMethod = JavaMethod {
                methodName :: JavaToken,
                methodAccessModifiers :: JavaAccessModifiers,
                returnType :: JavaType }
        deriving (Show)

data JavaAccessModifiers = JavaAccessModifiers {
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
