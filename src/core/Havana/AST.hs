module Havana.AST where

data AST = JavaClass { filePath :: FilePath, className :: JavaToken, methods :: [JavaMethod] }
        deriving (Show)

data JavaType = Void
instance Show JavaType where
    show Void = "void"

data JavaMethod = JavaMethod {
                methodName :: JavaToken,
                returnType :: JavaType,
                visibilityModifier :: JavaVisibilityModifier }
        deriving (Show)

data JavaVisibilityModifier = Public | Protected | Private | DefaultAccess
instance Show JavaVisibilityModifier where
    show Public = "public"
    show Protected = "protected"
    show Private = "private"
    show DefaultAccess = ""

type JavaToken = String
