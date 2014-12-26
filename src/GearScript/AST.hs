module GearScript.AST where

data Type = Type
              { typeName :: String
              , typeParent :: Maybe Type
              }
          deriving (Show, Eq)

data Typed a = Typed
    { typedType :: Maybe Type
    , typedVal :: a
    }

data TopStatement = FunctionDef
                      { functionName :: String
                      , functionArguments :: [String]
                      , functionBody :: [Statement]
                      }
                  | TopPlainStatement Statement
                  deriving (Show, Eq)

data Statement = ExprStatement Expression
               deriving (Show, Eq)

data Expression = Call
                    { callObject :: Maybe Expression
                    , callFunctionName :: String
                    , callArguments :: [Expression]
                    }
                | GlobalVariable String
                | LocalVariable String
                | StringLiteral String
                | NumberLiteral Double
                | Plus Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                deriving (Show, Eq)
