module GearScript.Parser where

import qualified GearScript.Lexer as L
import Control.Applicative((<$>), (<$), (<*), (*>), (<*>))
import Text.Parsec.Error
import Text.Parsec.Pos
import Text.Parsec.Prim
import Text.Parsec.Combinator
import GearScript.Util
import GearScript.AST

type Parser = Parsec [L.GSToken] ()

satisfyM :: (L.Token -> Maybe a) -> Parser a
satisfyM f = token showTok posFromTok (f . snd)
    where showTok (_, t) = show t
          posFromTok (pos, _) = pos

satisfy :: (L.Token -> Bool) -> Parser L.Token
satisfy f = satisfyM cond
    where cond x | f x = Just x
                 | otherwise = Nothing

tok :: L.Token -> Parser L.Token
tok x = satisfy (==x)

takeName :: L.Token -> Maybe String
takeName (L.Name n) = Just n
takeName _ = Nothing

localVariable :: Parser String
localVariable = satisfyM takeVar
    where takeVar (L.LocalVariable n) = Just n
          takeVar _ = Nothing

globalVariable :: Parser String
globalVariable = satisfyM takeVar
    where takeVar (L.GlobalVariable n) = Just n
          takeVar _ = Nothing

variable :: Parser Expression
variable = LocalVariable <$> localVariable
       <|> GlobalVariable <$> globalVariable

stringLiteral :: Parser Expression
stringLiteral = satisfyM takeLit
    where takeLit (L.StringLiteral x) = Just $ StringLiteral x
          takeLit _ = Nothing

parens :: Parser a -> Parser a
parens p = do
    _ <- tok L.ParenBegin
    result <- p
    _ <- tok L.ParenEnd
    return result

functionDef :: Parser TopStatement
functionDef = do
    _ <- tok $ L.KeywordTok L.Function
    funcName <- satisfyM takeName
    args <- parens (localVariable `sepBy` tok L.ArgumentSeparator)
    body <- block
    return $ FunctionDef funcName args body

functionCall :: Parser Expression
functionCall = do
    funcName <- satisfyM takeName
    args <- parens (expr `sepBy` tok L.ArgumentSeparator)
    return $ Call Nothing funcName args

expr :: Parser Expression
expr = term `chainl1` exprOp

exprOp :: Parser (Expression -> Expression -> Expression)
exprOp = Plus <$ tok L.Plus
     <|> Minus <$ tok L.Minus

term :: Parser Expression
term = factor `chainl1` termOp

termOp :: Parser (Expression -> Expression -> Expression)
termOp = Mult <$ tok L.Mult
     <|> Div <$ tok L.Div
     <|> Mod <$ tok L.Mod

factor :: Parser Expression
factor = tryChoice
           [ functionCall
           , variable
           , stringLiteral
           ]

block :: Parser [Statement]
block = tok L.BlockBegin *> many statement <* tok L.BlockEnd

statementOp :: Parser Statement
statementOp = tryChoice
                [ ExprStatement <$> expr
                ]

statement :: Parser Statement
statement = statementOp <* tok L.StmtEnd

topLevelStatement :: Parser TopStatement
topLevelStatement = tryChoice
                      [ TopPlainStatement <$> statement
                      , functionDef
                      ]

parseGS :: SourceName -> [L.GSToken] -> Either ParseError [TopStatement]
parseGS = runP (many topLevelStatement <* eof) ()