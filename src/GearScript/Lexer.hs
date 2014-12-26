module GearScript.Lexer where

import Control.Applicative((<$>), (<*>), (<$), (*>))
import Control.Monad
import Data.Maybe(catMaybes)
import Data.Text hiding (concat)
import GearScript.Util
import Text.Parsec hiding (tokens)
import Text.Parsec.Text
import Text.Parsec.Prim hiding (tokens)

data Keyword = Function
             | Class
             | Break
             | Return
             | Case
             deriving (Show, Eq)

data Token = GlobalVariable String
           | LocalVariable String
           | Name String
           | Annotation String
           | KeywordTok Keyword
           | BlockBegin
           | BlockEnd
           | ParenBegin
           | ParenEnd
           | StmtEnd
           | ArgumentSeparator
           | Plus
           | Minus
           | Mult
           | Div
           | Mod
           | Colon
           | StringLiteral String
           | NumberLiteral Double
           deriving (Show, Eq)

type GSToken = (SourcePos, Token)

tok :: Monad m => ParsecT s u m a -> ParsecT s u m (SourcePos, a)
tok p = (,) <$> getPosition <*> p

underscore :: Parser Char
underscore = char '_' <?> "underscore"

nameFirstChar :: Parser Char
nameFirstChar = letter <|> underscore

nameChar :: Parser Char
nameChar = nameFirstChar <|> digit

nameChars :: Parser String
nameChars = liftM2 (:) nameFirstChar (many nameChar)

name :: Parser GSToken
name = tok (Name <$> nameChars) <?> "name"

stringChar :: Parser String
stringChar = try $ string "\""
         <|> return <$> anyChar

str :: Parser GSToken
str = char '\"' *> tok (StringLiteral . concat <$> manyTill stringChar (char '\"'))

globalVariable :: Parser GSToken
globalVariable = tok $ char '$' >> GlobalVariable <$> nameChars

localVariable :: Parser GSToken
localVariable = tok $ char '%' >> LocalVariable <$> nameChars

annotation :: Parser GSToken
annotation = tok $ char '@' >> Annotation <$> nameChars

keyword :: Parser Keyword
keyword = do
    kw <- tryChoice
        [ Function <$ string "function"
        , Class <$ string "class"
        , Break <$ string "break"
        , Return <$ string "return"
        , Case <$ string "case"
        ]
    notFollowedBy nameChar
    return kw

gsToken :: Parser GSToken
gsToken = globalVariable
      <|> localVariable
      <|> try (tok (KeywordTok <$> keyword))
      <|> annotation
      <|> name
      <|> tok (BlockBegin <$ char '{')
      <|> tok (BlockEnd <$ char '}')
      <|> tok (ParenBegin <$ char '(')
      <|> tok (ParenEnd <$ char ')')
      <|> tok (StmtEnd <$ char ';')
      <|> tok (ArgumentSeparator <$ char ',')
      <|> tok (Plus <$ char '+')
      <|> tok (Minus <$ char '-')
      <|> tok (Mult <$ char '*')
      <|> tok (Div <$ char '/')
      <|> tok (Mod <$ char '%')
      <|> tok (Colon <$ char ':')
      <|> str

whitespace :: Parser ()
whitespace = () <$ (space <|> newline)

gsTokens :: Parser [GSToken]
gsTokens = do
    tokens <- many $ choice
        [ Just <$> gsToken
        , Nothing <$ whitespace
        ]
    eof
    return $ catMaybes tokens

lexGS :: SourceName -> Text -> Either ParseError [GSToken]
lexGS = runP gsTokens ()
