module GearScript.Lexer where

import Control.Applicative((<$>), (<*>), (<$), (*>))
import Control.Monad
import Data.List
import Data.Maybe(catMaybes)
import Data.Text hiding (concat, intercalate)
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
           | IndexBegin
           | IndexEnd
           | StmtEnd
           | ArgumentSeparator
           | Plus
           | Minus
           | Mult
           | Div
           | Mod
           | QuestionMark
           | Colon
           | StringLiteral String
           | TaggedStringLiteral String
           | NumberLiteral Double
           | Not
           | Set
           | Eq
           | Neq
           | Lt
           | Lte
           | Gt
           | Gte
           | Dot
           | Comment String
           | BoolAnd
           | BoolOr
           deriving (Show, Eq)

type GSToken = (SourcePos, Token)

tok :: Monad m => ParsecT s u m a -> ParsecT s u m (SourcePos, a)
tok p = (,) <$> getPosition <*> p

underscore :: Parser Char
underscore = char '_'

nameFirstChar :: Parser Char
nameFirstChar = letter <|> underscore

doubleColon :: Parser String
doubleColon = string "::"

nameChar :: Parser Char
nameChar = nameFirstChar <|> digit

nameChars :: Parser String
nameChars = intercalate "::" <$> (liftM2 (:) nameFirstChar (many nameChar) `sepBy1` doubleColon)

name :: Parser GSToken
name = tok (Name <$> nameChars) <?> "name"

stringChar :: Parser Char -> Parser String
stringChar delim = try (sequence [char '\\', delim])
               <|> return <$> anyChar

str' :: Parser Char -> Parser GSToken
str' delim = delim *> tok (StringLiteral . concat <$> manyTill (stringChar delim) delim)

str :: Parser GSToken
str = str' $ char '\"'

taggedStr :: Parser GSToken
taggedStr = str' $ char '\''

number :: Parser GSToken
number = tok (fmap (NumberLiteral . read) . (++) <$> many1 digit <*> (maybe "" ('.' :) <$> optionMaybe (char '.' *> many1 digit)))

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

lineComment :: Parser GSToken
lineComment = tok $ Comment <$> (string "//" *> manyTill anyChar (try ((() <$ newline) <|> eof)))

blockComment :: Parser GSToken
blockComment = tok $ Comment <$> (string "/*" *> manyTill anyChar (try $ string "*/"))

comment :: Parser GSToken
comment = try lineComment <|> try blockComment

gsToken :: Parser GSToken
gsToken = globalVariable
      <|> localVariable
      <|> try (tok (KeywordTok <$> keyword))
      <|> annotation
      <|> name
      <|> comment
      <|> tok (BlockBegin <$ char '{')
      <|> tok (BlockEnd <$ char '}')
      <|> tok (ParenBegin <$ char '(')
      <|> tok (ParenEnd <$ char ')')
      <|> tok (IndexBegin <$ char '[')
      <|> tok (IndexEnd <$ char ']')
      <|> tok (StmtEnd <$ char ';')
      <|> tok (ArgumentSeparator <$ char ',')
      <|> tok (Plus <$ char '+')
      <|> tok (Minus <$ char '-')
      <|> tok (Mult <$ char '*')
      <|> tok (Div <$ char '/')
      <|> tok (Mod <$ char '%')
      <|> tok (QuestionMark <$ char '?')
      <|> tok (Colon <$ char ':')
      <|> tok (Eq <$ try (string "=="))
      <|> tok (Neq <$ try (string "!="))
      <|> tok (Lte <$ try (string "<="))
      <|> tok (Lt <$ try (string "<"))
      <|> tok (Gte <$ try (string ">="))
      <|> tok (Gt <$ try (string ">"))
      <|> tok (Not <$ char '!')
      <|> tok (Set <$ char '=')
      <|> tok (Dot <$ char '.')
      <|> tok (BoolAnd <$ try (string "&&"))
      <|> tok (BoolOr <$ try (string "||"))
      <|> str
      <|> taggedStr
      <|> number

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
