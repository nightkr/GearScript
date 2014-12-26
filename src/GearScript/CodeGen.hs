module GearScript.CodeGen where

import Prelude
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
-- import Data.List
import qualified  Data.Text as T
import GearScript.AST

data PPrintEnv = PPrintEnv
    { indentLevel :: Int
    }

type PPrinter = ReaderT PPrintEnv (Writer T.Text) ()

indent :: PPrintEnv -> PPrintEnv
indent e = e { indentLevel = indentLevel e + 1 }

runIndent :: Int -> T.Text
runIndent levels = T.replicate (4 * levels) " "

line :: T.Text -> PPrinter
line str = do
    indentCount <- indentLevel <$> ask
    let indentation = runIndent indentCount
    lift $ tell $ T.concat [indentation, str, "\r\n"]

pprintArgList :: [T.Text] -> T.Text
pprintArgList args = T.concat ["(", T.intercalate ", " args, ")"]

pprintBinaryOp :: T.Text -> Expression -> Expression -> T.Text
pprintBinaryOp op one two = T.concat ["(", pprintExpression one, " ", op, " ", pprintExpression two, ")"]

pprintExpression :: Expression -> T.Text
pprintExpression (Call (Just obj) name args) = T.concat [pprintExpression obj, ".", pprintExpression (Call Nothing name args)]
pprintExpression (Call Nothing name args) = T.concat [T.pack name, pprintArgList (pprintExpression <$> args)]
pprintExpression (GlobalVariable name) = "$" `T.append` T.pack name
pprintExpression (LocalVariable name) = "%" `T.append` T.pack name
pprintExpression (Plus one two) = pprintBinaryOp "+" one two
pprintExpression (Minus one two) = pprintBinaryOp "-" one two
pprintExpression (Mult one two) = pprintBinaryOp "*" one two
pprintExpression (Div one two) = pprintBinaryOp "/" one two
pprintExpression (Mod one two) = pprintBinaryOp "%" one two
pprintExpression (StringLiteral x) = T.concat ["\"", T.pack x, "\""]
pprintExpression (NumberLiteral x) = T.pack $ show x

pprintStatement :: Statement -> PPrinter
pprintStatement (ExprStatement expr) = line $ pprintExpression expr `T.append` ";"

pprintBlock :: [Statement] -> PPrinter
pprintBlock body = do
    line "{"
    local indent $ sequence_ $ pprintStatement <$> body
    line "}"


pprintTopStatement :: TopStatement -> PPrinter
pprintTopStatement (TopPlainStatement stmt) = pprintStatement stmt
pprintTopStatement (FunctionDef name args body) = do
    line $ T.concat ["function ", T.pack name, pprintArgList $ T.pack <$> args]
    pprintBlock body

pprintTopStatements :: [TopStatement] -> PPrinter
pprintTopStatements stmts = sequence_ $ pprintTopStatement <$> stmts

doPprint :: [TopStatement] -> String
doPprint stmts = T.unpack $ execWriter $ runReaderT (pprintTopStatements stmts) (PPrintEnv 0)
