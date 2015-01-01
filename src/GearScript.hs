module GearScript where

import qualified Data.Text as T
import GearScript.AST
import GearScript.Lexer(lexGS)
import GearScript.Parser(parseGS)
import GearScript.CodeGen(doPprint)

liftEitherM :: (Monad m, Show a) => Either a b -> m b
liftEitherM (Left x) = fail $ show x
liftEitherM (Right x) = return x

parse :: FilePath -> IO [TopStatement]
parse file = do
    input <- readFile file
    tokens <- liftEitherM $ lexGS file $ T.pack input
    liftEitherM $ parseGS file tokens

compile :: String -> IO String
compile path = do
    ast <- parse $ path ++ ".gs"
    let result = doPprint ast
    writeFile (path ++ ".cs") result
    return result
