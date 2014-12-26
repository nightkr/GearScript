module Main where

import GearScript.AST
import GearScript.Lexer
import GearScript.Parser
import GearScript.CodeGen
import qualified Data.Text as T

liftEitherM :: (Monad m, Show a) => Either a b -> m b
liftEitherM (Left x) = fail $ show x
liftEitherM (Right x) = return x

parse :: FilePath -> IO [TopStatement]
parse file = do
    input <- readFile file
    tokens <- liftEitherM $ lexGS file $ T.pack input
    liftEitherM $ parseGS file tokens

main :: IO ()
main = do
    ast <- parse "Examples/Example1.gs"
    let result = doPprint ast
    putStrLn result
    writeFile "Examples/Example1.cs" result
