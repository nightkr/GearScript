module Main where

import GearScript.AST
import GearScript.Lexer(lexGS)
import GearScript.Parser(parseGS)
import GearScript.CodeGen(doPprint)
import Control.Applicative
import Data.Maybe
import qualified Data.Text as T
import System.Environment

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
    path <- fromMaybe "Examples/Example1" . listToMaybe <$> getArgs
    ast <- parse $ path ++ ".gs"
    let result = doPprint ast
    putStrLn result
    writeFile (path ++ ".cs") result
