module Main where

import GearScript
import Control.Applicative
import Data.Maybe
import System.Environment

main :: IO ()
main = do
    path <- fromMaybe "Examples/Example1" . listToMaybe <$> getArgs
    compiled <- compile path
    putStrLn compiled
