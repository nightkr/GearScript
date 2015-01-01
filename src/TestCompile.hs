module Main where

import GearScript(compile)

main :: IO ()
main = do
    _ <- compile "Examples/Gamemode_Deathmatch"
    return ()