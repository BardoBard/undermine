module Modes.NewSave.NewSave where

import System.Console.ANSI ( clearScreen )
import System.TimeIt ( timeIt )
import Data.List ( transpose )

--My Libs
import Modes.Shared ( rooms, showDisplay, getIndex, replaceAt', sum', dibbleItem )
import Modes.Json.Shared ( newsaveDisplay, newsaveWeight )
import Modes.Newsave.Json.Parser ( starterDisplay, starterWeight)
import Modes.Error ( findIndexWithError, nextRoom, amount)
import Basic 

starterToNewsave y = case y of
     0 -> 5
     1 -> 31
     2 -> 0
     3 -> 22
     4 -> 7
     5 -> 44
     6 -> 25
     7 -> 6
     8 -> 50
     _ -> -1

-- |
-- Backtracking for new save
-- 
-- Note: if user input is -1, due to the laziness of haskell it won't calculate the relics, it'll just skip it
--
-- e.g | newsaveBacktracking [5, -1, -1, -1, -1, 6, 7, -1, -1, -1, -1, -1, -1, 8, -1, -1, -1, -1, -1, -1] 2 -> 
--
-- returns: [seed]
--newsaveBacktracking :: [Int] -> Int -> [Int]
newsaveBacktracking list n = do
    starterItems <- starterWeight
    return $ take n [seed |
        -- TODO: change -1 to a maybe
        let sum0     =  sum starterItems,

        --Seed
        seed <- [0..99999999],

        --Tutorial
        let x1       =  getIndex (seed + 1) starterItems sum0, --find relic in starter relics
        [x1]         == take 1 list,

        --Mine 2
        let index1   = list !! 1,
        let items1   = replaceAt (starterToNewsave x1) 0 newsaveWeight, --replace last found relic to not show up again
        let sum1     = sum items1, --get the sum of items
        let x2       = getIndex (seed + 2) items1 sum1, --find relic specific relic
        x2 == index1 || index1 == (-1), --check if user doesn't want relic or relic is what user wants

        --Mine 3
        let index2   = list !! 2,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 -  sum' index1 x2 items1,
        let x1       = getIndex (seed + 3) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Mine 4
        let index1   = list !! 3,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 4) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Dungeon 1
        let index2   = list !! 4,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 6) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Dungeon 2
        let index1   = list !! 5,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 7) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Dungeon 3
        let index2   = list !! 6,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 8) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Dungeon 4
        let index1   = list !! 7,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 9) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Halls 1
        let index2   = list !! 8,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 11) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Halls 2
        let index1   = list !! 9,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 12) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Halls 3
        let index2   = list !! 10,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 13) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Halls 4
        let index1   = list !! 11,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 14) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Caverns 1
        let index2   = list !! 12,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 16) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Caverns 2
        let index1   = list !! 13,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 17) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Caverns 3
        let index2   = list !! 14,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 18) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Caverns 4
        let index1   = list !! 15,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 19) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Core 1
        let index2   = list !! 16,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 21) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Core 2
        let index1   = list !! 17,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 22) items1 sum1,
        x2 == index1 || index1 == (-1),

        --Core 3
        let index2   = list !! 18,
        let items2   = replaceAt' index1 x2 items1, --replace last found relic to not show up again
        let sum2     = sum1 - sum' index1 x2 items1,
        let x1       = getIndex (seed + 23) items2 sum2,
        x1 == index2 || index2 == (-1),

        --Core 4
        let index1   = list !! 19,
        let items1   = replaceAt' index2 x1 items2, --replace last found relic to not show up again
        let sum1     = sum2 - sum' index2 x1 items2,
        let x2       = getIndex (seed + 24) items1 sum1,
        x2 == index1 || index1 == (-1)


        ]

newsaveMain = do
    starterDisplay <- starterDisplay
    clearScreen
    putStr "\n\n"
    putStr "You are in newsave, if you'd like to quit press CTRL + C,"
    putStr "\nIf you'd like to skip a relic press enter. (after mine 1)"
    putStr "\n\n"
    putStr "\n\n\n\n\n\n\n\n\n\n\n"
    putStr "Pick one of the following: "
    putStr "\n\n"
    print starterDisplay
    putStr "\n"
    print "Mine 1: "
    x1 <- getLine :: IO String
    x1 <- findIndexWithError x1 "Mine 1: " starterDisplay

    userInput' <- mapM (`nextRoom` newsaveDisplay) (tail rooms)

    let userInput = x1 : userInput'

    clearScreen
    print "Amount of output: "
    n <- amount "Amount of output: "

    print "This may take a few minutes"
    x <- newsaveBacktracking userInput n
    timeIt $ print x
    print "Finished"

    appendFile "Output.txt" $ "\n\n" ++ "newsave permutations:" ++ " \n" ++ show (transpose [rooms, (starterDisplay !! x1) : showDisplay newsaveDisplay (tail userInput)]) ++ "\nseeds: " ++ show x

    getLine
    