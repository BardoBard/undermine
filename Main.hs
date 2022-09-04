{-# LANGUAGE OverloadedStrings #-}

import Basic

import System.Console.ANSI

import Json.SaveParser
import Modes.Newsave.Newsave
import Modes.LoadSave.Json.Parser
import Modes.LoadSave.LoadSave

import Json.RelicParser (baseRelic)
import Json.SaveParser
import Modes.Json.Shared
import Modes.Newsave.Json.Parser
import Modes.Shared


main = do
    clearScreen
    putStr "\n\n"
    putStr "Hello!"
    putStr "\n\nThis program finds specific seeds that you can use in undermine."
    putStr "\nKeep in mind that there are not "
    setSGR [SetColor Foreground Vivid Red]
    putStr "nearly enough "
    setSGR [Reset]
    putStr "seeds for permutations exceeding 5 relics"
    putStr "\n\n"
    putStr "Type either \"newsave\" or \"100\" for the right save file"
    putStr "\n\n\n\n\n\n\n\n\n\n\n\n"
    newsave
    
newsave = newsaveMain