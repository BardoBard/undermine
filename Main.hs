{-# LANGUAGE OverloadedStrings #-}

import Basic

import System.Console.ANSI ( clearScreen, setSGR, ColorIntensity( Vivid ), ConsoleLayer( Foreground ), SGR( SetColor, Reset ), Color ( Red ))

import Json.RelicParser (baseRelic)

import Modes.NewSave.NewSave ( newsaveMain )
import Modes.LoadSave.LoadSave ( loadfileMain )

main = do
    clearScreen
    putStr "\n\n"
    putStr "Hello!"
    putStr "\n\nThis program finds specific seeds that you can use in undermine."
    putStr "\nKeep in mind that there are not "
    setSGR [SetColor Foreground Vivid Red]
    putStr "nearly enough "
    setSGR [Reset]
    putStr "seeds for permutations exceeding 4 relics"
    putStr "\n\nThis issue will be fixed in the next update!!!!"
    putStr "\n\n\n"
    putStr "Type either \"newsave\" or \"loadfile\" for the right mode"
    putStr "\n\n\n\n\n\n\n\n\n\n\n\n"

    modeParsing "Error, wrong input."

goToModes mode
    | mode == "newsave"  = newsaveMain
    | mode == "loadfile" = loadfileMain

modes = ["newsave", "loadfile"]

modeParsing msg = do
    x <- getLine :: IO String
    if x `elem` modes
        then goToModes x
    else do
        print msg
        modeParsing msg



newsave = loadfileMain