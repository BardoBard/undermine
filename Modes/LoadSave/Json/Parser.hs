{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Modes.LoadSave.Json.Parser where

import Basic

import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Maybe
import Control.Monad
import Data.List

import Json.RelicParser
import Json.SaveParser
import Modes.Json.Shared
import Modes.Newsave.Json.Parser
import Modes.Shared

saveInput = do
    guids <- filter (/= "") . map (\ x -> case [get| x.tables.relic |] of
                                                Just y  -> [get| x.guid |]
                                                Nothing -> []             ) <$> baseRelic
    unlocked <- unlocked
    altarItemID <- altarItemID
    return $ catMaybes [elemIndex x guids | x <- filter (/=altarItemID) unlocked]

unlockedInput = do
    maxDisplay <- maxDisplay
    saveInput  <- saveInput
    return $ nub $ mapMaybe (`elemIndex` maxDisplay) newsaveDisplay ++ saveInput

inputWeight = do
    unlockedInput <- unlockedInput
    maxWeight <- maxWeight
    return $ placeAtIndex [a | a <- [0..(length maxWeight - 1)], a `notElem` unlockedInput] (replicate 120 0) maxWeight

inputDisplay = do
    inputWeight <- inputWeight
    maxDisplay <- maxDisplay
    return $ [b | (a,b) <- zip inputWeight maxDisplay, a /= 0]