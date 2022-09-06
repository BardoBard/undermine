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

relicGuids = filter (/= "") . map (\ x -> case [get| x.tables.relic |] of
                                           Just y  -> [get| x.guid |]
                                           Nothing -> []             ) <$> baseRelic


saveInput = do
    relicGuids <- relicGuids
    unlocked <- unlocked
    return $ catMaybes [elemIndex x relicGuids | x <- unlocked] --check altaritemid later


inputUnlocked = do
    maxDisplay <- maxDisplay
    saveInput  <- saveInput
    altarItemID <- altarItemID
    relicGuids <- relicGuids
    return $ nub $ mapMaybe (`elemIndex` maxDisplay) newsaveDisplay ++ saveInput -- ++ elemIndex (filter (==altarItemID) relicGuids) relicGuids

inputWeight = do
    maxWeight <- maxWeight
    map (maxWeight !!) <$> inputUnlocked --return $ placeAtIndex [a | a <- [0..(length maxWeight - 1)], a `notElem` inputUnlocked] (replicate 120 0) maxWeight

inputDisplay = do
    maxDisplay <- maxDisplay
    map (maxDisplay !!) <$> inputUnlocked