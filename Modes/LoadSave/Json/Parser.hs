{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Modes.LoadSave.Json.Parser ( inputWeight, inputDisplay, inputUnlocked ) where

--Other Libs

import Data.Aeson.Schema ( get )
import Data.List (elemIndex, sort, nub, isInfixOf)
import Data.Maybe ( catMaybes )
import Data.Char (digitToInt)
import Control.Monad.Reader ( liftM2 )

--My Libs

import Basic ( splitOnAnyOf )
import Json.RelicParser ( baseRelic )
import Json.SaveParser ( altarItemID, upgradeString, unlocked )
import Modes.Json.Shared ( maxWeight, maxDisplay, newsaveDisplay )

altarItemIndex = do
  altarItemID <- altarItemID
  result <- map
    ( \x -> case [get| x.tables.relic |] of
        Just y ->
          if [get| x.guid |] == altarItemID
            then [get| y.index |]
            else 0
        Nothing -> 0
    ) <$> baseRelic
  return $ last $ -1 : filter (/= 0) result

hasMaxGoldIntegrity = do
  upgradeString <- upgradeString
  return $ any (`isInfixOf` upgradeString) ["gold_keep_percent:95", "goldsack_upgrade:9"]

hasMaxGoldIndex     = do
  hasMaxGoldIntegrity <- hasMaxGoldIntegrity
  result <- elemIndex (if hasMaxGoldIntegrity then "Sewing Kit" else "") <$> maxDisplay
  maybe (return (-1)) return result

hasKurtzStashe = do
  list <- splitOnAnyOf [",", ":"] <$> upgradeString
  let a = case elemIndex "delve_count" list of
                Just x  -> read $ list !! (x + 1) :: Int
                Nothing -> -1
  elemIndex (if a >= 4 then "Kurtz' Stache" else "") <$> maxDisplay
  

relicGuids = do
  unlocked <- unlocked
  map
    ( \x -> case [get| x.tables.relic |] of
        Just y -> case elemIndex [get| x.guid |] unlocked of
          Just z -> Just [get| x.display |]
          Nothing -> Nothing
        Nothing -> Nothing
    ) <$> baseRelic

inputUnlocked = do
  maxDisplay      <- maxDisplay
  relicGuids      <- relicGuids
  altarItemIndex  <- altarItemIndex
  hasMaxGoldIndex <- hasMaxGoldIndex
  hasKurtzStashe  <- hasKurtzStashe
  return $ filter (liftM2 (&&) (/= altarItemIndex) (/= hasMaxGoldIndex)) $ sort $ nub $ catMaybes $ hasKurtzStashe : map (`elemIndex` maxDisplay) (newsaveDisplay ++ catMaybes relicGuids)

inputWeight = do
  maxWeight <- maxWeight
  map (maxWeight !!) <$> inputUnlocked --return $ placeAtIndex [a | a <- [0..(length maxWeight - 1)], a `notElem` inputUnlocked] (replicate 120 0) maxWeight

inputDisplay = do
  maxDisplay <- maxDisplay
  map (maxDisplay !!) <$> inputUnlocked
