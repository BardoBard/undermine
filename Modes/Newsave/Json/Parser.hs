{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Modes.Newsave.Json.Parser where

import Basic

import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Maybe

import Json.RelicParser (baseJson, masterindex, isTrue)

--STARTER RELICS

starter = do
  base <- baseJson
  let starter = map (\ x -> [get| x.tables.relicStarter |]) base
  return $ [x | Just x <- starter]

starterWeight = (masterindex starterIndex . map (\ x -> [get| x.weight |])) =<< starter
starterIndex  = map (\ x -> [get| x.index |]) <$> starter

starterDisplay = do
  base <- baseJson
  let display = map (\ x -> [get| x.display |]) base
  let condition = map (\ x -> isJust [get| x.tables.relicStarter |]) base

  result <- masterindex starterIndex $ remove T.empty $ zipWith isTrue condition display
  return $ map T.unpack result 


--NEW SAVE RELICS

--I have hand picked the new save items due to the fact that the json doesn't have a field for new save items (obviously)


newsaveDisplay :: [String]
newsaveDisplay = ["Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Demon Ring","Intensifier","Cracked Orb","Conductor","Grimhilde's Mirror","Meal Ticket","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Butcher's Cleaver","Iron Branch","Knight's Pendant","Queen's Crown","Aegis","Adventurer's Whip","Axe Thrower's Pendant","Cosmic Egg","Battle Standard","Battle Axe","Tent","Masa","Lunchbox","Phantasmal Axe","Gecko Blast","Soul Cannon","Greaves","Pauldron","Obsidian Knife","Fork","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Shrapnel","Tortoise Shield","Golden Axe"]

newsaveWeight :: [Int]
newsaveWeight = [9,9,9,9,9,9,9,9,9,9,9,3,9,9,9,9,9,9,9,9,9,9,9,9,3,3,9,9,9,9,9,9,9,9,3,9,3,9,3,9,9,9,3,9,9,1,3,5,5,3,3,9,9,9,9,9,9,9,3]


