{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Modes.Json.Shared where

import Basic

import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Maybe

import Json.RelicParser (baseRelic)
import Json.SaveParser

isTrue x y
  | x         = y
  | otherwise = []

masterindex x y = do
  x' <- x
  return $ placeAtIndex x' y y


--NEW SAVE RELICS

--I have hand picked the new save items due to the fact that the json doesn't have a field for new save items (obviously)


newsaveDisplay :: [String]
newsaveDisplay = ["Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Demon Ring","Intensifier","Cracked Orb","Conductor","Grimhilde's Mirror","Meal Ticket","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Butcher's Cleaver","Iron Branch","Knight's Pendant","Queen's Crown","Aegis","Adventurer's Whip","Axe Thrower's Pendant","Cosmic Egg","Battle Standard","Battle Axe","Tent","Masa","Lunchbox","Phantasmal Axe","Gecko Blast","Soul Cannon","Greaves","Pauldron","Obsidian Knife","Fork","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Shrapnel","Tortoise Shield","Golden Axe"]

newsaveWeight :: [Int]
newsaveWeight = [9,9,9,9,9,9,9,9,9,9,9,3,9,9,9,9,9,9,9,9,9,9,9,9,3,3,9,9,9,9,9,9,9,9,3,9,3,9,3,9,9,9,3,9,9,1,3,5,5,3,3,9,9,9,9,9,9,9,3]

--MAX SAVE RELICS

relics = do
  base <- baseRelic
  let relics = map (\ x -> [get| x.tables.relic |]) base
  return $ [x | Just x <- relics]

maxWeight = (masterindex maxIndex . map (\ x -> [get| x.weight |])) =<< relics

maxIndex  = map (\ x -> [get| x.index |]) <$> relics

display = map (\ x -> [get| x.display |]) <$> baseRelic
condition = map (\ x -> isJust [get| x.tables.relic |]) <$> baseRelic

--["Bombushka","Seer's Blood","Rook's Bomb","Lightning Bomb","Galoshes","Bottled Lightning","Salamander Tail","Guidance","Ursine Ring","Demon Ring","Intensifier","Cracked Orb","Conductor","Grimhilde's Mirror","Meal Ticket","Dillon's Claw","Bramble Vest","Leftovers","Spare Ordnance","Simple Chest","Unstable Concoction","Totem of Life","Golden Popcorn","Miner's Flask","Sewing Kit","Floating Skull","Float Boots","Key Blade","War Paint","Sonic Boom","Gold Frenzy","Butcher's Cleaver","Iron Branch","Knight's Pendant","Queen's Crown","Aegis","Adventurer's Whip","Axe Thrower's Pendant","Cosmic Egg","Battle Standard","Battle Axe","Tent","Masa","Lunchbox","Phantasmal Axe","Gecko Blast","Soul Cannon","Greaves","Pauldron","Obsidian Knife","Fork","Ursa Major","Canis Major","Sagitta","Circinus","Orion's Sword","Shrapnel","Tortoise Shield","Golden Axe"]

maxDisplay = do
  base <- baseRelic
  let display = map (\ x -> [get| x.display |]) base
  let condition = map (\ x -> isJust [get| x.tables.relic |]) base

  masterindex maxIndex $ remove "" $ zipWith isTrue condition display
