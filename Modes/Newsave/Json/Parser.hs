{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Modes.Newsave.Json.Parser where

import Basic

import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Maybe

import Modes.Json.Shared
import Json.RelicParser (baseRelic)

--STARTER RELICS

starter = do
  base <- baseRelic
  let starter = map (\ x -> [get| x.tables.relicStarter |]) base
  return $ [x | Just x <- starter]

starterWeight = (masterindex starterIndex . map (\ x -> [get| x.weight |])) =<< starter

starterIndex  = map (\ x -> [get| x.index |]) <$> starter

starterDisplay = do
  base <- baseRelic
  let display = map (\ x -> [get| x.display |]) base
  let condition = map (\ x -> isJust [get| x.tables.relicStarter |]) base

  masterindex starterIndex $ remove "" $ zipWith isTrue condition display


