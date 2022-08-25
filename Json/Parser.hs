{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}

module Json.Parser where

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T
import Data.Typeable
import Data.Aeson.Schema.Key
import Data.Maybe
import Control.Monad
import Control.Lens
import qualified Data.Sequence as S

-- First, define the schema of the JSON data
type MySchema = [schema|
  {
    relics: List {
      guid: Text,
      name: Text,
      display: Text,
      rarity: Text,
      crafting: Int,
      cost: Int,
      extra: String,
      tables: {
        relic: {
            index: Int,
            weight: Int,
            },
      },
    },
  }
|]




index = do
    obj <- either fail return =<<
        eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

    let index  = [get| obj.relics[].tables.relic.index |]

    return index


weight = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

  let weight = [get| obj.relics[].tables.relic.weight |]
  return weight

weight' = do
  items' <- items
  return $ items' !! 90

masterindex mIndex = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Relics.json" :: IO (Object MySchema)

  let weight = [get| obj.relics[].display |]
  return $ weight !! mIndex