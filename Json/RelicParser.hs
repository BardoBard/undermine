{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Json.RelicParser ( baseRelic ) where

import Basic

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema ( schema, get, Object)
import qualified Data.Text as T ( empty )

type Relics = [schema|
  {
    relics: List {
      guid: String,
      name: Text,
      display: String,
      rarity: Text,
      crafting: Int,
      cost: Int,
      extra: String,
      tables: {
        relic: Maybe {
            index: Int,
            weight: Int,
            },
        relicAll: Maybe {
            index: Int,
            weight: Int,
            },
        relicStarter: Maybe {
            index: Int,
            weight: Int,
            },
      },
    },
  }
|]

objRelic = do
  either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Main.json" :: IO (Object Relics)
    
baseRelic = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Main.json" :: IO (Object Relics)

  return [get| obj.relics[] |]