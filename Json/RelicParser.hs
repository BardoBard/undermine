{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Json.RelicParser where

import Basic 

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema ( schema, get, Object)
import qualified Data.Text as T ( empty ) 

type Relics = [schema|
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
        relic: Maybe {
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

baseJson = do
  obj <- either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Main.json" :: IO (Object Relics)

  return [get| obj.relics[] |]

isTrue x y
  | x         = y
  | otherwise = T.empty

masterindex x y = do
  x' <- x
  return $ placeAtIndex x' y y