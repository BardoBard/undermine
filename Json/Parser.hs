{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema
import qualified Data.Text as T

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
        relic: Maybe {
            index: Int,
            weight: Int,
            },
      },
    },
  }
|]

main :: IO ()
main = do
  -- Then, load data from a file
  obj <- either fail return =<<
    eitherDecodeFileStrict "relics.json" :: IO (Object MySchema)

  flip mapM_[get| obj.relics[].tables |] $ \table -> do
    case [get| table.relic |] of
      Just relic -> print relic
      Nothing -> return ()