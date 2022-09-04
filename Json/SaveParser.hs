{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Json.SaveParser where


import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Schema ( schema, get, Object)
import qualified Data.Text as T ( empty )
import Data.List ( elemIndex )


type SaveFile = [schema|
  {
    altarItemID: String,
    unlocked: List String,
    upgradeString: String,
  }
|]

baseSave = do
  either fail return =<<
    eitherDecodeFileStrict "JsonFiles/Save0.json" :: IO (Object SaveFile)

unlocked = do
    base <- baseSave
    return [get| base.unlocked |]

altarItemID = do
    base <- baseSave
    return [get| base.altarItemID |]

upgradeString = do
    base <- baseSave
    return [get| base.upgradeString |]


