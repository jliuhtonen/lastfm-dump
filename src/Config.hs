{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config where

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import GHC.Generics
import Data.Text
import Data.Aeson

data Config = Config {
    apiKey :: !Text,
    mongoServer :: !Text,
    mongoDatabase :: !Text,
    pageSize :: Int
} deriving (Show, Generic)

instance FromJSON Config

readConfig :: IO (Maybe Config) 
readConfig = fmap decode $ readFile "config.json"
