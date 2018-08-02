{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config where

import Prelude hiding (readFile)
import GHC.Generics
import Network (PortID(..))
import Network.Socket (PortNumber)
import Configuration.Dotenv (loadFile)
import Configuration.Dotenv.Types (defaultConfig)
import Data.Text

data Config = Config {
    apiKey :: !Text,
    mongoServer :: !Text,
    dbName :: !Text,
    user :: !Text,
    password :: !Text,
    port :: PortID,
    pageSize :: Int,
    lastFmUser :: String
} deriving (Show, Generic)

readConfig :: IO (Maybe Config) 
readConfig = do
    env <- loadFile defaultConfig
    return $ asConfig env

asConfig :: [(String, String)] -> Maybe Config
asConfig envMap = do
  apiKey <- fmap pack $ lookup "LASTFM_API_KEY" envMap
  mongoServer <- fmap pack $ lookup "MONGODB_ADDRESS" envMap
  mongoPort <- fmap read $ lookup "MONGODB_PORT" envMap
  let portid = PortNumber mongoPort
  mongoUser <- fmap pack $ lookup "MONGODB_USER" envMap
  mongoPassword <- fmap pack $ lookup "MONGODB_PASSWORD" envMap
  dbName <- fmap pack $ lookup "MONGODB_DBNAME" envMap
  pageSize <- fmap read $ lookup "PAGE_SIZE" envMap
  lastFmUser <- lookup "LASTFM_USER" envMap
  Just Config { apiKey=apiKey,
      mongoServer=mongoServer,
      port=portid,
      dbName=dbName,
      user=mongoUser,
      password=mongoPassword,
      pageSize=pageSize,
      lastFmUser=lastFmUser }