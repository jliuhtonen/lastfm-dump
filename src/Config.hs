{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Config where

import Prelude hiding (readFile)
import GHC.Generics
import Network (PortID(..))
import Network.Socket (PortNumber)
import Configuration.Dotenv as Dotenv (loadFile)
import Configuration.Dotenv.Types (defaultConfig)
import System.Environment
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

readConfig :: IO Config
readConfig = do
  Dotenv.loadFile defaultConfig
  apiKey <- fmap pack $ getEnv "LASTFM_API_KEY"
  mongoServer <- fmap pack $ getEnv "MONGODB_ADDRESS"
  mongoPort <- fmap read $ getEnv "MONGODB_PORT"
  let portid = PortNumber mongoPort
  mongoUser <- fmap pack $ getEnv "MONGODB_USER"
  mongoPassword <- fmap pack $ getEnv "MONGODB_PASSWORD"
  dbName <- fmap pack $ getEnv "MONGODB_DBNAME"
  pageSize <- fmap read $ getEnv "PAGE_SIZE"
  lastFmUser <- getEnv "LASTFM_USER"
  return $ Config { apiKey=apiKey,
      mongoServer=mongoServer,
      port=portid,
      dbName=dbName,
      user=mongoUser,
      password=mongoPassword,
      pageSize=pageSize,
      lastFmUser=lastFmUser }