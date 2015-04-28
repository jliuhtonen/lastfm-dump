{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Database.MongoDB
import Network.HTTP.Conduit hiding (host)
import System.Environment (getArgs)
import qualified Data.Bson as Bson (Document)
import qualified Data.ByteString.Char8 as StrictC8
import qualified LastFm
import Config

apiCallDelay = 1000000 -- 1 sec in microseconds
url = "http://ws.audioscrobbler.com/2.0/"

data CrawlerEnv = CrawlerEnv {
    lastFmUser :: String,
    httpManager :: Manager,
    mongoPipe :: Pipe,
    runConfig :: Config
}

type Crawler = ReaderT CrawlerEnv IO

toByteString :: Int -> StrictC8.ByteString
toByteString = StrictC8.pack . show

logPagingStatus page pages = putStrLn $ "Fetched page " ++ show page ++ " / " ++ show pages

logError page code msg = 
        putStrLn $ "Error fetching page " ++ show page ++ "\n" ++
        "Error code " ++ show code ++ "\n" ++
        "Message: " ++ unpack msg

requestWithParams :: Text -> Int -> String -> Int -> Request -> Request
requestWithParams key items user page request = setQueryString params request where
    params = [("method", Just "user.getrecenttracks"),
             ("user", Just (StrictC8.pack user)),
             ("limit", Just (toByteString items)),
             ("api_key", Just (encodeUtf8 key)),
             ("format", Just "json"),
             ("page", Just (toByteString page))]

fetchTracks :: Int -> Crawler (Maybe LastFm.Response)
fetchTracks page = do
        (CrawlerEnv lastFmUser manager _ (Config key _ _ items)) <- ask
        request <- fmap (requestWithParams key items lastFmUser page) $ parseUrl url
        response <- httpLbs request manager
        return $ decode $ responseBody response

handleError :: Int -> Int -> Text -> Crawler ()
handleError page code msg = errorOutput >> recentTracks page where
           errorOutput = lift $ logError page code msg >> 
                putStrLn "Retrying..." 

persist :: [LastFm.Track] -> Crawler ()
persist tracks = do
        (CrawlerEnv _ _ mongoPipe cfg) <- ask
        let databaseName = mongoDatabase cfg
        let inMongo = access mongoPipe master databaseName
        lift $ inMongo $ insertMany databaseName $ fmap LastFm.toDocument tracks
        return ()

handleResponse :: LastFm.RecentTracks -> Crawler ()
handleResponse tracks = do
        persist $ LastFm.timestampedScrobbles tracks
        let (page, pages) = LastFm.paging tracks
        lift $ logPagingStatus page pages
        if page < pages
        then recentTracks $ page + 1 
        else return ()

recentTracks :: Int -> Crawler ()
recentTracks page = do
        response <- fetchTracks page
        lift $ threadDelay apiCallDelay
        case response of
            Nothing -> return ()
            Just (LastFm.Error code msg _) -> handleError page code msg 
            Just (LastFm.RecentTracksResponse tracks) -> handleResponse tracks

usage = putStrLn "Usage: lastfm-dump username"

runCrawler [user] = do
    config <- readConfig
    case config of
        Nothing -> putStrLn "Malformed config.json"
        Just cfg -> do
            mongoPipe <- (connect . host . unpack . mongoServer) cfg
            withManager $ \manager -> do
                let env = CrawlerEnv user manager mongoPipe cfg
                liftIO $ runReaderT (recentTracks 0) env 
            close mongoPipe

runCrawler [] = usage
runCrawler (_:_) = usage 

main = getArgs >>= runCrawler
