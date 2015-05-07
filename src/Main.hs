{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
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
    lastCrawlTimestamp :: Maybe Int,
    lastFmUser :: String,
    httpManager :: Manager,
    mongoPipe :: Pipe,
    runConfig :: Config
}

type Crawler = ReaderT CrawlerEnv IO

runMongoAction :: Action IO a -> Crawler a
runMongoAction action = do
        (CrawlerEnv _ _ _ mongoPipe cfg) <- ask
        let databaseName = mongoDatabase cfg
        liftIO $ access mongoPipe master databaseName action

toByteString :: Int -> StrictC8.ByteString
toByteString = StrictC8.pack . show

logPagingStatus page pages = putStrLn $ "Fetched page " ++ show page ++ " / " ++ show pages

logError page code msg = 
        putStrLn $ "Error fetching page " ++ show page ++ "\n" ++
        "Error code " ++ show code ++ "\n" ++
        "Message: " ++ unpack msg

requestWithParams :: Text -> Int -> String -> Int -> Maybe Int -> Request -> Request
requestWithParams key items user page from request = setQueryString params request where
    params = [("method", Just "user.getrecenttracks"),
             ("user", Just (StrictC8.pack user)),
             ("limit", Just (toByteString items)),
             ("api_key", Just (encodeUtf8 key)),
             ("format", Just "json"),
             ("page", Just (toByteString page)),
	     ("from", fmap toByteString from)]

fetchTracks :: Int -> Crawler (Maybe LastFm.Response)
fetchTracks page = do
        (CrawlerEnv lastCrawled lastFmUser manager _ (Config key _ _ items)) <- ask
        request <- fmap (requestWithParams key items lastFmUser page lastCrawled) $ parseUrl url
        response <- httpLbs request manager
        return $ decode $ responseBody response

handleError :: Int -> Int -> Text -> Crawler ()
handleError page code msg = errorOutput >> recentTracks page where
           errorOutput = lift $ logError page code msg >> 
                putStrLn "Retrying..." 

persist :: [LastFm.Track] -> Crawler [Database.MongoDB.Value]
persist tracks = do
        (CrawlerEnv _ _ _ _ cfg) <- ask
        let databaseName = mongoDatabase cfg
	let insertAction = insertMany databaseName $ fmap LastFm.toDocument tracks
        runMongoAction insertAction

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

latestScrobbleTimestamp :: Pipe -> Text -> IO (Maybe Int)
latestScrobbleTimestamp mongoPipe databaseName = do
	let run = access mongoPipe master databaseName
	let latestScrobbleAction = findOne (select [] "scrobbles") {sort = ["scrobbledAt" =: (-1 :: Int)]}
	latestScrobbleDocument <- run latestScrobbleAction
	let latestScrobbleTime = fmap (at "scrobbledAt") latestScrobbleDocument :: Maybe UTCTime 
	return $ fmap (round . utcTimeToPOSIXSeconds) latestScrobbleTime 

crawl = recentTracks 0 

initCrawler [user] = do
    config <- readConfig
    case config of
        Nothing -> putStrLn "Malformed config.json"
        Just cfg -> do
            mongoPipe <- (connect . host . unpack . mongoServer) cfg
	    let db = mongoDatabase cfg
	    lastCrawled <- latestScrobbleTimestamp mongoPipe db
            withManager $ \manager -> do
                let env = CrawlerEnv lastCrawled user manager mongoPipe cfg
                liftIO $ runReaderT crawl env 
            close mongoPipe

initCrawler [] = usage
initCrawler (_:_) = usage 

main = getArgs >>= initCrawler
