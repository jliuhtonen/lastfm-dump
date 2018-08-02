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

apiCallDelay :: Int
apiCallDelay = 1000000 `div` 5 -- 1 / 5 sec in microseconds
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
        let databaseName = dbName cfg
        liftIO $ access mongoPipe master databaseName action

toByteString :: Int -> StrictC8.ByteString
toByteString = StrictC8.pack . show

logPagingStatus page pages = putStrLn $ "Fetched page " ++ show page ++ " / " ++ show pages

logError page code msg =
        putStrLn $ "Error fetching page " ++ show page ++ "\n" ++
        "Error code " ++ show code ++ "\n" ++
        "Message " ++ unpack msg

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
        (CrawlerEnv lastCrawled lastFmUser manager _ (Config key _ _ _ _ _ items _)) <- ask
        let scrobblesSince = fmap (+ 1) lastCrawled
        request <- fmap (requestWithParams key items lastFmUser page scrobblesSince) $ parseUrl url
        response <- httpLbs request manager
        return $ decode $ responseBody response

handleError :: Int -> Int -> Text -> Crawler ()
handleError page code msg = errorOutput >> recentTracks page where
           errorOutput = lift $ logError page code msg >>
                putStrLn "Retrying..."

persist :: [LastFm.Track] -> Crawler [Database.MongoDB.Value]
persist tracks = do
        (CrawlerEnv _ _ _ _ cfg) <- ask
        let databaseName = dbName cfg
	let insertAction = insertMany databaseName $ fmap LastFm.toDocument tracks
        runMongoAction insertAction

handleResponse :: LastFm.RecentTracks -> Crawler ()
handleResponse tracks = do
        persist $ LastFm.timestampedScrobbles tracks
        let (page, pages) = LastFm.paging tracks
        lift $ logPagingStatus page pages
        if page > 1
        then recentTracks $ page - 1
        else return ()

recentTracks :: Int -> Crawler ()
recentTracks 0 = return ()
recentTracks page = do
        response <- fetchTracks page
        lift $ threadDelay apiCallDelay
        case response of
            Nothing -> return ()
            Just (LastFm.Error code msg _) -> handleError page code msg
            Just (LastFm.RecentTracksResponse tracks) -> handleResponse tracks

latestScrobbleTimestamp :: Pipe -> Text -> IO (Maybe Int)
latestScrobbleTimestamp mongoPipe databaseName = do
	let run = access mongoPipe master databaseName
	let latestScrobbleAction = findOne (select [] databaseName) {sort = ["scrobbledAt" =: (-1 :: Int)]}
	latestScrobbleDocument <- run latestScrobbleAction
	let latestScrobbleTime = fmap (at "scrobbledAt") latestScrobbleDocument :: Maybe UTCTime
	return $ fmap (round . utcTimeToPOSIXSeconds) latestScrobbleTime

numberOfPages :: Crawler Int
numberOfPages = fetchTracks 1 >>= \response -> case response of
	Just (LastFm.RecentTracksResponse tracks) -> return $ (snd . LastFm.paging) tracks
	_ -> return 0

crawl = numberOfPages >>= recentTracks

main = do
  config <- readConfig
  case config of
    Nothing -> putStrLn "Malformed config"
    Just cfg -> do
      let user = Config.lastFmUser cfg
      let mongoHost = unpack $ mongoServer cfg
      let mongoPort = Config.port cfg
      mongoPipe <- connect $ Host mongoHost mongoPort
      let db = dbName cfg
      access mongoPipe master db $ auth (Config.user cfg) (password cfg)
      lastCrawled <- latestScrobbleTimestamp mongoPipe db
      manager <- newManager tlsManagerSettings
      let env = CrawlerEnv lastCrawled user manager mongoPipe cfg
      liftIO $ runReaderT crawl env
      close mongoPipe