{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
import qualified Data.Bson as Bson (Document)
import qualified Data.ByteString.Char8 as StrictC8
import qualified LastFm
import Config

apiCallDelay = 1000000 -- 1 sec in microseconds
url = "http://ws.audioscrobbler.com/2.0/"

data CrawlerEnv = CrawlerEnv {
    httpManager :: Manager,
    mongoPipe :: Pipe,
    runConfig :: Config
}

type Crawler = ReaderT CrawlerEnv IO

toByteString :: Int -> StrictC8.ByteString
toByteString = StrictC8.pack . show

requestWithParams :: Text -> Int -> String -> Maybe Int -> Request -> Request
requestWithParams key items user page request = setQueryString params request where
    params = [("method", Just "user.getrecenttracks"),
             ("user", Just (StrictC8.pack user)),
             ("limit", Just (toByteString items)),
             ("api_key", Just (encodeUtf8 key)),
             ("format", Just "json"),
             ("page", fmap toByteString page)]

fetchTracks :: Maybe Int -> Crawler (Maybe LastFm.Response)
fetchTracks page = do
        (CrawlerEnv manager _ (Config key _ _ items)) <- ask
        request <- fmap (requestWithParams key items "badg" page) $ parseUrl url
        response <- httpLbs request manager
        return $ decode $ responseBody response

logPagingStatus :: Int -> Int -> IO ()
logPagingStatus page pages = putStrLn $ "Fetched page " ++ show page ++ " / " ++ show pages

logError page code msg = 
        putStrLn $ "Error fetching page " ++ (maybe "0" show page) ++ "\n" ++
        "Error code " ++ show code ++ "\n" ++
        "Message: " ++ unpack msg

handleError :: Maybe Int -> Int -> Text -> Crawler ()
handleError page code msg = errorOutput >> recentTracks page where
           errorOutput = lift $ logError page code msg >> 
                putStrLn "Retrying..." 

handleResponse :: LastFm.RecentTracks -> Crawler ()
handleResponse tracks = do
        (CrawlerEnv _ mongoPipe cfg) <- ask
        let databaseName = mongoDatabase cfg
        let tracks' = fmap LastFm.toDocument $ LastFm.timestampedScrobbles tracks
        let (page', pages) = LastFm.paging tracks
        let inMongo = access mongoPipe master databaseName
        lift $ logPagingStatus page' pages
        lift $ inMongo $ insertMany databaseName tracks'
        if page' < pages
        then recentTracks (Just (page' + 1)) 
        else return ()

recentTracks :: Maybe Int -> Crawler ()
recentTracks page = do
        response <- fetchTracks page
        lift $ threadDelay apiCallDelay
        case response of
            Nothing -> return ()
            Just (LastFm.Error code msg _) -> handleError page code msg 
            Just (LastFm.RecentTracksResponse tracks) -> handleResponse tracks

main = do
    config <- readConfig
    case config of
        Nothing -> putStrLn "Malformed config.json"
        Just cfg -> do
            mongoPipe <- connect $ host $ unpack $ mongoServer cfg
            withManager $ \manager -> do
                liftIO $ runReaderT (recentTracks Nothing) $ CrawlerEnv manager mongoPipe cfg
            close mongoPipe
