{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
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

fetchTracks :: Config -> Manager -> Maybe Int -> IO (Maybe LastFm.Response)
fetchTracks config manager page = do
        request <- fmap (requestWithParams key items "badg" page) $ parseUrl url
        response <- httpLbs request manager
        return $ decode $ responseBody response where
            key = apiKey config
            items = pageSize config

logPagingStatus :: Int -> Int -> IO ()
logPagingStatus page pages = putStrLn $ "Fetched page " ++ show page ++ " / " ++ show pages

logError :: Maybe Int -> Int -> Text -> [Text] -> IO ()
logError page code msg links = 
        putStrLn $ "Error fetching page " ++ (maybe "0" show page) ++ "\n" ++
        "Error code " ++ show code ++ "\n" ++
        "Message: " ++ unpack msg

recentTracks :: Config -> Maybe Int -> Pipe -> Manager -> IO ()
recentTracks config page mongoPipe manager = do
        response <- fetchTracks config manager page
        threadDelay apiCallDelay
        case response of
            Nothing -> return ()
            Just (LastFm.Error code msg links) -> do 
                logError page code msg links
                putStrLn "Retrying..."
                recentTracks config page mongoPipe manager
            Just (LastFm.RecentTracksResponse r) -> do
                logPagingStatus page' pages
                inMongo $ insertMany databaseName tracks
                if page' < pages
                then recentTracks config (Just (page' + 1)) mongoPipe manager
                else return () where
                    tracks = fmap LastFm.toDocument $ LastFm.timestampedScrobbles r
                    paging = LastFm.paging r
                    page' = fst paging
                    pages = snd paging
                    databaseName = mongoDatabase config
                    inMongo = access mongoPipe master databaseName

main = do
    config <- readConfig
    case config of
        Nothing -> putStrLn "Malformed config.json"
        Just cfg@(Config _ _ _ _) -> do
            mongoPipe <- connect $ host $ unpack $ mongoServer cfg
            withManager $ \manager -> do
                liftIO $ recentTracks cfg Nothing mongoPipe manager
            close mongoPipe
