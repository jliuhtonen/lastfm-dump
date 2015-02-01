{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Conduit hiding (host)
import Data.Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Data.Time
import Data.Maybe
import Data.List (intercalate)
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack, putStrLn, writeFile)
import qualified LastFm
import Database.MongoDB
import qualified Data.Bson as Bson (Document)

apiKey = "cc5a08a82ef3d31fef33894f0fbd54cc"
apiCallDelay = 1000000 -- 1 sec in microseconds
pageSize = 200
baseUrl = "http://ws.audioscrobbler.com/2.0/"

mongoserver = "127.0.0.1"

removeQuotes :: String -> String
removeQuotes = filter $ not . ((==) '"')

toPlainString :: Show a => a -> String
toPlainString = removeQuotes . show

urlParam :: Show a => String -> a -> String
urlParam key value = key ++ "=" ++ (toPlainString value)

optUrlParam :: Show a => String -> Maybe a -> String
optUrlParam key mValue = maybe "" (urlParam key) mValue

buildParams :: String -> Maybe Int -> String
buildParams user page = "?" ++ intercalate "&" params  where
        params = filter (\x -> length x > 0) [methodP, userP, limitP, formatP, apiKeyP, pageP]
        methodP = urlParam "method" "user.getrecenttracks"
        userP = urlParam "user" user
        limitP = urlParam "limit" pageSize
        apiKeyP = urlParam "api_key" apiKey
        formatP = urlParam "format" "json"
        pageP = optUrlParam "page" page

fetchTracks :: Manager -> Maybe Int -> IO (Maybe LastFm.Response)
fetchTracks manager page = do
                      let pageParam' = optUrlParam "page" page
                      let url = baseUrl ++ buildParams "badg" page 
                      putStrLn $ "url" ++ url
                      request <- parseUrl url
                      response <- httpLbs request manager
                      let body = responseBody response
                      putStrLn (show body)
                      return $ decode body

handleResponse :: Maybe Int -> Pipe -> Manager -> Maybe LastFm.Response -> IO ()
handleResponse page mongoPipe manager (Just (LastFm.RecentTracksResponse r)) = do
        inMongo $ insertMany "scrobbles" tracks
        if page' < pages
        then recentTracks (Just (page' + 1)) mongoPipe manager
        else return () where
            tracks = fmap LastFm.toDocument $ filter (isJust . LastFm.scrobbledAt) (LastFm.track r)
            attrs = LastFm.attr r
            page' = LastFm.page attrs
            pages = LastFm.totalPages attrs
            inMongo = access mongoPipe master "scrobbles"
handleResponse page mongoPipe manager (Just (LastFm.Error _ _ _)) = recentTracks page mongoPipe manager
handleResponse page _ _ Nothing = return ()

recentTracks :: Maybe Int -> Pipe -> Manager -> IO ()
recentTracks page mongoPipe manager = do
        response <- fetchTracks manager page
        putStrLn (show response)
        threadDelay apiCallDelay
        handleResponse page mongoPipe manager response

main = do
  mongoPipe <- connect $ host mongoserver
  withManager $ \manager -> do
      liftIO $ recentTracks Nothing mongoPipe manager
  close mongoPipe
