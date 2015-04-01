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
import qualified Data.ByteString.Char8 as StrictC8 
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack, putStrLn, writeFile, toStrict)
import qualified LastFm
import Database.MongoDB
import qualified Data.Bson as Bson (Document)
import Control.Exception (catch)

apiKey = "cc5a08a82ef3d31fef33894f0fbd54cc"
apiCallDelay = 1000000 -- 1 sec in microseconds
pageSize = 200
url = "http://ws.audioscrobbler.com/2.0/"

mongoserver = "127.0.0.1"

toByteString :: Int -> StrictC8.ByteString
toByteString = StrictC8.pack . show

requestWithParams :: String -> Maybe Int -> Request -> Request
requestWithParams user page request = setQueryString params request where
	params = [("method", Just "user.getrecenttracks"),
		  ("user", Just (StrictC8.pack user)),
		  ("limit", Just (toByteString pageSize)),
		  ("api_key", Just apiKey),
		  ("format", Just "json"),
		  ("page", fmap toByteString page)]

fetchTracks :: Manager -> Maybe Int -> IO (Maybe LastFm.Response)
fetchTracks manager page = do
                      request <- fmap (requestWithParams "badg" page) $ parseUrl url
		      putStrLn $ show request
                      response <- httpLbs request manager
                      return $ decode $ responseBody response 

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
