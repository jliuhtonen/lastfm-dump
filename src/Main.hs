{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Text as Text
import Data.Time
import Data.Maybe
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack, putStrLn)
import qualified LastFm

apiKey = "cc5a08a82ef3d31fef33894f0fbd54cc"
apiCallDelay = 1000000 -- 1 sec in microseconds
pageSize = 200

fetchTracks :: Maybe Int -> IO (Maybe LastFm.RecentTracksResponse)
fetchTracks page = fmap decode $ simpleHttp $ "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=badg&format=json&api_key=" ++ apiKey ++ pageParam where
    pageParam = case page of
              Just p -> "&page=" ++ (show p)
              Nothing -> ""

scrobbles :: LastFm.RecentTracksResponse -> [LastFm.Track]
scrobbles = LastFm.track . LastFm.recenttracks

attributes :: LastFm.RecentTracksResponse -> LastFm.Attributes
attributes = LastFm.attr . LastFm.recenttracks

handleResponse :: Maybe LastFm.RecentTracksResponse -> [LastFm.Track] -> IO [LastFm.Track]
handleResponse (Just r) collected = 
        if page < pages
        then recentTracks (Just (page + 1)) allTracks
        else return allTracks where
            allTracks = scrobbles r ++ collected
            attrs = attributes r
            page = LastFm.page attrs
            pages = LastFm.totalPages attrs
handleResponse _ collected = return collected

recentTracks :: Maybe Int -> [LastFm.Track] -> IO ([LastFm.Track])
recentTracks page collected = do
        response <- fetchTracks page
        putStrLn (show response)
        threadDelay apiCallDelay
        case response of
            Just r -> let attrs = attributes r
                          page = LastFm.page attrs
                          pages = LastFm.totalPages attrs
                      in if page < pages 
                         then recentTracks (Just (page + 1)) (collected ++ (scrobbles r))
                         else return collected
            _ -> return collected

main = do
  tracks <- recentTracks Nothing []
  C8.putStrLn $ encode $ tracks
