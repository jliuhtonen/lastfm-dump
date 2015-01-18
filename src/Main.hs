{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics
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

fetchTracks :: String -> IO (Maybe LastFm.RecentTracksResponse)
fetchTracks apiKey = fmap decode $ simpleHttp $ "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=badg&format=json&api_key=" ++ apiKey

responseAsScrobbles :: LastFm.RecentTracksResponse -> [LastFm.Track]
responseAsScrobbles = LastFm.track . LastFm.recenttracks

recentTracks :: String -> IO ()
recentTracks apiKey = do
        resp <- fetchTracks apiKey
        case (fmap responseAsScrobbles resp) of
            Just r -> C8.putStrLn (encode r)
            Nothing -> Prelude.putStrLn "empty!"

main = do
  apiKey <- fmap (Text.unpack . Text.strip . Text.pack) (readFile "apiKey")
  putStrLn apiKey
  recentTracks apiKey
