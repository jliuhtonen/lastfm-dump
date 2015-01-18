{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import qualified Data.ByteString as BString
import qualified LastFm

fetchTracks :: String -> IO (Maybe LastFm.RecentTracksResponse)
fetchTracks apiKey = fmap decode $ simpleHttp $ "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=badg&format=json&api_key=" ++ apiKey

recentTracks :: String -> IO ()
recentTracks apiKey = do
        resp <- fetchTracks apiKey
        case resp of
            Just r -> Prelude.putStrLn (show r)
            Nothing -> Prelude.putStrLn "empty!"

main = do
  apiKey <- fmap (unpack . strip . pack) (readFile "apiKey")
  putStrLn apiKey
  recentTracks apiKey
