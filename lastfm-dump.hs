{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import qualified Data.ByteString as BString

parseInt :: AesonTypes.Object -> Text -> AesonTypes.Parser Int
parseInt v f = fmap (read :: String -> Int) $ v .: f

parseDateTime :: AesonTypes.Object -> Text -> AesonTypes.Parser UTCTime
parseDateTime v f = do
        date <- v .: f
        let toUTCTime = posixSecondsToUTCTime . fromIntegral
        let timestamp = date `parseInt` "uts"
        fmap toUTCTime timestamp

data RecentTracksResponse = RecentTracksResponse {
    recenttracks :: RecentTracks
} deriving (Show, Generic)

instance FromJSON RecentTracksResponse

data RecentTracks = RecentTracks {
    track :: [Track],                    
    attr :: Attributes    
} deriving (Show, Generic)

instance FromJSON RecentTracks where
        parseJSON (Object v) = RecentTracks <$>
                            v .: "track" <*>
                            v .: "@attr"
        parseJSON _ = mzero

data Track = Track {
    name :: !Text,
    artist :: NamedRef,
    album :: NamedRef,
    url :: !Text,
    scrobbledAt :: UTCTime
} deriving Show

instance FromJSON Track where
        parseJSON (Object v) = Track <$>
                            v .: "name" <*>
                            v .: "artist" <*>
                            v .: "album" <*>
                            v .: "url" <*>
                            v `parseDateTime` "date"
        parseJSON _ = mzero

data NamedRef = NamedRef {
    refName :: !Text,
    mbid :: !Text
} deriving Show

instance FromJSON NamedRef where
        parseJSON (Object v) = NamedRef <$>
                            v .: "#text" <*>
                            v .: "mbid"

data Attributes = Attributes {
    user :: !Text,
    page :: Int,
    perPage :: Int,
    totalPages :: Int,
    total :: Int    
} deriving Show

instance FromJSON Attributes where
        parseJSON (Object v) = Attributes <$>
                            v .: "user" <*>
                            v `parseInt` "page" <*>
                            v `parseInt` "perPage" <*>
                            v `parseInt` "totalPages" <*>
                            v `parseInt` "total"

fetchTracks :: String -> IO (Maybe RecentTracksResponse)
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
