{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LastFm where

import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Aeson.Types as AesonTypes
    
parseInt :: AesonTypes.Object -> Text -> AesonTypes.Parser Int
parseInt v f = fmap (read :: String -> Int) $ v .: f

parseDateTime :: AesonTypes.Object -> Text -> AesonTypes.Parser UTCTime
parseDateTime v f = do
        date <- v .: f
        let toUTCTime = posixSecondsToUTCTime . fromIntegral
        let timestamp = date `parseInt` "uts"
        fmap toUTCTime timestamp

data Response = RecentTracksResponse {
    recenttracks :: RecentTracks              
} | Error {
    error :: Int,
    message :: !Text,
    links :: [Text]
} deriving Show

instance FromJSON Response where
        parseJSON json
            | isJust (json ^? key "recenttracks") = 
                case json of 
                             Object v -> RecentTracksResponse <$> v .: "recenttracks"
                             _ -> mzero
            | isJust (json ^? key "error") = 
                case json of
                    (Object v) -> LastFm.Error <$> v .: "error" <*> v .: "message" <*> v .: "links"
                    _ -> mzero
            | otherwise = mzero

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

instance ToJSON Track where
        toJSON (Track name artist album url scrobbledAt) =
            object ["name" .= name, "artist" .= artist, "album" .= album, 
                    "url" .= url, "scrobbledAt" .= scrobbledAt]

data NamedRef = NamedRef {
    refName :: !Text,
    mbid :: !Text
} deriving Show

instance FromJSON NamedRef where
        parseJSON (Object v) = NamedRef <$>
                            v .: "#text" <*>
                            v .: "mbid"

instance ToJSON NamedRef where
        toJSON (NamedRef refName mbid) = object ["name" .= refName, "mbid" .= mbid]

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
