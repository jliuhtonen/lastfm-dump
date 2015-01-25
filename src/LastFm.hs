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
import Database.MongoDB
    
parseInt :: AesonTypes.Object -> Text -> AesonTypes.Parser Int
parseInt v f = fmap (read :: String -> Int) $ v .: f

parseDateTime :: AesonTypes.Object -> Text -> AesonTypes.Parser (Maybe UTCTime)
parseDateTime v f = do
        date <- v .:? f
        let toUTCTime = posixSecondsToUTCTime . fromIntegral
        case date of
            Just (Object o) -> fmap (Just . toUTCTime) (o `parseInt` "uts")
            Nothing -> return Nothing

class ToDocument a where
        toDocument :: a -> Document

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
    scrobbledAt :: Maybe UTCTime
} deriving Show

instance FromJSON Track where
        parseJSON (Object v) = Track <$>
                            v .: "name" <*>
                            v .: "artist" <*>
                            v .: "album" <*>
                            v .: "url" <*>
                            v `parseDateTime` "date"
        parseJSON _ = mzero

instance ToDocument Track where
        toDocument (Track name artist album url scrobbledAt) = 
            ["name" =: name, 
                "artist" =: (toDocument artist),
                "album" =: (toDocument album), 
                "url" =: url,
                "scrobbledAt" =: scrobbledAt]

data NamedRef = NamedRef {
    refName :: !Text,
    mbid :: !Text
} deriving Show

instance FromJSON NamedRef where
        parseJSON (Object v) = NamedRef <$>
                            v .: "#text" <*>
                            v .: "mbid"

instance ToDocument NamedRef where
        toDocument (NamedRef refName mbid) = ["name" =: refName, "mbid" =: mbid]

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
