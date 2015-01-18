{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import Control.Monad
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Text
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as C8 (pack, unpack)
import qualified Data.ByteString as BString

data RecentTracks = RecentTracks {
    recenttracks :: TrackContainer
} deriving (Show, Generic)

instance FromJSON RecentTracks

data TrackContainer = TrackContainer {
    track :: [Track],                    
    attr :: Attributes    
} deriving (Show, Generic)

instance FromJSON TrackContainer where
        parseJSON (Object v) = TrackContainer <$>
                            v .: "track" <*>
                            v .: "@attr"
        parseJSON _ = mzero

data Track = Track {
    name :: !Text
} deriving (Show, Generic)

instance FromJSON Track

data Attributes = Attributes {
    user :: !Text,
    page :: !Text,
    perPage :: !Text,
    totalPages :: !Text,
    total :: !Text    
} deriving (Show, Generic)

instance FromJSON Attributes

fetchTracks :: IO (Maybe RecentTracks)
fetchTracks = fmap decode $ simpleHttp "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=badg&api_key=cc5a08a82ef3d31fef33894f0fbd54cc&format=json"

recentTracks :: IO ()
recentTracks = do
        resp <- fetchTracks
        case resp of
            Just r -> Prelude.putStrLn (show r)
            Nothing -> Prelude.putStrLn "empty!"
