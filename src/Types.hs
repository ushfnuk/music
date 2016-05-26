{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text
import GHC.Generics
import Servant.API (FromText(..), ToText(..))

data Auth = Auth
  { csrf      :: Text
  , device_id :: Text
  , logged    :: Bool
  , premium   :: Maybe Text 
  , invalid   :: Bool
  --, timestamp :: Int
  } deriving (Show, Generic)


instance FromJSON Auth
instance ToJSON Auth


data SearchResult = SearchResult
  { albums  :: [Album]
  , artists :: [Artist]
  } deriving (Show, Generic)

instance FromJSON SearchResult where
  parseJSON (Object vs) = SearchResult <$> (vs .: "albums"  >>= (.: "items"))
                                       <*> (vs .: "artists" >>= (.: "items"))
  parseJSON _           = mzero

instance ToJSON SearchResult

data Album = Album
  { albumId :: Int
  , title   :: Text
  , year    :: Int
  } deriving (Show, Generic)

instance FromJSON Album where
  parseJSON (Object vs) = Album <$> vs .: "id"
                                <*> vs .: "title"
                                <*> vs .: "year"
  parseJSON _           = mzero
instance ToJSON Album

data Artist = Artist
  { artistId :: Int
  , name     :: Text
  } deriving (Show, Generic)

instance FromJSON Artist where
  parseJSON (Object vs) = Artist <$> vs .: "id"
                                 <*> vs .: "name"
  parseJSON _           = mzero
instance ToJSON Artist


data Lang = Uk | En | Ru deriving Show

instance ToText Lang where
  toText Uk = "uk"
  toText En = "en"
  toText Ru = "ru"

instance FromText Lang where
  fromText "uk" = Just Uk
  fromText "ru" = Just Ru
  fromText "en" = Just En
  fromText _    = Nothing


newtype QueryString = QueryString { query :: Text } deriving Show

instance ToText QueryString where
  toText = query

instance FromText QueryString where
  fromText = Just . QueryString


data Type = All deriving Show

instance ToText Type where
  toText _ = "all"

instance FromText Type where
  fromText "all" = Just All
  fromText _     = Nothing


newtype NCRnd = NCRnd { ncrnd :: Double } deriving Show

instance ToText NCRnd where
  toText = toText . ncrnd

instance FromText NCRnd where
  fromText str = NCRnd <$> fromText str


data ExternalDomain = YandexDomain deriving Show

instance ToText ExternalDomain where
  toText YandexDomain = "music.yandex.ru"

instance FromText ExternalDomain where
  fromText "music.yandex.ru" = Just YandexDomain
  fromText _                 = Nothing


newtype Overembed = Overembed { overembed :: Bool } deriving Show

instance ToText Overembed where
  toText = toText . overembed

instance FromText Overembed where
  fromText bool = Overembed <$> fromText bool
