module Handlers where

import Control.Monad.Trans.Either
import Data.Proxy
import Servant.Client
import Servant.API

import API
import Types

domain :: String
domain = "music.yandex.ru"

api :: Proxy MusicAPI
api = Proxy

auth   :: EitherT ServantError IO Auth
search :: Maybe QueryString
       -> Maybe Type
       -> Maybe Lang
       -> Maybe ExternalDomain
       -> Maybe NCRnd
       -> EitherT ServantError IO SearchResult

auth :<|> search = client api baseUrl

baseUrl :: BaseUrl
baseUrl = BaseUrl Http domain 80
