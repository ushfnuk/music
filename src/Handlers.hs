module Handlers where

import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text
import Servant.Client
import Servant.API

import API
import Types

api :: Proxy MusicAPI
api = Proxy

auth   :: EitherT ServantError IO Auth
search :: Maybe QueryString
       -> Maybe Type
       -> Maybe Lang
       -> Maybe ExternalDomain
       -> Maybe Overembed
       -> Maybe NCRnd
       -> EitherT ServantError IO SearchResult

auth :<|> search = client api baseUrl

{-# INLINE baseUrl #-}
baseUrl :: BaseUrl
baseUrl = let domain = unpack $ toText YandexDomain
          in  BaseUrl Https domain 443
