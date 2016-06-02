{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Handlers where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (runReaderT, ask)

import Data.Default (def)
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client

import API
import Config
import Types

api :: Proxy MusicAPI
api = Proxy

baseUrl :: BaseUrl
baseUrl = let domain = unpack $ toText YandexDomain
          in  BaseUrl Https domain 443


auth :: Client AuthAPI
search :: Client SearchAPI

auth :<|> search = client api baseUrl


runAuth :: EitherT ServantError IO Auth
runAuth = flip runReaderT def $ do
  Config{..} <- ask
  ncrnd <- liftIO ncrnd

  lift $ auth lang externalDomain overembed ncrnd


runQuery :: Auth -> QueryString -> EitherT ServantError IO SearchResult
runQuery auth query = flip runReaderT def $ do
  Config{..} <- ask
  ncrnd <- liftIO ncrnd

  let cookie = mkCookie auth

  lift $ search (Just query) typeParam (Just cookie) lang externalDomain overembed ncrnd
