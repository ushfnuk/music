{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module API where

import Servant.API

import Types

type CommonParams t = QueryParam "lang" Lang
                   :> QueryParam "external-domain" ExternalDomain
                   :> QueryParam "overembed" Overembed
                   :> QueryParam "ncrnd" NCRnd
                   :> t

type AuthAPI = "api"
            :> "v2.0"
            :> "handlers"
            :> "auth"
            :> Get '[JSON] Auth

type SearchAPI = "handlers"
              :> "music-search.jsx" 
              :> QueryParam "text" QueryString
              :> QueryParam "type" Type
              :> CommonParams (Get '[JSON] SearchResult)


type MusicAPI = AuthAPI :<|> SearchAPI
