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
            :> "v2.1"
            :> "handlers"
            :> "auth"
            :> CommonParams (Get '[JSON] Auth)

type SearchAPI = "handlers"
              :> "music-search.jsx" 
              :> QueryParam "text" QueryString
              :> QueryParam "type" Type
              :> Header "Cookie" CookieString
              :> CommonParams (Get '[JSON] SearchResult)


type MusicAPI = AuthAPI :<|> SearchAPI
