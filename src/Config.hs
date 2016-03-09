module Config where

import Data.Default (Default(..))
import System.Random (getStdRandom, randomR)
import Types (NCRnd(NCRnd), Type(..), Lang(..), ExternalDomain(..))


data Config = Config
  { typeParam      :: Maybe Type
  , lang           :: Maybe Lang
  , externalDomain :: Maybe ExternalDomain
  , ncrnd          :: IO (Maybe NCRnd)
  }

instance Default Config where
  def = Config 
    { typeParam = Just All
    , externalDomain = Just YandexDomain
    , lang = Just Uk
    , ncrnd = Just . NCRnd <$> getStdRandom (randomR (0, 1))
    }
