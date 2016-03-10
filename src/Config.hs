module Config where

import Data.Default (Default(..))
import System.Random (getStdRandom, randomR)
import Types (NCRnd(NCRnd), Type(..), Lang(..), ExternalDomain(..), Overembed(Overembed))


data Config = Config
  { typeParam      :: Maybe Type
  , lang           :: Maybe Lang
  , externalDomain :: Maybe ExternalDomain
  , overembed      :: Maybe Overembed
  , ncrnd          :: IO (Maybe NCRnd)
  }

instance Default Config where
  def = Config 
    { typeParam = Just All
    , externalDomain = Just YandexDomain
    , lang = Just Uk
    , overembed = Just $ Overembed False
    , ncrnd = Just . NCRnd <$> getStdRandom (randomR (0, 1))
    }
