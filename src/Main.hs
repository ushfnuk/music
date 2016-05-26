{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Reader (runReaderT, ask)

import Data.Default (def)
import Data.String (IsString(..))
import qualified Data.Text.IO as T
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Servant.Client (ServantError)


import Config
import Handlers (search)
import Types (SearchResult(..), QueryString(..), Album(..), Artist(..))


runQuery :: QueryString -> EitherT ServantError IO SearchResult
runQuery query = flip runReaderT def $ do
  Config{..} <- ask
  ncrnd <- liftIO ncrnd

  lift $ search (Just query) typeParam lang externalDomain overembed ncrnd


showError :: ServantError -> IO ()
showError err = putStrLn $ "Error: " ++ show err


numerate :: (Monoid str, IsString str, Enum num, Show num)
         => (a -> str) -> num -> [a] -> [str]
numerate f start xs = let addBrace = (`mappend` ") ") . fromString . show
                          numbers  = fmap addBrace [start..]
                          list     = fmap f xs
                      in  zipWith mappend numbers list


choosePoint :: SearchResult -> IO ()
choosePoint res = flip runReaderT res $ do
  SearchResult{..} <- ask

  liftIO $ do
    T.putStrLn "\nFound artists:"
    mapM_ T.putStrLn $ numerate name 1 artists

    T.putStrLn "\nFound albums:"
    let artistsCount     = length artists
        startNumber      = artistsCount + 1
        showAlbums album = mconcat [ title album
                                   , " ("
                                   , fromString . show $ year album
                                   , ")"]

    mapM_ T.putStrLn $ numerate showAlbums startNumber albums

    T.putStr "\nChoose point: "
    point <- T.getLine
    T.putStrLn point


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  T.putStr "Input search query: "
  query <- T.getLine

  res <- runEitherT $ do
    searchResult <- runQuery $ QueryString query
    liftIO $ choosePoint searchResult

  either showError (const $ return ()) res
