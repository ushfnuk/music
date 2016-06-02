{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import Data.String (IsString(..))
import qualified Data.Text.IO as T

import Safe (atMay)

import System.Exit (ExitCode(..), exitWith)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Servant.Client (ServantError)


import Handlers (runAuth, runQuery)
import Types ( SearchResult(..), QueryString(..)
             , Album(..), Artist(..))


showError :: ServantError -> IO ExitCode
showError err = do
  putStrLn $ "Error: " ++ show err
  return $ ExitFailure 1


numerate :: (Monoid str, IsString str)
         => (a -> str) -> Int -> [a] -> [str]
numerate f start xs = let addBrace = (`mappend` ") ") . fromString . show
                          numbers  = fmap addBrace [start..]
                          list     = fmap f xs
                      in  zipWith mappend numbers list


showInfo :: SearchResult -> IO ()
showInfo = runReaderT $ do
  SearchResult{..} <- ask

  liftIO $ do
    let artistsCount     = length artists
        startNumber      = artistsCount + 1
        showAlbums album = mconcat [ title album
                                   , " ("
                                   , fromString . show $ year album
                                   , ")"]
        numeratedArtists = numerate name 1 artists
        numeratedAlbums  = numerate showAlbums startNumber albums

    T.putStrLn "\nFound artists:"
    mapM_ T.putStrLn numeratedArtists

    T.putStrLn "\nFound albums:"
    mapM_ T.putStrLn numeratedAlbums

  choosePoint


choosePoint :: ReaderT SearchResult IO ()
choosePoint = do
  SearchResult{..} <- ask

  liftIO $ T.putStr "\nChoose point: "
  point <- liftIO readLn

  let failCase = liftIO $ T.putStrLn "Point is out of bounds! Please try again!"
      safePrint xs i = maybe (failCase >> choosePoint)
                             (liftIO . print)
                             (xs `atMay` i)
      artistsCount = length artists
      startNumber  = artistsCount + 1

  if point > artistsCount
    then safePrint albums  (point - startNumber)
    else safePrint artists (point - 1)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  T.putStr "Input search query: "
  query <- T.getLine

  res <- runEitherT $ do
    auth <- runAuth

    searchResult <- runQuery auth $ QueryString query

    liftIO $ showInfo searchResult

  either showError (const $ return ExitSuccess) res >>= exitWith
