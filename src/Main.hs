{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Reader (runReaderT, ask)

import Data.Default (def)
import qualified Data.Text    as T
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

numerate :: (a -> String) -> Int -> [a] -> [String]
numerate f start xs = let numbers = fmap ((++") ") . show) [start..]
                          list    = fmap f xs
                      in  zipWith (++) numbers list


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  T.putStr "Input search query: "
  query <- T.getLine

  res <- runEitherT $ do
    searchResult <- runQuery $ QueryString query
    liftIO $ do
      T.putStrLn "Found artists:"
      mapM_ putStrLn $ numerate (\art -> T.unpack $ name art)
                                1
                                (artists searchResult)

      T.putStrLn "\nFound albums:"
      let artistsCount = length $ artists searchResult
      mapM_ putStrLn $ numerate (\alb -> mconcat [
                                           T.unpack $ title alb
                                           , " ("
                                           , show $ year alb
                                           , ")"
                                         ])
                                (artistsCount + 1)
                                (albums searchResult)

  either showError (const $ return ()) res
