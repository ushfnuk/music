{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, runEitherT)
import Control.Monad.Trans.Reader (runReaderT, ask)
import Data.Default (def)
import qualified Data.Text.IO as T
import Servant.Client (ServantError)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)


import Config
import Handlers (search)
import Types (SearchResult, QueryString(..))


runQuery :: QueryString -> EitherT ServantError IO SearchResult
runQuery query = flip runReaderT def $ do
  Config{..} <- ask
  ncrnd <- liftIO ncrnd

  lift $ search (Just query) typeParam lang externalDomain overembed ncrnd


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  T.putStr "Input search query: "
  query <- T.getLine

  res <- runEitherT . runQuery $ QueryString query
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right s  -> print s
