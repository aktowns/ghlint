{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Prelude ( module BasePrelude
               , AppConfig(..)
               , AppError
               , App
               , Text
               , throwAppError
               , runApp
               , tryGetEnv
               , lookupEnv
               , liftIO
               , forM_
               , ask
               , reader
               , filterM
               , nub
               , findIndices
               , fromMaybe
               , toS) where

import BasePrelude hiding (String, head)

import Data.String.Conv as Exports (toS, StringConv)

import Data.Text (Text)
import Data.Map (Map)

import Data.List (nub, findIndices)
import Data.Maybe (fromMaybe)

import qualified System.Environment as Env

import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad

data AppConfig = AppConfig { inputPath  :: Text
                           , inputToken :: Text
                           } deriving (Show)

data AppError = AppError Text

newtype App a = App { 
  unApp :: ReaderT AppConfig (ExceptT AppError IO) a 
} deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError AppError, MonadReader AppConfig)

throwAppError :: Text -> App a
throwAppError err = throwError $ AppError err

runApp :: AppConfig -> App a -> IO a
runApp cfg m = do
  r <- runExceptT $ runReaderT (unApp m) cfg
  case r of
    Left (AppError err) -> error $ "Application failed: " <> toS err
    Right x -> pure x

lookupEnv :: Text -> IO (Maybe Text)
lookupEnv ev = fmap toS <$> Env.lookupEnv (toS ev)

tryGetEnv :: Text -> App Text
tryGetEnv var = do
  evvar <- liftIO $ Env.lookupEnv (toS var)
  maybe (throwAppError $ "Failed to find env var: " <> var) pure (toS <$> evvar)
