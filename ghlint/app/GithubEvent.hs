{-# LANGUAGE DeriveGeneric #-}
module GithubEvent where

import qualified Data.Aeson as Json
import Data.Aeson (FromJSON)

import GHC.Generics (Generic)

data GithubEvent = GithubEvent { pullRequest :: GithubPullRequest
                               , repository  :: GithubRepository
                               } deriving (Generic, Show)

instance FromJSON GithubEvent where
  parseJSON = Json.genericParseJSON 
    Json.defaultOptions { Json.fieldLabelModifier = Json.camelTo2 '_' }

data GithubPullRequest = GithubPullRequest { number :: Int
                                           , head   :: GithubPullRequestHead
                                           } deriving (Generic, Show)

instance FromJSON GithubPullRequest where
  parseJSON = Json.genericParseJSON 
    Json.defaultOptions { Json.fieldLabelModifier = Json.camelTo2 '_' }

data GithubPullRequestHead = GithubPullRequestHead { sha :: Text
                                                   , ref :: Text
                                                   } deriving (Generic, Show)

instance FromJSON GithubPullRequestHead where
  parseJSON = Json.genericParseJSON 
    Json.defaultOptions { Json.fieldLabelModifier = Json.camelTo2 '_' }

data GithubRepository = GithubRepository { owner :: GithubRepositoryOwner
                                         , name  :: Text
                                         } deriving (Generic, Show)

instance FromJSON GithubRepository where
  parseJSON = Json.genericParseJSON 
    Json.defaultOptions { Json.fieldLabelModifier = Json.camelTo2 '_' }
 
data GithubRepositoryOwner = GithubRepositoryOwner { login :: Text } deriving (Generic, Show)

instance FromJSON GithubRepositoryOwner where
  parseJSON = Json.genericParseJSON 
    Json.defaultOptions { Json.fieldLabelModifier = Json.camelTo2 '_' }

readGithubEvent :: App GithubEvent
readGithubEvent = do
  eventPath <- tryGetEnv "GITHUB_EVENT_PATH"
  ghEvent <- liftIO $ Json.decodeFileStrict (toS eventPath)
  maybe (throwAppError $ "Failed to decode event at: " <> eventPath) pure ghEvent

getOwner :: GithubEvent -> Text
getOwner = login . owner . repository

getRepo :: GithubEvent -> Text
getRepo = name . repository

isPullRequest :: GithubEvent -> Bool
isPullRequest ev = getPullRequest ev /= 0

getPullRequest :: GithubEvent -> Int
getPullRequest = number . pullRequest

getSha :: GithubEvent -> Text
getSha = sha . head . pullRequest 