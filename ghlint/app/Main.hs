module Main where

import Language.Haskell.HLint

import GithubEvent
import Github

getConfig :: IO AppConfig
getConfig = do
  token <- lookupEnv "INPUT_TOKEN"
  path  <- lookupEnv "INPUT_PATH"
  let cfg = AppConfig <$> path <*> token
  case cfg of 
    Nothing -> error "Failed to read config, are INPUT_TOKEN and INPUT_PATH set?"
    Just x -> pure x

main :: IO ()
main = do
  cfg <- getConfig
  runApp cfg $ do
    ev <- readGithubEvent
    if not (isPullRequest ev) then liftIO $ putStrLn "Not a pull request ignoring"
    else do
      diff <- processDiff (getOwner ev) (getRepo ev) (getPullRequest ev)
      comments <- buildComments diff
      let ghComments = mkGithubComment (getSha ev) <$> comments
      forM_ ghComments $ postComment (getOwner ev) (getRepo ev) (getPullRequest ev)