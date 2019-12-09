{-# LANGUAGE ScopedTypeVariables, RecordWildCards, DeriveGeneric #-}
module Github where

import Control.Exception (catch, displayException)
import Network.Wreq
import Network.HTTP.Client (HttpException(..))
import Control.Lens ((?~), (&), (^?), (.~))
import System.Posix.Directory
import System.Directory
import System.FilePath.Posix (isExtensionOf)
import qualified Data.Text.IO as T
import Language.Haskell.Exts.SrcLoc(SrcSpan(..))
import Data.Aeson (toJSON, ToJSON, object, (.=))

import Text.Diff.Parse
import Text.Diff.Parse.Types
import Language.Haskell.HLint4
import GHC.Generics (Generic)

data Comment = Comment { lineStart :: Int
                       , lineEnd   :: Maybe Int
                       , idea      :: Idea
                       } deriving (Show)

data GithubComment = SingleLine { commitId  :: Text
                                , path      :: Text
                                , line      :: Int
                                , side      :: Text
                                , body      :: Text
                                }
                   | MultiLine  { commitId  :: Text
                                , path      :: Text
                                , startLine :: Int
                                , startSide :: Text
                                , line      :: Int
                                , side      :: Text
                                , body      :: Text
                                }
                   deriving (Show, Generic)

instance ToJSON GithubComment where
    toJSON (SingleLine commitId path line side body) =
        object [ "commit_id" .= commitId
               , "path" .= path
               , "line" .= line
               , "side" .= side
               , "body" .= body]
    toJSON (MultiLine commitId path startLine startSide line side body) =
        object [ "commit_id" .= commitId
               , "path" .= path
               , "start_line" .= startLine
               , "start_side" .= startSide
               , "line" .= line
               , "side" .= side
               , "body" .= body
               ]

rootEndpoint :: Text
rootEndpoint = "https://api.github.com"

githubOpts :: App Options
githubOpts = do
  token <- reader inputToken
  pure $ defaults & header "Accept" .~ ["application/vnd.github.comfort-fade-preview+json"]
                  & auth ?~ oauth2Token (toS token)

reviewCommentUrl :: Text -> Text -> Int -> Text
reviewCommentUrl owner repo num = 
  rootEndpoint <> "/repos/" <> owner <> "/" <> repo <> "/pulls/" <> toS (show num) <> "/comments"

diffOpts :: App Options
diffOpts = do
  token <- reader inputToken
  pure $ defaults & header "Accept" .~ ["application/vnd.github.v3.diff"]
                  & auth ?~ oauth2Token (toS token)
wrapHttpError :: IO a -> App a
wrapHttpError m = do
  r <- liftIO $ catch (Right <$> m) (\(e :: HttpException) -> pure (Left $ displayException e))
  case r of
    Left err -> throwAppError $ toS err
    Right x  -> pure x

getDiff :: Text -> Text -> Int -> App Text
getDiff owner repo num = do
  opts <- diffOpts 
  let url = rootEndpoint <> "/repos/" <> owner <> "/" <> repo <> "/pulls/" <> toS (show num)
  r <- wrapHttpError $ getWith opts $ toS url
  case r ^? responseBody of
    Nothing -> throwAppError "Diff body expected, not found"
    Just body -> pure $ toS body

getDiffDeltas :: Text -> App FileDeltas
getDiffDeltas rawDiff = 
  case parseDiff rawDiff of
    Left err -> throwAppError $ toS err
    Right x -> pure x
  
processDiff :: Text -> Text -> Int -> App FileDeltas
processDiff owner repo num = getDiff owner repo num >>= getDiffDeltas

filterChangedFiles :: FileDeltas -> IO FileDeltas
filterChangedFiles ds = do
  putStrLn $ "Found " <> show (length activeFiles) <> " files to lint"
  pure activeFiles
 where
  activeFiles = filter (isExtensionOf ".hs" . toS . fileDeltaDestFile) $ filter (createdOrModified . fileDeltaStatus) ds
  createdOrModified Created  = True
  createdOrModified Modified = True
  createdOrModified _        = False

lintChangedFiles :: FileDeltas -> App [Idea]
lintChangedFiles fds = do
  path <- reader inputPath
  liftIO $ do
    filesToLint <- fmap (toS . fileDeltaDestFile) <$> filterChangedFiles fds
    if not (null filesToLint) then hlint filesToLint
    else do
      putStrLn "No files to lint found"
      return []

ideaFilename :: Idea -> Text
ideaFilename Idea{..} = toS $ srcSpanFilename ideaSpan

buildComment :: (FileDelta, [Idea]) -> [Comment]
buildComment (fd@FileDelta{..}, ideas) = foldl (\xs h -> xs ++ check h) [] hunks
  where 
    check :: Hunk -> [Comment]
    check hunk = let matches = filter (`overlapping` hunk) ideas in map mkComment matches
    mkComment idea@Idea{..} = 
      let start = ideaStart idea in
      let end   = start + ideaLength idea in
      Comment start (if start == end then Nothing else Just end) idea 
    (Hunks hunks) = fileDeltaContent
 
    ideaStart :: Idea -> Int
    ideaStart Idea{..} = srcSpanStartLine ideaSpan

    ideaLength :: Idea -> Int
    ideaLength idea@Idea{..} = srcSpanEndLine ideaSpan - ideaStart idea

    overlapping :: Idea -> Hunk -> Bool
    overlapping idea@Idea{..} hunk = hintStart > hunkStart && hintEnd < hunkEnd
     where 
      hintStart = srcSpanStartLine ideaSpan
      hintEnd   = srcSpanEndLine ideaSpan
      hunkStart = rangeStartingLineNumber $ hunkDestRange hunk
      hunkEnd   = hunkStart + rangeNumberOfLines (hunkDestRange hunk)

buildComments :: FileDeltas -> App [Comment]
buildComments fds = do
  ideas <- lintChangedFiles fds
  let matchedDiffs = filter (not . null . snd) $ findIdeas ideas <$> fds
  pure $ matchedDiffs >>= buildComment

  where 
    findIdeas :: [Idea] -> FileDelta -> (FileDelta, [Idea])
    findIdeas ideas fd@FileDelta{..} = (fd, matchingIdeas)
     where
      matchingIdeas = filter (\x -> ideaFilename x == fileDeltaDestFile) ideas

mkGithubComment :: Text -> Comment -> GithubComment
mkGithubComment cid (Comment start Nothing idea@Idea{..}) = 
  SingleLine { commitId = cid
             , path = ideaFilename idea 
             , line = start
             , side = "RIGHT"
             , body = toS $ "**" <> ideaHint <> "**\n" <> fromMaybe "" ideaTo
             }
mkGithubComment cid (Comment start (Just end) idea@Idea{..}) = 
  MultiLine { commitId = cid
            , path = ideaFilename idea
            , startLine = start
            , startSide = "RIGHT"
            , line = end
            , side = "RIGHT"
            , body = toS $ "**" <> ideaHint <> "**\n\n```haskell\n" <> fromMaybe "" ideaTo <> "\n```"
            }

postComment :: Text -> Text -> Int -> GithubComment -> App (Maybe Text)
postComment owner repo num cmt = do
  opts <- githubOpts
  r <- wrapHttpError $ postWith opts (toS $ reviewCommentUrl owner repo num) (toJSON cmt)
  pure $ toS <$> r ^? responseBody