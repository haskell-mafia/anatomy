{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Ci.GitHub (
    hook
  , hooks
  , files
  , repos
  , listRepos
  , renderRepo
  , stats
  ) where

import           Anatomy.Data

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Retry

import qualified Data.Map as M
import           Data.String (String)
import           Data.Time
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Github.Data.Definitions
import           Github.GitData.Trees as GHTree
import           Github.PullRequests
import           Github.Repos
import           Github.Repos.Hooks

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.FilePath.Glob
import           System.IO

-- | List all organization projects
renderRepo :: Repo -> Text
renderRepo repo =
  T.pack . intercalate " " $ [
      githubOwnerLogin . repoOwner $  repo
    , repoName repo
    , fromMaybe "" $ repoDescription repo
    ]

listRepos :: GithubAuth -> Org -> EitherT Error IO [Repo]
listRepos oauth org =
  EitherT $ organizationRepos' (Just oauth) (T.unpack $ orgName org)

-- | Print stats for all repositories
stats :: GithubAuth -> Org -> EitherT Error IO ()
stats oauth org = do
  rs <- listRepos oauth org
  forM_ rs  $ \r -> do
    t <- stat oauth org r
    liftIO . putStrLn . T.unpack $ t

stat :: GithubAuth -> Org -> Repo -> EitherT Error IO Text
stat oauth org repo = do
  now <- liftIO $ getCurrentTime
  prs <- EitherT $ pullRequestsWith' (Just oauth) (T.unpack $ orgName org) (repoName repo) "all"
  let prs' = flip filter prs $ \pr ->
        let prdate = fromGithubDate . pullRequestCreatedAt $ pr
            diff = diffUTCTime now prdate
        in diff <= 60 * 60 * 24 * 7
  pure $ T.pack . intercalate " " $ [
      githubOwnerLogin . repoOwner $  repo
    , repoName repo
    , show . length $ prs'
    ]

-- | Register all hooks for all organization projects
hooks :: HipchatToken -> HipchatRoom -> HooksUrl -> GithubAuth -> Org -> EitherT Error IO ()
hooks token room url oauth org = do
  rs <- listRepos oauth org
  forM_ rs $ \r ->
    retrye $ hook url token room oauth (Org . T.pack . githubOwnerLogin . repoOwner $ r) (ProjectName . T.pack $ repoName r)

-- | Register all hooks for specified project
hook :: HooksUrl -> HipchatToken -> HipchatRoom -> GithubAuth -> Org -> ProjectName -> EitherT Error IO ()
hook url token room oauth org p = do
  liftIO . T.putStrLn $ "Creating hooks for [" <> orgName org <> "/" <> renderName p <> "]"
  forM_ [jenkins url, hipchat token room, webhook url] $ \h ->
    h oauth org p

-- | Register the jenkins hook
-- |   schema: https://api.github.com/hooks
jenkins :: HooksUrl -> GithubAuth -> Org -> ProjectName -> EitherT Error IO ()
jenkins url oauth org p = EitherT $ do
  _ <- createHook oauth (s orgName org) (s renderName p) "jenkins" (M.fromList [
       ("jenkins_hook_url", (T.unpack (hooksUrl url) </> "github-webhook/"))
     ]) (Just [
       "push"
     ]) (Just True)
  pure $ Right ()

-- | Register the generic web hook
-- |   schema: https://developer.github.com/v3/repos/hooks/#create-a-hook
webhook :: HooksUrl -> GithubAuth -> Org -> ProjectName -> EitherT Error IO ()
webhook url oauth org p = EitherT $ do
  _ <- createHook oauth (s orgName org) (s renderName p) "web" (M.fromList [
       ("url", (T.unpack (hooksUrl url) </> "github"))
     , ("content_type", "json")
     ]) (Just [
       "*"
     ]) (Just True)
  pure $ Right ()

-- | Filter the list of files in a specified Github project (at the HEAD)
-- |  NOTE: This is not optimized and will retrieve the entire git tree first
files :: GithubAuth -> Org -> ProjectName -> String -> IO [String]
files oauth org p fileGlob = do
  ghTree <- GHTree.nestedTree (Just oauth) (s orgName org) (s renderName p) "HEAD" >>= handler
  let pattern = compile fileGlob
  return . filter (match pattern) . fmap gitTreePath . treeGitTrees $ ghTree

s :: (a -> Text) -> a -> String
s f =
  T.unpack . f

-- | Register the hipchat hook
-- |   schema: https://api.github.com/hooks
hipchat :: HipchatToken -> HipchatRoom -> GithubAuth -> Org -> ProjectName -> EitherT Error IO ()
hipchat token room oauth org p = EitherT $ do
  _ <- createHook oauth (s orgName org) (s renderName p) "hipchat" (M.fromList [
      ("auth_token", s hipchatToken token)
    , ("room", s hipchatRoom room)
--    , ("restrict_to_branch", "")
--    , ("color", "")
--    , ("server", "")
    , ("notify", "1")
    , ("quiet_fork", "0")
    , ("quiet_watch", "0")
    , ("quiet_comments", "0")
    , ("quiet_wiki", "0")
    ]) (Just [
      "commit_comment"
    , "create"
    , "delete"
    , "deployment"
    , "deployment_status"
    , "download"
    , "follow"
    , "fork"
    , "fork_apply"
    , "gist"
    , "gollum"
    , "issue_comment"
    , "issues"
    , "member"
    , "public"
    , "pull_request"
    , "pull_request_review_comment"
    , "push"
    , "release"
    , "status"
    , "team_add"
    , "watch"
    ]) (Just True)
  pure $ Right ()

repos :: GithubAuth -> String -> IO [String]
repos oauth jn =
  fmap repoName <$> (organizationRepos' (Just oauth) jn >>= handler)


handler :: Either Error a -> IO a
handler t =
  case t of
    Left e ->
      hPutStrLn stderr (T.unpack $ renderGithubError e) >>
        exitFailure
    Right v ->
      pure v
