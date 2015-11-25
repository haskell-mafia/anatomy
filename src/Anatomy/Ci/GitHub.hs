{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Ci.GitHub (
    auth
  , auth'
  , hook
  , hooks
  , files
  , repos
  , list
  , stats
  ) where

import           Anatomy.Ci.Jenkins (HooksUrl (..))

import           Control.Concurrent.Async

import qualified Data.Map as M
import           Data.String (String)
import           Data.Time

import           Github.Data.Definitions
import           Github.GitData.Trees as GHTree
import           Github.PullRequests
import           Github.Auth
import           Github.Repos
import           Github.Repos.Hooks

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.FilePath.Glob
import           System.IO
import           System.Posix.Env


env :: String -> IO String
env var = getEnv var >>= \t -> case t of
  Nothing -> putStrLn ("Need to specify $" <> var) >> exitFailure
  Just a -> pure a

auth' :: IO String
auth' = env "GITHUB_OAUTH"

auth :: IO GithubAuth
auth = GithubOAuth <$> auth'

hipchat' :: IO String
hipchat' = env "HIPCHAT_TOKEN"

hiproom' :: IO String
hiproom' = fromMaybe "eng" <$> getEnv "HIPCHAT_ROOM_GITHUB"

-- | List all organization projects
list :: GithubAuth -> String -> IO ()
list oauth org = do
  organizationRepos' (Just oauth) org >>= handler >>= \repos' ->
    forM_ repos' $ \r ->
      putStrLn . intercalate " " $ [
          githubOwnerLogin . repoOwner $  r
        , repoName r
        , fromMaybe "" $ repoDescription r
        ]

-- | Print stats for all repositories
stats :: GithubAuth -> String -> IO ()
stats oauth org = do
  now <- getCurrentTime
  repos' <- organizationRepos' (Just oauth) org >>= handler
  void . flip mapConcurrently repos' $ \r -> do
      pullRequestsWith' (Just oauth) org (repoName r) "all" >>= handler >>= \prs -> do
        let prs' = flip filter prs $ \pr ->
              let prdate = fromGithubDate . pullRequestCreatedAt $ pr
                  diff = diffUTCTime now prdate
               in diff <= 60 * 60 * 24 * 7
        putStrLn . intercalate " " $ [
            githubOwnerLogin . repoOwner $  r
          , repoName r
          , show . length $ prs'
          ]

-- | Register all hooks for all organization projects
hooks :: HooksUrl -> GithubAuth -> String -> IO ()
hooks url oauth org = do
  repos' <- organizationRepos' (Just oauth) org >>= handler
  forM_ repos' $ \r ->
    hook url oauth (githubOwnerLogin . repoOwner $ r) (repoName r)

-- | Register all hooks for specified project
hook :: HooksUrl -> GithubAuth -> String -> String -> IO ()
hook url oauth name project = void $ do
  putStrLn $ "Creating hooks for [" <> name </> project <> "]"
  forM_ [jenkins url, hipchat] $ \h ->
    h oauth name project >>= handler

-- | Register the jenkins hook
-- |   schema: https://api.github.com/hooks
jenkins :: HooksUrl -> GithubAuth -> String -> String -> IO (Either Error Hook)
jenkins url oauth name project =
  createHook oauth name project "jenkins" (M.fromList [
       ("jenkins_hook_url", (hooksUrl url </> "github-webhook/"))
     ]) (Just [
       "push"
     ]) (Just True)

-- | Filter the list of files in a specified Github project (at the HEAD)
-- |  NOTE: This is not optimized and will retrieve the entire git tree first
files :: GithubAuth -> String -> String -> String -> IO [String]
files oauth name project fileGlob = do
  ghTree <- GHTree.nestedTree (Just oauth) name project "HEAD" >>= handler
  let pattern = compile fileGlob
  return . filter (match pattern) . fmap gitTreePath . treeGitTrees $ ghTree


-- | Register the hipchat hook
-- |   schema: https://api.github.com/hooks
hipchat :: GithubAuth -> String -> String -> IO (Either Error Hook)
hipchat oauth name project =
  hipchat' >>= \token -> hiproom' >>= \room -> createHook oauth name project "hipchat" (M.fromList [
      ("auth_token", token)
    , ("room", room)
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

repos :: GithubAuth -> String -> IO [String]
repos oauth name =
  fmap repoName <$> (organizationRepos' (Just oauth) name >>= handler)


handler :: Either Error a -> IO a
handler (Left e) = putStrLn (show e) >> exitFailure
handler (Right v) = return v
