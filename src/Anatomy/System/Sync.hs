{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anatomy.System.Sync (
    sync
  , syncReport
  ) where

import           Anatomy.Data
import           Anatomy.System.Report

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import qualified Anatomy.Ci.GitHub as G
import           Anatomy.Ci.Jenkins (JenkinsUrl (..), HooksUrl (..))
import qualified Anatomy.Ci.Jenkins as J

import           Data.Text (Text)
import qualified Data.Text as T

import           Github.Repos
import qualified Github.Organizations as GO

import           P

import           System.IO
import           System.IO.Temp
import           System.Process
import           System.Posix.Env (getEnv)

-- | Create github repos for things listed in anatomy but don't have
--   github repositories.  Repositories will be created with owner
--   specified by "team" argument. This is normally owners and extra
--   teams are specified as permissions elsewhere (but that bit isn't
--   implemented yet ...)
sync ::
  Text
  -> JenkinsUrl
  -> HooksUrl
  -> (a -> Maybe GithubTemplate)
  -> Org
  -> Team
  -> Team
  -> [Project a b]
  -> SyncMode
  -> EitherT SyncError IO [Project a b]
sync defaultJenkinsUser j h templateName o t everyone ps m = do
  ss <- bimapEitherT SyncReportError id $ syncables <$> report o ps
  when (m == Sync) $ do
    forM_ ss $ create defaultJenkinsUser j h templateName o t everyone
  pure ss

-- | Log sync reporting for the specified projects.
syncReport :: [Project a b] -> IO ()
syncReport ps =
  forM_ ps $ \p ->
    putStrLn . T.unpack . T.intercalate " " $ [
        "new github project:"
      , name p
      ]

syncables :: [Report a b] -> [Project a b]
syncables rs =
  rs >>= \r -> case r of
    Report Nothing Nothing -> []
    Report (Just _) (Just _) -> []
    Report Nothing (Just _) -> []
    Report (Just p) Nothing -> [p]

create :: Text -> JenkinsUrl -> HooksUrl -> (a -> Maybe GithubTemplate) -> Org -> Team -> Team -> Project a b -> EitherT SyncError IO ()
create defaultJenkinsUser j h templateName o t everyone p = do
  auth <- liftIO G.auth
  void . bimapEitherT SyncCreateError id . EitherT $
      do
        r <-  createOrganizationRepo auth (T.unpack . orgName $ o) (newOrgRepo . T.unpack . name $ p) {
            newOrgRepoDescription = Just . T.unpack . description $ p
          , newOrgRepoPrivate = Just True
          , newOrgRepoHasIssues = Just True
          , newOrgRepoHasWiki = Just False
          , newOrgRepoHasDownloads = Just False
          , newOrgRepoTeamId = Just . toInteger . teamGithubId $ t
          , newOrgRepoAutoInit = Just False
          , newOrgRepoLicense = Nothing
          , newOrgRepoGitIgnore = Nothing
          }
        applyTemplate r p $ templateName (cls p)
        addExtraTeam (Just auth) r
        setupCi r defaultJenkinsUser o j h p
        return r
      where
        addExtraTeam auth (Right _) =
          void $ GO.addTeamToRepo auth teamId (T.unpack $ orgName o) (T.unpack $ name p)
        addExtraTeam _ _ =
          return ()
        teamId =
          teamGithubId everyone


setupCi :: Either Error Repo -> T.Text -> Org -> JenkinsUrl -> HooksUrl -> Project a b -> IO ()
setupCi (Right _) defaultJenkinsUser o j h p = do
  createJenkinsJob defaultJenkinsUser j h p
  setupHooks o h p
setupCi _ _ _ _ _ _ =
  return ()


setupHooks :: Org -> HooksUrl -> Project a b -> IO ()
setupHooks o h p = do
    auth <- G.auth
    G.hook h auth (T.unpack $ orgName o) (T.unpack $ name p)


createJenkinsJob :: Text -> JenkinsUrl -> HooksUrl -> Project a b -> IO ()
createJenkinsJob defaultJenkinsUser j h p = do
  auth <- G.auth'
  jenkinsUser <- fromMaybe (T.unpack defaultJenkinsUser) <$> getEnv "JENKINS_USER"
  J.createJob J.ModJob {
      J.jobreq = J.Job {
            J.org = jenkinsUser
          , J.oauth = auth
          , J.jobName = T.unpack $ name p
          , J.jenkinsHost = j
          , J.jenkinsHooks = h
        },
      J.templatefile = jobTemplate,
      J.params = toLookup p "./bin/ci"
  }
  J.createJob J.ModJob {
      J.jobreq = J.Job {
            J.org = jenkinsUser
          , J.oauth = auth
          , J.jobName = (T.unpack $ name p) <> ".branches"
          , J.jenkinsHost = j
          , J.jenkinsHooks = h
        },
      J.templatefile = jobTemplateBranch,
      J.params = toLookup p "./bin/ci.branches"
  }


toLookup :: Project a b -> CICommand -> [Char] -> Maybe [Char]
toLookup p cmd s = case s of
  "project"         -> Just $ T.unpack $ name p
  "env_CI_COMMAND"  -> Just $ T.unpack cmd
  _                 -> Nothing

type CICommand = Text

jobTemplate :: [Char]
jobTemplate = "templates/job.xml"

jobTemplateBranch :: [Char]
jobTemplateBranch = "templates/branches.xml"

applyTemplate :: Either Error Repo -> Project a b -> Maybe GithubTemplate -> IO ()
applyTemplate (Right _) p (Just tmpName) =
  pushTemplate p tmpName
applyTemplate _ _ _ = return ()

pushTemplate :: Project a b -> GithubTemplate -> IO ()
pushTemplate p template =
  void .withSystemTempDirectory "template_repo" $ \dir ->
    system $ P.intercalate " " ["./bin/clone_template", dir, T.unpack . githubTemplate $ template, T.unpack $ name p]
