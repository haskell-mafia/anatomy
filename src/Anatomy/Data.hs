{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Data (
    Org (..)
  , GithubTemplate (..)
  , GithubAuth (..)
  , Team (..)
  , T.Permission (..)
  , Project (..)
  , ProjectName (..)
  , Variant (..)
  , Report (..)
  , SyncMode (..)
  , SyncAction (..)
  , ReportError (..)
  , SyncError (..)
  , Build (..)
  , BuildSkeleton (..)
  , Replace (..)
  , BuildTemplate (..)
  , XmlDiffError (..)
  , HooksUrl (..)
  , JenkinsConfiguration (..)
  , JenkinsUrl (..)
  , JenkinsUser (..)
  , JenkinsAuth (..)
  , SyncBuildError (..)
  , GithubCreateError (..)
  , HipchatToken (..)
  , HipchatRoom (..)
  , Branch (..)
  , G.Protection (..)
  , G.RequiredStatusChecks (..)
  , G.EnforcementLevel (..)
  , G.PushRestrictions (..)
  , G.User (..)
  , G.TeamName (..)
  , renderReportError
  , renderSyncError
  , renderGithubError
  , renderBuildError
  , renderXmlDiffError
  , project
  , retry
  , retrye
  ) where

import           Control.Exception.Base (SomeException)
import           Control.Retry

import qualified Data.Text as T

import           Github.Auth
import           Github.Data (Error (..), Permission, Protection, Repo)
import qualified Github.Data.Definitions as G
import qualified Github.Data.Teams as T

import           P

import           System.IO (IO)

import           Control.Monad.Trans.Either

retry :: IO a -> IO a
retry =
  recoverAll (limitRetries 5 <> exponentialBackoff 100000 {-- 100 ms --})

retrye :: EitherT e IO a -> EitherT e IO a
retrye e =
  EitherT $ retrying
    (limitRetries 5 <> exponentialBackoff 100000 {-- 100 ms --})
    (\_ x -> pure $ isLeft x)
    (runEitherT e)

newtype HooksUrl =
  HooksUrl {
      hooksUrl :: Text
    } deriving (Eq, Show)

newtype JenkinsUrl =
  JenkinsUrl {
      jenkinsUrl :: Text
    } deriving (Eq, Show)

data JenkinsConfiguration =
  JenkinsConfiguration {
      jenkinsUser :: JenkinsUser
    , jenkinsOAuth :: JenkinsAuth
    , jenkinsHost :: JenkinsUrl
    } deriving (Eq, Show)

newtype JenkinsUser =
  JenkinsUser {
      renderUser :: Text
    } deriving (Eq, Show)

newtype JenkinsAuth =
  JenkinsAuth {
      jenkinsAuth :: Text
    } deriving (Eq, Show)

newtype Org =
  Org {
      orgName :: Text
    } deriving (Eq, Show)

newtype GithubTemplate =
  GithubTemplate {
      githubTemplate ::  Text
    } deriving (Show, Eq)

newtype HipchatToken =
  HipchatToken {
      hipchatToken ::  Text
    } deriving (Show, Eq)

newtype HipchatRoom =
  HipchatRoom {
      hipchatRoom ::  Text
    } deriving (Show, Eq)

data Team =
  Team {
      teamName :: Text
    , teamGithubId :: Int
    , teamPermission :: Maybe Permission
    } deriving (Eq, Show)

data Project a b c =
  Project {
      name :: ProjectName
    , description :: Text
    , status :: c
    , cls :: a
    , category :: Maybe b
    , teams :: [Team]
    , builds :: [Build]
    , branchProtection :: [(Branch, Protection)]
    } deriving (Eq, Show)

newtype ProjectName =
  ProjectName {
      renderName :: Text
    } deriving (Eq, Show)

newtype Branch =
  Branch {
      renderBranch :: Text
    } deriving (Eq, Show)

data Build =
  Build {
      buildName :: Text
    , replacements :: [Replace]
    , template :: BuildTemplate
    } deriving (Eq, Show)

data BuildSkeleton =
  BuildSkeleton {
      skeletonNameWith :: Text -> Text
    , skeletonReplacements :: [Replace]
    , skeletonTemplate :: BuildTemplate
    }

data Replace =
  Replace {
      replaceKey :: Text
    , replaceValue :: Text
    } deriving (Eq, Show)

newtype BuildTemplate =
  BuildTemplate {
      buildTemplate :: Text
    } deriving (Eq, Show)

project :: Text -> Text -> c -> a -> [BuildSkeleton] -> [(Branch, Protection)] -> Project a b c
project n d s c b pro =
  Project (ProjectName n) d s c Nothing [] ((\(BuildSkeleton w r t) -> Build (w n) r t) <$> b) pro

data SyncAction =
    CreateRepository
  | ArchiveRepository
  | CreateBuilds
  | UpdateDescription
  | UpdatePermissions
  deriving (Eq, Show)

data Variant =
    Cli
  | Daemon
  | Service
  | Library
  deriving (Eq, Show)

data Report a b c =
  Report {
      reportProject :: Maybe (Project a b c)
    , reportGithub :: Maybe Repo
    } deriving (Eq, Show)

data ReportError =
  GitHubError Error
  deriving (Show)

data GithubCreateError =
    CreateRepoError Error
  | AddTeamError SomeException
  | AddProtectionError SomeException
    deriving Show

data SyncError =
    SyncReportError ReportError
  | SyncCreateError Error
  | SyncGithubError GithubCreateError
  | SyncBuildError SyncBuildError
    deriving Show

data SyncBuildError =
  XmlError Build XmlDiffError
  deriving (Eq, Show)

data SyncMode =
    Diagnose
  | Sync
  | SyncUpdate
    deriving (Eq, Show)

data XmlDiffError =
  XmlParseError Text
  deriving (Eq, Show)

renderReportError :: ReportError -> Text
renderReportError err =
  case err of
    GitHubError e ->
      renderGithubError e

renderSyncError :: SyncError -> Text
renderSyncError err =
  case err of
    SyncReportError e ->
      renderReportError e
    SyncCreateError e ->
      renderGithubError e
    SyncGithubError e ->
      renderGithubCreateError e
    SyncBuildError e ->
      renderBuildError e

renderGithubCreateError :: GithubCreateError -> Text
renderGithubCreateError err =
  case err of
    CreateRepoError e ->
      "Error creating github repository: " <> renderGithubError e
    AddTeamError e ->
      "Error adding team to repository: " <> T.pack (show e)
    AddProtectionError e ->
      "Error branch protection to repository: " <> T.pack (show e)

renderGithubError :: Error -> Text
renderGithubError err =
  case err of
    HTTPConnectionError ex ->
      "A HTTP error occurred trying to contact github: " <> (T.pack . show) ex
    ParseError s ->
      "An error occured trying to parse response from github: " <> T.pack s
    JsonError s ->
      "An error occured trying to process json response from github: " <> T.pack s
    UserError s ->
      "We made an invalid request to github, this is likely a bug in the calling code (https://github.com/ambiata/anatomy/issues): " <> T.pack s

renderBuildError :: SyncBuildError -> Text
renderBuildError e =
  case e of
    XmlError b t ->
      "Failed syncing build [" <> buildName b <> "] with: " <> renderXmlDiffError t

renderXmlDiffError :: XmlDiffError -> Text
renderXmlDiffError e =
  case e of
    XmlParseError t ->
      "Failed to parse xml [" <> t <> "]."
