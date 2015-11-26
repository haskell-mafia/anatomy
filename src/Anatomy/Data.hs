{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Data (
    Org (..)
  , GithubTemplate(..)
  , Team (..)
  , Project (..)
  , Status (..)
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
  , renderReportError
  , renderSyncError
  , project
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Github.Data

import           P

newtype Org =
  Org {
      orgName :: Text
    } deriving (Eq, Show)

newtype GithubTemplate =
  GithubTemplate {
      githubTemplate ::  Text
    } deriving (Show, Eq)

data Team =
  Team {
      teamName :: Text
    , teamGithubId :: Int
    } deriving (Eq, Show)

data Project a b =
  Project {
      name :: Text
    , description :: Text
    , status :: Status
    , cls :: a
    , category :: Maybe b
    , teams :: [Team]
    , builds :: [Build]
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

project :: Text -> Text -> Status -> a -> [BuildSkeleton] -> Project a b
project n d s c b =
  Project n d s c Nothing [] ((\(BuildSkeleton w r t) -> Build (w n) r t) <$> b)

data Status =
    Idea             -- A readme.
  | Nursery          -- Some code. Might work. Probably doesn't.
  | Mature           -- Stable, small, safe incremental changes only.
  | Broken           -- Broken, but in use, needs some attention, but needs to be treated with a soft touch.
  | Deprecated       -- Don't use it.
  | Unknown          -- Something so we can ease into this.
  deriving (Eq, Show)

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

data Report a b =
  Report {
      reportProject :: Maybe (Project a b)
    , reportGithub :: Maybe Repo
    } deriving (Eq, Show)

data ReportError =
  GitHubError Error
  deriving (Show)

data SyncError =
    SyncReportError ReportError
  | SyncCreateError Error
  deriving (Show)

data SyncMode =
    Diagnose
  | Sync
  | SyncUpdate
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
