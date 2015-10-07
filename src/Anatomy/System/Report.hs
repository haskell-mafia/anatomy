{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.System.Report (
    report
  , reportOn
  ) where

import           Anatomy.Data

import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import qualified Ci.GitHub as G

import           Data.List as L
import           Data.Text as T

import           Github.Data
import           Github.Auth
import           Github.Repos

import           P

import           System.IO

-- | Produce a report for all projects for specified organization.
report :: Org -> [Project a b] -> EitherT ReportError IO [Report a b]
report o ps =
  liftIO G.auth >>= \a -> scrape a o >>= \gs -> forM ps (reportOn gs) >>= \rs -> pure (rs <> scraps gs ps)

-- | Get all organization repos.
scrape :: GithubAuth -> Org -> EitherT ReportError IO [Repo]
scrape a o =
  bimapEitherT GitHubError id . EitherT  $ organizationRepos' (Just a) (T.unpack . orgName $ o)

-- | Find all the scraps, i.e. repositories we don't have anatomy metadata for.
scraps :: [Repo] -> [Project a b] -> [Report a b]
scraps gs ps =
  let ns = name <$> ps
   in Report Nothing . Just <$> L.filter (not . flip L.elem ns . T.pack . repoName) gs

-- | Report on a particular project, given the current set of github repositories.
reportOn :: [Repo] -> Project a b -> EitherT ReportError IO (Report a b)
reportOn rs p =
  pure $
    Report {
        reportProject = Just p
      , reportGithub = L.find ((==) (name p) . T.pack . repoName) rs
      }
