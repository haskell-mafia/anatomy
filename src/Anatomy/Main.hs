{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Main where

import           Anatomy

import           BuildInfo_ambiata_anatomy

import           Control.Monad.IO.Class

import           Data.Text (Text)
import qualified Data.Text as T

import           Github.Data

import           Options.Applicative

import           P

import           System.IO
import           System.Exit

import           X.Options.Applicative hiding (VersionCommand)


data Command =
    VersionCommand
  | ReportCommand
  | DiagnoseCommand
  | SyncCommand
  | SyncUpdateCommand
  deriving (Eq, Show)

anatomyMain ::
  (a -> Maybe GithubTemplate) -- ^ a function that takes the class of a project and decides which template to apply, if any at all...
  -> Text       -- ^ Default Jenkins user, overridden by the JENKINS_USER environment variable
  -> JenkinsUrl -- ^ Jenkins base url
  -> HooksUrl   -- ^ Jenkins url to point github hooks to
  -> Org        -- ^ GitHub organisation
  -> Team       -- ^ A team of people to add as Collaborators on the new repo.
  -> Team       -- ^ A team of everyone in your organisation
  -> [Project a b]
  -> IO ()
anatomyMain templateName defaultJenkinsUser j h o owners everyone projects = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let sync' s = (orDie renderSyncError $ sync defaultJenkinsUser j h templateName o owners everyone projects s) >>= syncReport
  dispatch anatomy >>= \sc ->
    case sc of
      VersionCommand ->
        (putStrLn $ "anatomy: " <> buildInfoVersion) >> exitSuccess
      ReportCommand ->
        orDie renderReportError $
          report o projects >>= mapM_ (\r ->
            liftIO $ case (reportProject r, reportGithub r) of
              (Nothing, Nothing) ->
                pure ()
              (Just p, Just _) ->
                putStrLn $ "[OK] " <> (T.unpack . name) p
              (Just p, Nothing) ->
                putStrLn $ "[KO] no github repository found for " <> (T.unpack . name) p
              (Nothing, Just g) ->
                putStrLn $ "[KO] no anatomy metadata found for " <> repoName g)
      DiagnoseCommand ->
        sync' Diagnose
      SyncCommand ->
        sync' Sync
      SyncUpdateCommand ->
        sync' SyncUpdate

anatomy :: Parser Command
anatomy =
  versionP <|> commandP

commandP :: Parser Command
commandP =  subparser $
     command' "report"
              "Report on all projects"
              (pure ReportCommand)
  <> command' "diagnose"
              "Diagnose problems, undocumented projects."
              (pure DiagnoseCommand)
  <> command' "sync"
              "Create new projects, archive old ones (currently only creates new ones)."
              (pure SyncCommand)
  <> command' "sync-update"
              "Create new projects, update all jenkins configs."
              (pure SyncUpdateCommand)

versionP :: Parser Command
versionP =
  flag' VersionCommand $
       short 'v'
    <> long "version"
    <> help "Display the version for the anatomy executable."
