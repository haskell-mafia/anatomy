{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Main where

import           Anatomy

import           BuildInfo_ambiata_anatomy

import           Control.Monad.IO.Class

import           Data.Text as T

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
  deriving (Eq, Show)

anatomyMain
  :: (a -> Maybe GithubTemplate) -- ^ a function that takes the class of a project and decides which template to apply, if any at all...
  -> T.Text     -- ^ Default Jenkins user, overridden by the JENKINS_USER environment variable
  -> Org        -- ^ GitHub organisation
  -> Team       -- ^ A team of people to add as Collaborators on the new repo.
  -> Team       -- ^ A team of everyone in your organisation
  -> [Project a b]
  -> IO ()
anatomyMain templateName defaultJenkinsUser org owners everyone projects = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch anatomy >>= \sc ->
    case sc of
      VersionCommand ->
        (putStrLn $ "anatomy: " <> buildInfoVersion) >> exitSuccess
      ReportCommand ->
        orDie renderReportError $
          report org projects >>= mapM_ (\r ->
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
        (orDie renderSyncError $ sync defaultJenkinsUser templateName org owners everyone projects Diagnose) >>= syncReport
      SyncCommand ->
        (orDie renderSyncError $ sync defaultJenkinsUser templateName org owners everyone projects Sync) >>= syncReport

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

versionP :: Parser Command
versionP =
  flag' VersionCommand $
       short 'v'
    <> long "version"
    <> help "Display the version for the anatomy executable."
