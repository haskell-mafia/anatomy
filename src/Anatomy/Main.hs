{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Anatomy.Main where

import           Anatomy

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either

import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Github.Data

import           Options.Applicative

import           P

import           System.IO
import           System.Exit
import           System.Posix.Env (getEnv)

import           X.Options.Applicative hiding (VersionCommand)


data Command =
    VersionCommand
  | ReportCommand
  | SyncCommand
  | SyncNewCommand
  | RefreshGithubCommand
  | RefreshJenkinsCommand
  deriving (Eq, Show)

anatomyMain ::
     String
  -> (a -> Maybe GithubTemplate) -- ^ a function that takes the class of a project and decides which template to apply, if any at all...
  -> Org        -- ^ GitHub organisation
  -> Team       -- ^ A team of people to add as Collaborators on the new repo.
  -> Team       -- ^ A team of everyone in your organisation
  -> [Project a b]
  -> IO ()
anatomyMain buildInfoVersion templates org owners everyone projects = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  github' <- env "GITHUB_OAUTH"
  let github = GithubOAuth . T.unpack $ github'
  jenkins <- JenkinsAuth <$> (getEnv "JENKINS_AUTH" >>=  pure . maybe github' T.pack)
  user <- JenkinsUser <$> env "JENKINS_USER"
  host <- JenkinsUrl <$> env "JENKINS_HOST"
  hookz <- HooksUrl <$> env "JENKINS_HOOKS"

  let jconf = JenkinsConfiguration user jenkins host

  token <- HipchatToken <$> env "HIPCHAT_TOKEN"
  room <- (HipchatRoom . T.pack) <$> (fromMaybe "eng" <$> getEnv "HIPCHAT_ROOM_GITHUB")


--        (orDie renderSyncError $ sync user  j h templateName o owners everyone projects s) >>= syncReport
  dispatch anatomy >>= \sc ->
    case sc of
      VersionCommand ->
        (putStrLn $ "anatomy: " <> buildInfoVersion) >> exitSuccess

      -- not done
      ReportCommand ->
        orDie renderReportError $
          report github org projects >>= mapM_ (\r ->
            liftIO $ case (reportProject r, reportGithub r) of
              (Nothing, Nothing) ->
                pure ()
              (Just p, Just _) ->
                putStrLn $ "[OK] " <> (T.unpack . renderName . name) p
              (Just p, Nothing) ->
                putStrLn $ "[KO] no github repository found for " <> (T.unpack . renderName . name) p
              (Nothing, Just g) ->
                putStrLn $ "[KO] no anatomy metadata found for " <> repoName g)
--      DiagnoseCommand -> orDie renderReportError $ do -- FIX
--        r <- report github org projects
--        forM_ (syncables r) $
--          lift . T.putStrLn . renderProjectReport

      -- not done
      SyncNewCommand -> orDie renderSyncError $ do -- FIX
        -- FIX - add env var
        -- basically the implementation for `--only-new`
        r <- bimapEitherT SyncReportError id $ report github org projects
        let n = newprojects r
        bimapEitherT SyncGithubError id $ syncRepositories github templates org owners everyone n
        bimapEitherT SyncCreateError id $ syncHooks github token room org hookz n
        bimapEitherT SyncBuildError id $ syncBuilds jconf n

      -- not done
      -- FIX - add env var
      SyncCommand -> orDie renderSyncError $ do
        r <- bimapEitherT SyncReportError id $ report github org projects
        bimapEitherT SyncGithubError id . syncRepositories github templates org owners everyone $ newprojects r
        -- Assume all projects exist now
        bimapEitherT SyncCreateError id $ syncHooks github token room org hookz projects
        bimapEitherT SyncBuildError id $ syncBuilds jconf projects

      -- partial done
      RefreshGithubCommand -> orDie renderSyncError $ do
        github <- (GithubOAuth . T.unpack) <$> env "GITHUB_OAUTH"
        hookz <- HooksUrl <$> env "JENKINS_HOOKS"
        token <- HipchatToken <$> env "HIPCHAT_TOKEN"
        room <- (HipchatRoom . T.pack) <$> (fromMaybe "eng" <$> liftIO (getEnv "HIPCHAT_ROOM_GITHUB"))

        -- FIX investigate if this report is actually required
        r <- bimapEitherT SyncReportError id $ report github org projects
        bimapEitherT SyncCreateError id . syncHooks github token room org hookz $ hookable r

      -- done
      RefreshJenkinsCommand -> orDie renderBuildError $ do
        conf <- getJenkinsConfiguration
        syncBuilds conf projects

getJenkinsConfiguration :: MonadIO m => m JenkinsConfiguration
getJenkinsConfiguration = liftIO $ do
  jenkins <- JenkinsAuth <$> env "JENKINS_AUTH"
  user <- JenkinsUser <$> env "JENKINS_USER"
  host <- JenkinsUrl <$> env "JENKINS_HOST"
  pure $ JenkinsConfiguration user jenkins host


env :: MonadIO m => Text -> m Text
env var =
  liftIO $ getEnv (T.unpack var) >>= \t ->
    case t of
      Nothing ->
        T.hPutStrLn stderr ("Need to specify $" <> var) >>
          exitFailure
      Just a ->
        pure $ T.pack a

anatomy :: Parser Command
anatomy =
  versionP <|> commandP

{--


  anatomy report

  anatomy sync

  anatomy sync --only-new

  anatomy refresh-github-hooks

  anatomy refresh-jenkins-jobs

--}

commandP :: Parser Command
commandP =  subparser $
     command' "report"
              "Report on all projects, highlighting undocumented ones."
              (pure ReportCommand)
  <> command' "sync-new"
              "Create repositories, hooks and jobs for new projects ."
              (pure SyncNewCommand)
  <> command' "sync"
              "Create new projects, update all hooks and jenkins configs."
              (pure SyncCommand)
  <> command' "refresh-github-hooks"
              "Update all github hooks."
              (pure RefreshGithubCommand)
  <> command' "refresh-jenkins-jobs"
              "Update all jenkins configs."
              (pure RefreshJenkinsCommand)

versionP :: Parser Command
versionP =
  flag' VersionCommand $
       short 'v'
    <> long "version"
    <> help "Display the version for the anatomy executable."
