{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Anatomy.System.Sync (
    sync
  , syncReport
  ) where

import           Anatomy.Data
import           Anatomy.System.Report

import           Control.Concurrent (threadDelay)
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Control.Retry

import qualified Anatomy.Ci.GitHub as G
import qualified Anatomy.Ci.Jenkins as J
import           Anatomy.System.XmlDiff

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
  r <- bimapEitherT SyncReportError id $ report o ps
  when (m == Sync || m == SyncUpdate) .
    forM_ (syncables r) $
      create defaultJenkinsUser j h templateName o t everyone
  when (m == SyncUpdate) .
    forM_ (jenkinsable r) $ \z -> do
      liftIO $ threadDelay 200000 {-- 200 ms --}

      forM (builds z) $ \b -> EitherT . liftIO $ do
        auth <- G.auth'
        jenkinsUser <- (maybe defaultJenkinsUser T.pack) <$> getEnv "JENKINS_USER"

        currentJob <- recoverAll (limitRetries 5 <> exponentialBackoff 100000 {-- 100 ms --})
          $ getJob jenkinsUser auth b j h

        let createOrUpdate =
              recoverAll
                (limitRetries 5 <> exponentialBackoff 100000 {-- 100 ms --})
                (createOrUpdateJenkinsJob defaultJenkinsUser j h z b)
        case currentJob of
          Nothing ->
            Right <$> createOrUpdate
          Just currentJob' -> do
            let mj = genModJob j h jenkinsUser auth z b
            expectedJob <- J.generateJob mj
            case xmlDiffText currentJob' expectedJob of
              Left e ->
                pure . Left $ SyncXmlParseError b e
              Right (Right ()) ->
                pure $ Right ()
              Right (Left (XmlDiff e (n1, n2))) -> do
                putStrLn . T.unpack $ "Job '" <> buildName b <> "' has changed at " <> elementsPath e
                 <> " from " <> (T.pack . show) n1
                 <> " to " <> (T.pack . show) n2
                Right <$> createOrUpdate

  pure (syncables r)


getJob :: Text -> Text -> Build -> JenkinsUrl -> HooksUrl -> IO (Maybe Text)
getJob jenkinsUser auth b k h = do
  let j = J.Job {
      J.org = jenkinsUser
    , J.oauth = auth
    , J.jobName = buildName b
    , J.jenkinsHost = k
    , J.jenkinsHooks = h
    }
  e <- J.getJob_ j
  pure $ rightToMaybe e

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

jenkinsable :: [Report a b] -> [Project a b]
jenkinsable rs =
  rs >>= \r ->
    case r of
      Report (Just p) _ ->
        [p]
      Report _ _ ->
        []

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
        case r of
          Right _ ->
            setupCi o defaultJenkinsUser j h p
          Left _ ->
            pure ()
        return r
      where
        addExtraTeam auth (Right _) =
          void $ GO.addTeamToRepo auth teamId (T.unpack $ orgName o) (T.unpack $ name p)
        addExtraTeam _ _ =
          return ()
        teamId =
          teamGithubId everyone

setupCi :: Org -> Text -> JenkinsUrl -> HooksUrl -> Project a b -> IO ()
setupCi o defaultJenkinsUser j h p = do
    auth <- G.auth
    G.hook h auth (T.unpack $ orgName o) (T.unpack $ name p)
    forM_ (builds p) $
      createOrUpdateJenkinsJob defaultJenkinsUser j h p

createOrUpdateJenkinsJob :: Text -> JenkinsUrl -> HooksUrl -> Project a b -> Build -> IO ()
createOrUpdateJenkinsJob defaultJenkinsUser j h p b = do
  auth <- J.jauth
  jenkinsUser <- (maybe defaultJenkinsUser T.pack) <$> getEnv "JENKINS_USER"
  J.createOrUpdateJob $ genModJob j h jenkinsUser auth p b

genModJob :: JenkinsUrl -> HooksUrl -> Text -> Text -> Project a b -> Build -> J.ModJob
genModJob j h jenkinsUser auth p b =
  J.ModJob {
    J.jobreq = J.Job {
         J.org = jenkinsUser
       , J.oauth = auth
       , J.jobName = buildName b
       , J.jenkinsHost = j
       , J.jenkinsHooks = h
       }
    , J.jobTemplate = template b
    , J.params = toParams p $ replacements b
    }

toParams :: Project a b -> [Replace] -> Text -> Maybe Text
toParams p rs s = case s of
  "project" ->
    Just $ name p
  _ ->
    fmap replaceValue . flip P.find rs $ \r ->
      replaceKey r == s

applyTemplate :: Either Error Repo -> Project a b -> Maybe GithubTemplate -> IO ()
applyTemplate (Right _) p (Just tmpName) =
  pushTemplate p tmpName
applyTemplate _ _ _ = return ()

pushTemplate :: Project a b -> GithubTemplate -> IO ()
pushTemplate p t =
  void . withSystemTempDirectory "template_repo" $ \dir ->
    system $ P.intercalate " " ["./bin/clone_template", dir, T.unpack . githubTemplate $ t, T.unpack $ name p]
