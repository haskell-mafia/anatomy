{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Anatomy.Ci.Jenkins (
    Job (..)
  , ModJob (..)
  , jauth
  , getJob
  , getJob_
  , createJob
  , updateJob
  , renderJob
  , generateJob
  , createOrUpdateJob
  , isJobRunning
  ) where

import           Anatomy.Data
import qualified Anatomy.Ci.GitHub as G

import qualified Data.ByteString as B hiding (unpack, pack)
import qualified Data.ByteString.Lazy as BL hiding (unpack, pack)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Template (substituteA)

import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Env


data Job = Job {
    org :: Text
  , oauth :: Text
  , jobName :: Text
  , jenkinsHost :: JenkinsUrl
  , jenkinsHooks :: HooksUrl
  }

data ModJob = ModJob {
    jobreq :: Job
  , jobTemplate :: BuildTemplate
  , params :: Text -> Maybe Text
  }

jauth :: IO Text
jauth = getEnv "JENKINS_AUTH" >>= \t -> case t of
  Nothing -> G.auth'
  Just a -> pure $ T.pack a

getJob_ :: Job -> IO (Either (Int, Text) Text)
getJob_ job = do
  res <- https (T.pack $ (T.unpack . jenkinsUrl . jenkinsHost $ job) </> "job" </> T.unpack (jobName job) </> "config.xml") (org job) (oauth job) rGet
  let body = responseBody res
      b = T.decodeUtf8 . BL.toStrict $ body
  return $ case (statusCode . responseStatus) res of
    200 ->
      Right b
    n   ->
      Left (n, b)

getJob :: Job -> IO ()
getJob job =
   getJob_ job >>= \x ->
     case x of
       Right resp ->
         putStrLn . T.unpack $ resp
       Left (n, resp) -> do
         putStrLn $ "Couldn't fetch job [" <> show n <> "]"
         putStrLn . T.unpack $ resp
         exitFailure

createJob :: ModJob -> IO ()
createJob =
  modifyJob ("/createItem?name=" <>) $ \x -> case x of
    Right job -> "Created job [" <> job <> "]"
    Left n -> "Couldn't create job [" <> n <> "]"

updateJob :: ModJob -> IO ()
updateJob =
  modifyJob (\job -> T.pack $ "/job" </> T.unpack job </> "config.xml") $ \x -> case x of
    Right job -> "Updated job [" <> job <> "]"
    Left n -> "Couldn't update job [" <> n <> "]"

modifyJob :: (Text -> Text) -> (Either Text Text -> Text) -> ModJob -> IO ()
modifyJob url respHandler modjob = do
  let job = jobreq modjob
      baseUrl = jenkinsUrl . jenkinsHost $ job

  body <- mkBody (params modjob) (jobTemplate modjob)

  https ((<>) baseUrl . url . jobName $ job) (org job) (oauth job) (rPost body . rXml) >>= \res ->
    case (statusCode . responseStatus) res of
      200 ->
        T.putStrLn . respHandler . Right . jobName $ job
      n -> do
        T.putStrLn . respHandler . Left . T.pack . show $ n
        putStrLn . show . responseBody $ res
        exitFailure

renderJob :: ModJob -> IO ()
renderJob modjob =
  generateJob modjob >>=
    T.putStrLn

generateJob :: ModJob -> IO Text
generateJob modjob = do
  res <- mkBody (params modjob) (jobTemplate modjob)
  pure . T.decodeUtf8 $ res

createOrUpdateJob :: ModJob -> IO ()
createOrUpdateJob modjob = do
  getJob_ (jobreq modjob) >>= \x -> case x of
    Left _  -> createJob modjob
    Right _ ->
      -- Unfortunately there are race conditions in jenkins when updating a running job so we're a little careful here
      isJobRunning (jobreq modjob) >>= \case
        Left l -> do
          T.putStrLn $ "Error fetching job status " <> l
          exitFailure
        Right True ->
          T.putStrLn $ "Could not update " <> (jobName . jobreq) modjob <> " as it is currently running"
        Right False ->
          updateJob modjob

isJobRunning :: Job -> IO (Either Text Bool)
isJobRunning job = do
  res <- https (T.pack $ (T.unpack . jenkinsUrl . jenkinsHost $ job) </> "job" </> T.unpack (jobName job) </> "lastBuild/api/json?tree=result") (org job) (oauth job) rGet
  return $ case (statusCode . responseStatus) res of
    200 ->
      first T.pack . fmap (\(A.Object o) -> HM.lookup "result" o == Just A.Null) . A.eitherDecode $ responseBody res
    404 ->
      Left $ "No builds could be found for [" <> jobName job <> "]"
    n ->
      Left $ "Invalid status " <> (T.pack . show) n

https :: Text ->  Text -> Text -> (Request -> Request) -> IO (Response BL.ByteString)
https url user password xform =
  parseUrl (T.unpack url) >>= \req ->
    withManager (mkManagerSettings (TLSSettingsSimple True False True) Nothing) . httpLbs $
      (applyBasicAuth (T.encodeUtf8 user) (T.encodeUtf8 password) $ xform (req {
          checkStatus = const . const . const $ Nothing
        }))

rGet :: Request -> Request
rGet req =
  req { method = "GET" }

rPost :: B.ByteString -> Request -> Request
rPost body req =
  req { method = "POST", requestBody = RequestBodyBS body }

rXml :: Request -> Request
rXml req =
  req { requestHeaders = ("Content-Type", "text/xml") : requestHeaders req }

mkBody :: (Text -> Maybe Text) -> BuildTemplate -> IO B.ByteString
mkBody props b =
  T.encodeUtf8 . TL.toStrict <$> substituteA (buildTemplate b) (mkContext props)

mkContext :: (Text -> Maybe Text) -> Text -> IO Text
mkContext props x =
  case props x of
    Just y ->
      pure y
    Nothing ->
      case T.splitAt 4 x of
        ("env_", e) ->
          getEnv (T.unpack e) >>= maybe (fail $ "Invalid environment variable: " <> show x) (pure . T.pack)
        _ ->
          fail "Environment variables must be prefixed with `env_` in the template"
