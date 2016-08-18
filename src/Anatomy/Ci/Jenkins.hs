{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Anatomy.Ci.Jenkins (
    JobName (..)
  , ModJob (..)
  , getJob
  , putStrJob
  , createJob
  , updateJob
  , renderJob
  , generateJob
  , createOrUpdateJob
  , isJobRunning
  ) where

import           Anatomy.Data

import qualified Data.ByteString as B hiding (unpack, pack)
import qualified Data.ByteString.Lazy as BL hiding (unpack, pack)
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
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


newtype JobName =
  JobName {
      jobName :: Text
    } deriving (Eq, Show)

data ModJob = ModJob {
    modName :: JobName
  , jobTemplate :: BuildTemplate
  , params :: Text -> Maybe Text
  }

getJob :: JenkinsConfiguration -> JobName -> IO (Either (Int, Text) Text)
getJob conf job = do
  res <- https (T.pack $ (T.unpack . jenkinsUrl . jenkinsHost $ conf) </> "job" </> T.unpack (jobName job) </> "config.xml") (jenkinsUser conf) (jenkinsOAuth conf) rGet
  let body = responseBody res
      b = T.decodeUtf8 . BL.toStrict $ body
  return $ case (statusCode . responseStatus) res of
    200 ->
      Right b
    n   ->
      Left (n, b)

putStrJob :: JenkinsConfiguration -> JobName -> IO ()
putStrJob conf jn =
   getJob conf jn >>= \x ->
     case x of
       Right resp ->
         putStrLn . T.unpack $ resp
       Left (n, resp) -> do
         putStrLn $ "Couldn't fetch job [" <> show n <> "]"
         putStrLn . T.unpack $ resp
         exitFailure

createJob :: JenkinsConfiguration -> ModJob -> IO ()
createJob conf =
  modifyJob conf ("/createItem?name=" <>) $ \x ->
    case x of
      Right job ->
        "Created job [" <> job <> "]"
      Left n ->
        "Couldn't create job [" <> n <> "]"

updateJob :: JenkinsConfiguration -> ModJob -> IO ()
updateJob conf =
  modifyJob conf (\job -> T.pack $ "/job" </> T.unpack job </> "config.xml") $ \x ->
    case x of
      Right job ->
        "Updated job [" <> job <> "]"
      Left n ->
        "Couldn't update job [" <> n <> "]"

modifyJob :: JenkinsConfiguration -> (Text -> Text) -> (Either Text Text -> Text) -> ModJob -> IO ()
modifyJob conf url respHandler modjob = do
  let job = modName modjob
      baseUrl = jenkinsUrl . jenkinsHost $ conf

  body <- mkBody (params modjob) (jobTemplate modjob)

  https ((<>) baseUrl . url . jobName $ job) (jenkinsUser conf) (jenkinsOAuth conf) (rPost body . rXml) >>= \res ->
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

createOrUpdateJob :: JenkinsConfiguration -> ModJob -> IO ()
createOrUpdateJob conf modjob = do
  getJob conf (modName modjob) >>= \x ->
    case x of
      Left _ ->
        createJob conf modjob
      Right _ ->
        -- Unfortunately there are race conditions in jenkins when updating a running job so we're a little careful here
        isJobRunning conf (modName modjob) >>= \case
          Left l -> do
            T.putStrLn $ "Error fetching job status " <> l
            exitFailure
          Right True ->
            T.putStrLn $ "Could not update " <> (jobName . modName $ modjob) <> " as it is currently running"
          Right False ->
            updateJob conf modjob

isJobRunning :: JenkinsConfiguration -> JobName -> IO (Either Text Bool)
isJobRunning conf n = do
  res <- https
    ((jenkinsUrl . jenkinsHost $ conf) <> "/job/" <> jobName n <> "/lastBuild/api/json?tree=result")
    (jenkinsUser conf)
    (jenkinsOAuth conf)
    rGet
  return $ case (statusCode . responseStatus) res of
    200 ->
      first T.pack . fmap (\(A.Object o) -> HM.lookup "result" o == Just A.Null) . A.eitherDecode $ responseBody res
    404 ->
      Right False
    i ->
      Left $ "Invalid status " <> (T.pack . show) i

https :: Text -> JenkinsUser -> JenkinsAuth -> (Request -> Request) -> IO (Response BL.ByteString)
https url usr auth xform =
  parseUrlThrow (T.unpack url) >>= \req -> do
    m <- newManager (mkManagerSettings (TLSSettingsSimple True False True) Nothing)
    flip httpLbs m $
      (applyBasicAuth (T.encodeUtf8 . renderUser $ usr) (T.encodeUtf8 . jenkinsAuth $ auth) $ xform (req {
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
