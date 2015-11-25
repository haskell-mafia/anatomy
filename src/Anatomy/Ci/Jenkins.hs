{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Anatomy.Ci.Jenkins (
    Job (..)
  , ModJob (..)
  , JenkinsUrl (..)
  , HooksUrl (..)
  , getJob
  , createJob
  , updateJob
  , generateJob
  , createOrUpdateJob
  ) where

import qualified Data.ByteString as B hiding (unpack, pack)
import qualified Data.ByteString.Char8 as B (unpack, pack)
import qualified Data.ByteString.Lazy as BL hiding (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Template
import           Data.String

import           Network.Connection
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

import           P

import           System.Exit
import           System.FilePath ((</>))
import           System.IO
import           System.Posix.Env

newtype JenkinsUrl =
  JenkinsUrl {
      jenkinsUrl :: String
    } deriving (Eq, Show)

newtype HooksUrl =
  HooksUrl {
      hooksUrl :: String
    } deriving (Eq, Show)

data Job = Job {
    org    :: String
  , oauth   :: String
  , jobName :: String
  , jenkinsHost :: JenkinsUrl
  , jenkinsHooks :: HooksUrl
  }

data ModJob = ModJob {
    jobreq       :: Job
  , templatefile :: String
  , params       :: String -> Maybe String
  }

getJob_ :: Job -> IO (Either (Int, String) String)
getJob_ job = do
  res <- https ((jenkinsUrl . jenkinsHost $ job) </> "job" </> jobName job </> "config.xml") (org job) (oauth job) rGet
  let body = responseBody res
  return $ case (statusCode . responseStatus) res of
    200 -> Right . BL.unpack $ body
    n   -> Left (n, show body)

getJob :: Job -> IO ()
getJob job =
   getJob_ job >>= \x -> case x of
       Right resp     -> putStrLn resp
       Left (n, resp) -> do
         putStrLn $ "Couldn't fetch job [" <> show n <> "]"
         putStrLn resp
         exitFailure

createJob :: ModJob -> IO ()
createJob =
  modifyJob ("/createItem?name=" <>) $ \x -> case x of
    Right job -> "Created job [" <> job <> "]"
    Left n -> "Couldn't create job [" <> n <> "]"

updateJob :: ModJob -> IO ()
updateJob =
  modifyJob (\job -> "/job" </> job </> "config.xml") $ \x -> case x of
    Right job -> "Updated job [" <> job <> "]"
    Left n -> "Couldn't update job [" <> n <> "]"

modifyJob :: (String -> String) -> (Either String String -> String) -> ModJob -> IO ()
modifyJob url respHandler modjob = do
  let job = jobreq modjob
  body <- mkBody (params modjob) (templatefile modjob)
  let baseUrl = jenkinsUrl . jenkinsHost $ job
  https ((<>) baseUrl . url . jobName $ job) (org job) (oauth job) (rPost body . rXml) >>= \res ->
    case (statusCode . responseStatus) res of
      200 -> putStrLn . respHandler . Right . jobName $ job
      n   -> do putStrLn $ respHandler . Left . show $ n
                putStrLn . show . responseBody $ res
                exitFailure

generateJob :: ModJob -> IO ()
generateJob modjob = do
  res <- mkBody (params modjob) (templatefile modjob)
  putStrLn . B.unpack $ res

createOrUpdateJob :: ModJob -> IO ()
createOrUpdateJob modjob = do
  getJob_ (jobreq modjob) >>= \x -> case x of
    Left _  -> createJob modjob
    Right _ -> updateJob modjob

https :: String ->  String -> String -> (Request -> Request) -> IO (Response BL.ByteString)
https url user password xform =
  parseUrl url >>= \req -> withManager (mkManagerSettings (TLSSettingsSimple True False True) Nothing) . httpLbs $
    (applyBasicAuth (B.pack user) (B.pack password) $ xform (req { checkStatus = const . const . const $ Nothing }))

rGet :: Request -> Request
rGet req =
  req { method = "GET" }

rPost :: B.ByteString -> Request -> Request
rPost body req =
  req { method = "POST", requestBody = RequestBodyBS body }

rXml :: Request -> Request
rXml req =
  req { requestHeaders = ("Content-Type", "text/xml") : requestHeaders req }

mkBody :: (String -> Maybe String) -> String -> IO B.ByteString
mkBody props file = do
  t <- T.readFile file
  T.encodeUtf8 . TL.toStrict  <$> substituteA t (mkContext props)


mkContext :: (String -> Maybe String) -> Text -> IO Text
mkContext props x =
    case (props . T.unpack $ x) of
      Just y -> pure . T.pack $ y
      Nothing -> case T.splitAt 4 x of
           ("env_", e) ->
             getEnv (T.unpack e) >>= maybe (fail $ "Invalid environment variable: " <> show x) (pure . T.pack)
           _           ->
             fail "Environment variables must be prefixed with `env_` in the template"
